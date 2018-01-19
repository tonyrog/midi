//
// Midi
//
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>

#include "erl_nif.h"
#include "erl_driver.h"
#include "midi.h"

#define DEBUG

#define UNUSED(a) ((void) a)

#ifdef DEBUG
#include <stdio.h>
#define DBG(...) printf(__VA_ARGS__)
#define BADARG(env) printf("matrix_nif.c: badarg line=%d\r\n", __LINE__), enif_make_badarg((env))
#else
#define DBG(...)
#define BADARG(env) enif_make_badarg((env))
#endif


#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)		\
    atm_##name = enif_make_atom(env,string)

static int midi_load(ErlNifEnv* env, void** priv_data,
		       ERL_NIF_TERM load_info);
static int midi_upgrade(ErlNifEnv* env, void** priv_data,
			  void** old_priv_data,
		       ERL_NIF_TERM load_info);
static void midi_unload(ErlNifEnv* env, void* priv_data);

static ERL_NIF_TERM midi_open(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM midi_close(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM midi_write(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM midi_read(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[]);

#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 12))
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
//#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#elif (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif

static ErlNifResourceType* midi_r;

#define STATE_STATUS      0
#define STATE_PARAMS      1
#define STATE_PARAMS_0    2
#define STATE_PARAMS_1    3
#define STATE_PARAMS_2    4
#define STATE_PARAMS_F7   5
#define STATE_PARAMS_META 6
#define STATE_PARAMS_V0   7
#define STATE_PARAMS_V1   8
#define STATE_PARAMS_V2   9
#define STATE_PARAMS_V3   10
#define STATE_PARAMS_VL   11

#define MAX_MIDI_BUF 1024

typedef struct _midi_dev_t {
    int     fd;
    int     state;
    uint8_t params[MAX_MIDI_BUF];
    int     pos;
    int     len;
    int     status_in;
    int     running_in;
    int     status_out;    
    int     raw_mode;
    int     bin_mode;
    int     running_mode;
} midi_dev_t;


ErlNifFunc midi_funcs[] =
{
    NIF_FUNC("open",        2, midi_open),
    NIF_FUNC("close_",       1, midi_close),
    NIF_FUNC("write_",       2, midi_write),
    NIF_FUNC("read_",        1, midi_read),
};

DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(undefined);
DECL_ATOM(select);
// events
DECL_ATOM(midi);
DECL_ATOM(note_off);
DECL_ATOM(note_on);
DECL_ATOM(after_touch);
DECL_ATOM(control_change);
DECL_ATOM(pitch_bend);
DECL_ATOM(program_change);
DECL_ATOM(pressure);
DECL_ATOM(sys);
// open modes
DECL_ATOM(raw);
DECL_ATOM(event);
DECL_ATOM(binary);
DECL_ATOM(list);
DECL_ATOM(running);


static ERL_NIF_TERM make_error(ErlNifEnv* env, int e)
{
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(e)));
}

static void midi_dtor(ErlNifEnv* env, midi_dev_t* dp)
{
    UNUSED(env);

    DBG("midi_dtor: close %d\r\n", dp->fd);
    if (dp->fd >= 0) {
	close(dp->fd);
    }
    dp->fd = -1;
}


static void midi_stop(ErlNifEnv* env, midi_dev_t* dp,
		      ErlNifEvent event, int is_direct_call)
{
    UNUSED(env);
    DBG("midi_stop: event=%d\r\n", (int)event);
    if ((int)event >= 0) {
	close((int)event);
	if (dp->fd == (int)event)
	    dp->fd = -1;
    }
}

static void midi_down(ErlNifEnv* env, midi_dev_t* dp,
		      const ErlNifPid* pid, const ErlNifMonitor* mon)
{
    UNUSED(env);    
    DBG("midi_down: %d\r\n", dp->fd);    
}


static void midi_scan_init(midi_dev_t* dp)
{
    dp->state = STATE_STATUS;
    dp->pos = 0;
    dp->running_in = 0;
    dp->status_in  = 0;
    dp->len = 0;
}

static int midi_scan(ErlNifEnv* env, midi_dev_t* dp, uint8_t* ptr, size_t len)
{
    ERL_NIF_TERM event;
    ERL_NIF_TERM msg;
    ErlNifPid pid;
    int count = 0;
    int state = dp->state;
again:
    while(len) {
	switch(state) {
	case STATE_STATUS:
	    state = STATE_PARAMS;
	    dp->pos = 0;
	    if ((ptr[0] & 0x80) == 0) {
		dp->status_in = dp->running_in;
	    }
	    else {
		dp->status_in = ptr[0];
		if ((dp->status_in & 0xf0) == 0xf0) {
		    if (dp->status_in <= 0xf7) dp->running_in = 0;
		    // 0xf8...0xff do not affect running
		}
		else
		    dp->running_in = dp->status_in;
		ptr++;
		len--;
	    }
	    break;
	case STATE_PARAMS:
	    switch(dp->status_in & 0xf0) {
	    case MIDI_EVENT_NOTEOFF: state = STATE_PARAMS_2; break;
	    case MIDI_EVENT_NOTEON: state = STATE_PARAMS_2; break;
	    case MIDI_EVENT_AFTERTOUCH:	state = STATE_PARAMS_2; break;
	    case MIDI_EVENT_CONTROLCHANGE: state = STATE_PARAMS_2; break;
	    case MIDI_EVENT_PITCHBEND: state = STATE_PARAMS_2; break;
	    case MIDI_EVENT_PROGRAMCHANGE: state = STATE_PARAMS_1; break;
	    case MIDI_EVENT_PRESSURE: state = STATE_PARAMS_1; break;
	    case MIDI_EVENT_SYS:
		switch(dp->status_in & 0x0f) {
		case 0: state = STATE_PARAMS_F7; break;
		case 1: state = STATE_PARAMS_0; break;
		case 2: state = STATE_PARAMS_2; break;
		case 3: state = STATE_PARAMS_1; break;
		case 4: state = STATE_PARAMS_0; break;
		case 5: state = STATE_PARAMS_0; break;
		case 6: state = STATE_PARAMS_0; break;
		case 7: state = STATE_PARAMS_0; break;
		case 8: state = STATE_PARAMS_0; break;
		case 9: state = STATE_PARAMS_0; break;
		case 10: state = STATE_PARAMS_0; break;
		case 11: state = STATE_PARAMS_0; break;
		case 12: state = STATE_PARAMS_0; break;
		case 13: state = STATE_PARAMS_0; break;
		case 14: state = STATE_PARAMS_0; break;
		case 15: state = STATE_PARAMS_META; break;
		}
		break;
	    }
	    break;
	case STATE_PARAMS_0:
	    dp->len = 0;
	    state = STATE_PARAMS_VL;
	    break;
	case STATE_PARAMS_1:
	    dp->len = 1;
	    state = STATE_PARAMS_VL;
	    break;
	case STATE_PARAMS_2:
	    dp->len = 2;
	    state = STATE_PARAMS_VL;
	    break;
	case STATE_PARAMS_F7:
	    while(len && (ptr[0] != 0xf7)) {
		if (dp->pos < MAX_MIDI_BUF) {
		    dp->params[dp->pos++] = ptr[0];
		}
		ptr++;
		len --;
	    }
	    if (ptr[0] == 0xf7)
		goto done;
	    break;
	case STATE_PARAMS_META:
	    if (dp->pos < MAX_MIDI_BUF)
		dp->params[dp->pos++] = ptr[0]; // meta
	    ptr++;
	    len--;
	    state = STATE_PARAMS_V0;
	    break;
	case STATE_PARAMS_V0:
	    dp->len = (ptr[0] & 0x7f);
	    if ((ptr[0] & 0x80) == 0x00)
		state = STATE_PARAMS_VL;
	    else
		state = STATE_PARAMS_V1;
	    ptr++;
	    len--;
	    break;
	case STATE_PARAMS_V1:
	    dp->len = (dp->len << 7) | (ptr[0] & 0x7f);
	    if ((ptr[0] & 0x80) == 0x00)
		state = STATE_PARAMS_VL;
	    else
		state = STATE_PARAMS_V1;
	    ptr++;
	    len--;
	    break;
	case STATE_PARAMS_V2:
	    dp->len = (dp->len << 7) | (ptr[0] & 0x7f);
	    if ((ptr[0] & 0x80) == 0x00)
		state = STATE_PARAMS_VL;
	    else
		state = STATE_PARAMS_V3;
	    ptr++;
	    len--;
	    break;
	case STATE_PARAMS_V3:
	    dp->len = (dp->len << 7) | (ptr[0] & 0x7f);
	    state = STATE_PARAMS_VL;	    
	    ptr++;
	    len--;
	    break;
	case STATE_PARAMS_VL:
	    if (dp->len > 0) {
		if (dp->pos < MAX_MIDI_BUF)
		    dp->params[dp->pos++] = ptr[0];
		ptr++;
		len--;
		dp->len--;
	    }
	    if (dp->len == 0)
		goto done;
	    break;
	}
    }
    dp->state = state;
    return count;

done:
    // emit event
    switch(dp->status_in & 0xf0) {
    case MIDI_EVENT_NOTEOFF:
	event = enif_make_tuple4(env, ATOM(note_off),
				 enif_make_int(env, dp->state & 0x0f),
				 enif_make_int(env, dp->params[0]),
				 enif_make_int(env, dp->params[1]));
	break;
    case MIDI_EVENT_NOTEON:
	event = enif_make_tuple4(env, ATOM(note_on),
				 enif_make_int(env, dp->state & 0x0f),
				 enif_make_int(env, dp->params[0]),
				 enif_make_int(env, dp->params[1]));
	break;
    case MIDI_EVENT_AFTERTOUCH:
	event = enif_make_tuple4(env, ATOM(after_touch),
				 enif_make_int(env, dp->state & 0x0f),
				 enif_make_int(env, dp->params[0]),
				 enif_make_int(env, dp->params[1]));
	break;	
    case MIDI_EVENT_CONTROLCHANGE:
	event = enif_make_tuple4(env, ATOM(control_change),
				 enif_make_int(env, dp->state & 0x0f),
				 enif_make_int(env, dp->params[0]),
				 enif_make_int(env, dp->params[1]));
	break;
    case MIDI_EVENT_PITCHBEND:
	event = enif_make_tuple4(env, ATOM(pitch_bend),
				 enif_make_int(env, dp->state & 0x0f),
				 enif_make_int(env, dp->params[0]),
				 enif_make_int(env, dp->params[1]));
	break;
    case MIDI_EVENT_PROGRAMCHANGE:
	event = enif_make_tuple3(env, ATOM(program_change),
				 enif_make_int(env, dp->state & 0x0f),
				 enif_make_int(env, dp->params[0]));
	break;
    case MIDI_EVENT_PRESSURE:
	event = enif_make_tuple3(env, ATOM(pressure),
				 enif_make_int(env, dp->state & 0x0f),
				 enif_make_int(env, dp->params[0]));
	break;
    case MIDI_EVENT_SYS: {
	ERL_NIF_TERM data;
	if (dp->bin_mode) {
	    uint8_t* ptr = enif_make_new_binary(env, dp->pos, &data);
	    memcpy(ptr, dp->params, dp->pos);
	}
	else {
	    data = enif_make_string_len(env,(char*)dp->params,dp->pos,
					ERL_NIF_LATIN1);
	}
	event = enif_make_tuple3(env, ATOM(sys),
				 enif_make_int(env, dp->state & 0x0f),
				 data);
	break;
    }
    default:
	event = ATOM(undefined);
	break;
    }
    // send as {midi, Fd, Event}
    msg = enif_make_tuple3(env, ATOM(midi),
			   enif_make_resource(env, dp),
			   event);
    
    enif_send(env, enif_self(env, &pid), NULL, msg);

    
    state = STATE_STATUS;
    dp->pos = 0;
    dp->len = 0;
    count++;
    goto again;
}

// open(Device::string(),[raw,binary]) -> Synth::integer.
static ERL_NIF_TERM midi_open(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    int fd;
    int flags;
    midi_dev_t* dp;
    char devicename[1024];
    ERL_NIF_TERM dev;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;
    int raw_mode = 0;
    int bin_mode = 1;
    int running_mode = 0;
    
    if (!enif_get_string(env, argv[0], devicename,
			 sizeof(devicename), ERL_NIF_LATIN1))
	return enif_make_badarg(env);

    list = argv[1];
    while (enif_get_list_cell(env, list, &head, &tail)) {    
	if (head == ATOM(raw)) raw_mode = 1;
	else if (head == ATOM(event)) raw_mode = 0;
	else if (head == ATOM(binary)) bin_mode = 1;
	else if (head == ATOM(list)) bin_mode = 0;
	else if (head == ATOM(running)) running_mode = 1;
	else return enif_make_badarg(env);
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);
	
    if ((fd = open(devicename, O_RDWR)) < 0)
	return make_error(env, errno);
    flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, flags|O_NONBLOCK);
    
    if ((dp = enif_alloc_resource(midi_r, sizeof(midi_dev_t))) == NULL) {
	int e = errno;
	close(fd);
	return make_error(env, e);
    }
    dp->fd = fd;
    dp->raw_mode = raw_mode;
    dp->bin_mode = bin_mode;
    dp->running_mode = running_mode;
    dp->status_out = 0;
    midi_scan_init(dp);
    dev = enif_make_resource(env, dp);
    enif_release_resource(dp);
    return enif_make_tuple2(env, ATOM(ok), dev);
}

// close(Synth::integer()) -> ok
static ERL_NIF_TERM midi_close(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    midi_dev_t* dp;
    if (!enif_get_resource(env, argv[0], midi_r, (void**)&dp))
	return enif_make_badarg(env);
    DBG("midi:close: close %d\r\n", dp->fd);
    if (dp->fd >= 0) {
	ErlNifPid pid;
	enif_self(env, &pid);
	enif_select(env, (ErlNifEvent)dp->fd, ERL_NIF_SELECT_STOP,
		    dp, &pid, ATOM(undefined));
    }
    return ATOM(ok);
}

// write(Synth::integer(), Data::binary())
// if running mode then check the first byte againt
// a the status byte to check if we may skip to write it
static ERL_NIF_TERM midi_write(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    midi_dev_t* dp;
    ErlNifBinary binary;
    int n = 0;
    
    if (!enif_get_resource(env, argv[0], midi_r, (void**)&dp))
	return enif_make_badarg(env);
    if (!enif_inspect_iolist_as_binary(env,argv[1],&binary))
	return enif_make_badarg(env);
    if (dp->running_mode && (binary.size >= 2) &&
	(binary.data[0] == dp->status_out)) {
	n = write(dp->fd,binary.data+1,binary.size-1);
    }
    else if (binary.size >= 1) {
	n = write(dp->fd,binary.data,binary.size);
	if (dp->running_mode) {
	    uint8_t d = binary.data[0];
	    if (((d & 0xf0) == 0xf0) && (d <= 0xf7))
		dp->status_out = 0;
	    else
		dp->status_out = d;
	}
    }
    if (n < 0)
	return make_error(env, errno);
    return ATOM(ok);
}

static ERL_NIF_TERM midi_read(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    midi_dev_t* dp;
    unsigned char buffer[MAX_MIDI_BUF];
    ERL_NIF_TERM data;
    int n;

    if (!enif_get_resource(env, argv[0], midi_r, (void**)&dp))
	return enif_make_badarg(env);
        
    n = read(dp->fd, (char*)buffer, sizeof(buffer));
    if (n < 0) {
	if (errno == EAGAIN) {
	    ErlNifPid pid;
	    enif_self(env, &pid);
	    enif_select(env, (ErlNifEvent)dp->fd, ERL_NIF_SELECT_READ,
			dp, &pid, ATOM(undefined));
	    return ATOM(select);
	}
	return make_error(env, errno);
    }
    if (dp->raw_mode) {
	if (dp->bin_mode) {
	    uint8_t* ptr = enif_make_new_binary(env, n, &data);
	    memcpy(ptr, buffer, n);
	}
	else {
	    data = enif_make_string_len(env, (char*)buffer, n, ERL_NIF_LATIN1);
	}
	return enif_make_tuple2(env, ATOM(ok), data);
    }
    else {
	n = midi_scan(env, dp, buffer, n);
	return enif_make_tuple2(env, ATOM(ok), enif_make_int(env, n));
    }
}

static int midi_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;

    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(undefined);
    LOAD_ATOM(select);
    // evetns
    LOAD_ATOM(midi);    
    LOAD_ATOM(note_off);
    LOAD_ATOM(note_on);
    LOAD_ATOM(after_touch);
    LOAD_ATOM(control_change);
    LOAD_ATOM(pitch_bend);
    LOAD_ATOM(program_change);
    LOAD_ATOM(pressure);
    LOAD_ATOM(sys);
    // open mode
    LOAD_ATOM(raw);
    LOAD_ATOM(event);
    LOAD_ATOM(binary);
    LOAD_ATOM(list);
    LOAD_ATOM(running);
    

    cb.dtor = (ErlNifResourceDtor*) midi_dtor;
    cb.stop = (ErlNifResourceStop*) midi_stop;
    cb.down = (ErlNifResourceDown*) midi_down;
    
    midi_r = enif_open_resource_type_x(env, "midi",
				       &cb, ERL_NIF_RT_CREATE, &tried);
    *priv_data = 0;
    return 0;
}

static int midi_upgrade(ErlNifEnv* env, void** priv_data,
			void** old_priv_data,
			ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    DBG("midi_upgrade\r\n");
    *priv_data = *old_priv_data;
    return 0;
}

static void midi_unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);

    DBG("midi_unload\r\n");
}

ERL_NIF_INIT(midi, midi_funcs,
	     midi_load, NULL,
	     midi_upgrade, midi_unload)
