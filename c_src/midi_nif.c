//
// Midi
//
#include <stdio.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <poll.h>

#include "erl_nif.h"
#include "erl_driver.h"
#include "midi.h"

#define DEBUG

#define UNUSED(a) ((void) a)

#ifdef DEBUG
#define DEBUGF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#else
#define DEBUGF(f,a...)
#endif
#define ERRORF(f,a...) enif_fprintf(stderr, f "\r\n", a)
#define BADARG(env) enif_printf(stderr, "%s: badarg line=%d\r\n", __FILE__, __LINE__), enif_make_badarg((env))

#define UNUSED(a) ((void) a)

#ifdef ALSA
#include <alsa/asoundlib.h>
typedef snd_rawmidi_t* midi_handle_t;
#define INVALID ((snd_rawmidi_t*)0)
#define WRITE(handle,data,len) snd_rawmidi_write((handle),(data),(len))
#define READ(handle,data,len) snd_rawmidi_read((handle),(data),(len))
#define CLOSE(handle) snd_rawmidi_close((handle))
#define EVENT_HANDLE(handle) alsa_event_handle((handle))

static int alsa_event_handle(midi_handle_t h)
{
    int i, n;
    struct pollfd pfd[4];
    n = snd_rawmidi_poll_descriptors_count(h);
    snd_rawmidi_poll_descriptors(h, pfd, n);
    if (n == 1)
	return pfd[0].fd;
    for (i = 0; i < n; i++) {
	if (pfd[i].events & POLLIN)
	    return pfd[i].fd;
    }
    return -1;
}
#else
typedef int midi_handle_t;
#define INVALID -1
#define WRITE(handle,data,len) write((handle),(data),(len))
#define READ(handle,data,len) read((handle),(data),(len))
#define CLOSE(handle) close((handle))
#define EVENT_HANDLE(handle) (handle)
#endif


#define ATOM(name) atm_##name

#define DECL_ATOM(name) \
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)			\
    atm_##name = enif_make_atom(env,#name)

#define LOAD_ATOM_STRING(name,string)		\
    atm_##name = enif_make_atom(env,string)

static int load(ErlNifEnv* env, void** priv_data,
		       ERL_NIF_TERM load_info);
static int upgrade(ErlNifEnv* env, void** priv_data,
			  void** old_priv_data,
		       ERL_NIF_TERM load_info);
static void unload(ErlNifEnv* env, void* priv_data);

// Dirty optional since 2.7 and mandatory since 2.12
#if (ERL_NIF_MAJOR_VERSION > 2) || ((ERL_NIF_MAJOR_VERSION == 2) && (ERL_NIF_MINOR_VERSION >= 7))
#ifdef USE_DIRTY_SCHEDULER
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr),(0)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr),(ERL_NIF_DIRTY_JOB_CPU_BOUND)}
#endif
#else
#define NIF_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#define NIF_DIRTY_FUNC(name,arity,fptr) {(name),(arity),(fptr)}
#endif


#define NIF_LIST \
    NIF("open", 2,  nif_open)  \
    NIF("close_", 1, nif_close) \
    NIF("write_", 2, nif_write) \
    NIF("read_", 1,  nif_read)

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
    midi_handle_t in;
    midi_handle_t out;
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
    int     timestamp_mode;
    ErlNifTime last_time;
} midi_dev_t;


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
DECL_ATOM(timestamp);

// Declare all nif functions
#undef NIF
#ifdef NIF_TRACE
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]); \
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#else
#define NIF(name, arity, func)						\
    static ERL_NIF_TERM func(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]);
#endif

NIF_LIST

#undef NIF
#ifdef NIF_TRACE
#define NIF(name,arity,func) NIF_FUNC(name, arity, trace##_##func##_##arity),
#else
#define NIF(name,arity,func) NIF_FUNC(name, arity, func),
#endif

static ErlNifFunc nif_funcs[] =
{
    NIF_LIST
};



static ERL_NIF_TERM make_error(ErlNifEnv* env, int e)
{
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(e)));
}

static void midi_dtor(ErlNifEnv* env, midi_dev_t* dp)
{
    UNUSED(env);

    DEBUGF("midi_dtor: close in=%lx, out=%lx",
	   (intptr_t)dp->in, (intptr_t)dp->out);
    if (dp->in != INVALID) {
	CLOSE(dp->in);
	dp->in = INVALID;
    }
    if (dp->out != INVALID) {
	if (dp->out != dp->in)
	    CLOSE(dp->out);
	dp->out = INVALID;
    }
}

static void midi_stop(ErlNifEnv* env, midi_dev_t* dp,
		      ErlNifEvent event, int is_direct_call)
{
    UNUSED(env);
    DEBUGF("midi_stop: event=%lx", (intptr_t)event);
    // FIXME?: currently just assume we close main handle
    if (dp->in != INVALID) {
	CLOSE(dp->in);
	dp->in = INVALID;
    }
    if (dp->out != INVALID) {
	if (dp->in != dp->out)
	    CLOSE(dp->out);
	dp->out = INVALID;
    }
}

static void midi_down(ErlNifEnv* env, midi_dev_t* dp,
		      const ErlNifPid* pid, const ErlNifMonitor* mon)
{
    UNUSED(env);
    DEBUGF("midi_down: in=%lx, out=%lx",
	(intptr_t)dp->in, (intptr_t)dp->out);
}


static void midi_scan_init(midi_dev_t* dp)
{
    dp->state = STATE_STATUS;
    dp->pos = 0;
    dp->running_in = 0;
    dp->status_in  = 0;
    dp->len = 0;
    dp->last_time = enif_monotonic_time(ERL_NIF_USEC);
}

static int midi_scan(ErlNifEnv* env, ErlNifUInt64 delta,
		     midi_dev_t* dp, uint8_t* ptr, size_t len)
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
    if (dp->timestamp_mode) {
	// send as {midi, Fd, Event, DeltaTime[ms]}
	msg = enif_make_tuple4(env, ATOM(midi),
			       enif_make_resource(env, dp),
			       event,
			       enif_make_uint64(env, delta));
	delta = 0;
    }
    else {
	// send as {midi, Fd, Event}
	msg = enif_make_tuple3(env, ATOM(midi),
			       enif_make_resource(env, dp),
			       event);
    }
    
    enif_send(env, enif_self(env, &pid), NULL, msg);

    
    state = STATE_STATUS;
    dp->pos = 0;
    dp->len = 0;
    count++;
    goto again;
}

// open(Device::string(),[raw,binary]) -> Synth::integer.
static ERL_NIF_TERM nif_open(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    midi_handle_t in, out;
    midi_dev_t* dp;
    char devicename[1024];
    ERL_NIF_TERM dev;
    ERL_NIF_TERM list;
    ERL_NIF_TERM head, tail;
    int raw_mode = 0;
    int bin_mode = 1;
    int running_mode = 0;
    int timestamp_mode = 0;
    
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
	else if (head == ATOM(timestamp)) timestamp_mode = 1;
	else return enif_make_badarg(env);
	list = tail;
    }
    if (!enif_is_empty_list(env, list))
	return enif_make_badarg(env);

#ifdef ALSA
    {
	int mode = SND_RAWMIDI_SYNC;
	if (snd_rawmidi_open(&in, &out, devicename, mode) < 0)
	    return make_error(env, errno);
	snd_rawmidi_nonblock(in, 1);
	snd_rawmidi_nonblock(out, 1);
    }
#else
    if ((in = open(devicename, O_RDWR)) < 0)
	return make_error(env, errno);
    else {
	int flags = fcntl(in, F_GETFL, 0);
	fcntl(in, F_SETFL, flags|O_NONBLOCK);
    }
    out = in;
#endif
    if ((dp = enif_alloc_resource(midi_r, sizeof(midi_dev_t))) == NULL) {
	int e = errno;
	CLOSE(in);
	if (in != out) CLOSE(out);
	return make_error(env, e);
    }
    dp->in = in;
    dp->out = out;
    dp->raw_mode = raw_mode;
    dp->bin_mode = bin_mode;
    dp->running_mode = running_mode;
    dp->timestamp_mode = timestamp_mode;
    dp->status_out = 0;
    midi_scan_init(dp);
    dev = enif_make_resource(env, dp);
    enif_release_resource(dp);
    return enif_make_tuple2(env, ATOM(ok), dev);
}

// close(Synth::integer()) -> ok
static ERL_NIF_TERM nif_close(ErlNifEnv* env, int argc,
			      const ERL_NIF_TERM argv[])
{
    midi_dev_t* dp;
    if (!enif_get_resource(env, argv[0], midi_r, (void**)&dp))
	return enif_make_badarg(env);
    DEBUGF("midi:close: close in=%lx, out=%lx",
	   (intptr_t)dp->in, (intptr_t)dp->out);
    // DRAIN(dp->out) ?
    if (dp->in != INVALID) {
	ErlNifPid pid;
	enif_self(env, &pid);
	enif_select(env, (ErlNifEvent)EVENT_HANDLE(dp->in),
		    ERL_NIF_SELECT_STOP,
		    dp, &pid, ATOM(undefined));
    }
    return ATOM(ok);
}

// write(Synth::integer(), Data::binary())
// if running mode then check the first byte againt
// a the status byte to check if we may skip to write it
static ERL_NIF_TERM nif_write(ErlNifEnv* env, int argc,
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
	n = WRITE(dp->out,binary.data+1,binary.size-1);
    }
    else if (binary.size >= 1) {
	n = WRITE(dp->out,binary.data,binary.size);
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

static ERL_NIF_TERM nif_read(ErlNifEnv* env, int argc,
			     const ERL_NIF_TERM argv[])
{
    midi_dev_t* dp;
    unsigned char buffer[MAX_MIDI_BUF];
    ERL_NIF_TERM data;
    int n;

    if (!enif_get_resource(env, argv[0], midi_r, (void**)&dp))
	return enif_make_badarg(env);
        
    n = READ(dp->in, (char*)buffer, sizeof(buffer));
    if (n < 0) {
	if (errno == EAGAIN) {
	    ErlNifPid pid;
	    enif_self(env, &pid);
	    enif_select(env, (ErlNifEvent)EVENT_HANDLE(dp->in),
			ERL_NIF_SELECT_READ,
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
	ErlNifTime last_time = enif_monotonic_time(ERL_NIF_USEC);
	ErlNifTime delta = last_time - dp->last_time;
	n = midi_scan(env, delta, dp, buffer, n);
	dp->last_time = last_time;
	return enif_make_tuple2(env, ATOM(ok), enif_make_int(env, n));
    }
}

// create all tracing NIFs
#ifdef NIF_TRACE

#undef NIF

static void trace_print_arg_list(ErlNifEnv* env,int argc,const ERL_NIF_TERM argv[])
{
    enif_fprintf(stdout, "(");
    if (argc > 0) {
	int i;
	if (enif_is_ref(env, argv[0])) {
	    // FIXME print object type if available
	    enif_fprintf(stdout, "%T", argv[0]);
	}
	else
	    enif_fprintf(stdout, "%T", argv[0]);
	for (i = 1; i < argc; i++)
	    enif_fprintf(stdout, ",%T", argv[i]);
    }
    enif_fprintf(stdout, ")");
}

#define NIF(name, arity, func)					\
static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
{ \
    ERL_NIF_TERM result;					\
    enif_fprintf(stdout, "ENTER %s", (name));			\
    trace_print_arg_list(env, argc, argv);			\
    enif_fprintf(stdout, "\r\n");				\
    result = func(env, argc, argv);				\
    enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
    enif_fprintf(stdout, "LEAVE %s\r\n", (name));		\
    return result;						\
}

NIF_LIST

#endif

static int load_atoms(ErlNifEnv* env)
{
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
    LOAD_ATOM(timestamp);
    return 0;
}


static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;
    ErlNifResourceTypeInit cb;
    DEBUGF("load%s", "");

    cb.dtor = (ErlNifResourceDtor*) midi_dtor;
    cb.stop = (ErlNifResourceStop*) midi_stop;
    cb.down = (ErlNifResourceDown*) midi_down;
    
    if ((midi_r =
	 enif_open_resource_type_x(env, "midi",
				   &cb, ERL_NIF_RT_CREATE,
				   &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;

    *priv_data = 0;
    return 0;
}

static int upgrade(ErlNifEnv* env, void** priv_data,
		   void** old_priv_data,
		   ERL_NIF_TERM load_info)
{
    UNUSED(env);
    UNUSED(load_info);
    ErlNifResourceFlags tried;    
    ErlNifResourceTypeInit cb;
    
    DEBUGF("upgrade%s", "");

    cb.dtor = (ErlNifResourceDtor*) midi_dtor;
    cb.stop = (ErlNifResourceStop*) midi_stop;
    cb.down = (ErlNifResourceDown*) midi_down;

    if ((midi_r = enif_open_resource_type_x(env, "midi", &cb,
					    ERL_NIF_RT_CREATE|
					    ERL_NIF_RT_TAKEOVER,
					    &tried)) == NULL) {
	return -1;
    }
    if (load_atoms(env) < 0)
	return -1;    
    *priv_data = *old_priv_data;
    return 0;
}

static void unload(ErlNifEnv* env, void* priv_data)
{
    UNUSED(env);
    UNUSED(priv_data);
    DEBUGF("unload%s", "");
}

ERL_NIF_INIT(midi, nif_funcs, load, NULL, upgrade, unload)
