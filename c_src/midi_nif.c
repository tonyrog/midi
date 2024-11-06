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

// #define DEBUG

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
#endif

#define ATOM(name) atm_##name

#define DECL_ATOM(name)				\
    ERL_NIF_TERM atm_##name = 0

// require env in context (ugly)
#define LOAD_ATOM(name)				\
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


#define NIF_LIST				\
    NIF("open", 2,  nif_open)			\
    NIF("close_", 1, nif_close)			\
    NIF("write_", 2, nif_write)			\
    NIF("read_", 1,  nif_read)			\
    NIF("select_", 2, nif_select)

static ErlNifResourceType* midi_r;

#define MAX_MIDI_BUF 2048 // at least 1171+5 (was 1024)

typedef struct {
#ifdef ALSA    
    snd_rawmidi_t* h;  // set if device opened with snd_rawmidi_open, else NULL
#else
    void* h;           // NULL
#endif
    int fd;            // set when device is a path or device file
} midi_handle_t;

#define EOX 0xf7

typedef enum {
    STATE_STATUS,
    STATE_PARAMS,
    STATE_PARAMS_EOX
} decode_state_t;


typedef struct _midi_dev_t {
    midi_handle_t in;
    midi_handle_t out;
    decode_state_t state;
    uint8_t params[MAX_MIDI_BUF];
    int     pos;
    int     pnum;       // number pf params to collect
    uint8_t status_in;
    uint8_t running_in;
    uint8_t status_out;    
    int     raw_mode;
    int     bin_mode;
    int     running_mode;
    int     timestamp_mode;
    ErlNifTime last_time;
} midi_dev_t;


DECL_ATOM(ok);
DECL_ATOM(error);
DECL_ATOM(undefined);

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

// sys events
DECL_ATOM(ex);
DECL_ATOM(sys1);
DECL_ATOM(song_position);
DECL_ATOM(song_select);
DECL_ATOM(sys4);
DECL_ATOM(sys5);
DECL_ATOM(tune_request);
DECL_ATOM(eox);
DECL_ATOM(timing_clock);
DECL_ATOM(sys9);
DECL_ATOM(start);
DECL_ATOM(continue);
DECL_ATOM(stop);
DECL_ATOM(sys13);
DECL_ATOM(active_sensing);
DECL_ATOM(system_reset);


// open modes
DECL_ATOM(raw);
DECL_ATOM(event);
DECL_ATOM(binary);
DECL_ATOM(list);
DECL_ATOM(running);
DECL_ATOM(timestamp);

// select direction
DECL_ATOM(read);
DECL_ATOM(write);
DECL_ATOM(read_write);
DECL_ATOM(cancel_read);
DECL_ATOM(cancel_write);

// posix errors
DECL_ATOM(eagain);
DECL_ATOM(eintr);
DECL_ATOM(estrpipe);
DECL_ATOM(epipe);

// errors
DECL_ATOM(no_such_handle);
DECL_ATOM(select_already);
DECL_ATOM(bad_param);


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


static void midi_handle_clear(midi_handle_t* dp)
{
    dp->h = NULL;
    dp->fd = -1;
}
static int midi_handle_is_valid(midi_handle_t hndl)
{
    return (hndl.fd >= 0);
}

static int midi_write(midi_handle_t hndl, uint8_t* data, size_t len)
{
#ifdef ALSA
    if (hndl.h != NULL)
	return snd_rawmidi_write(hndl.h, data, len);
#endif
    return write(hndl.fd, data, len);
}

static int midi_read(midi_handle_t hndl, uint8_t* data, size_t len)
{
#ifdef ALSA
    if (hndl.h != NULL)
	return snd_rawmidi_read(hndl.h, data, len);
#endif
    return read(hndl.fd, data, len);
}	

static int midi_close(midi_handle_t hndl)
{
    int r;
#ifdef ALSA
    if (hndl.h != NULL) {
	DEBUGF("midi_close: close alsa h=%lx", (intptr_t)hndl.h);
	r = snd_rawmidi_close(hndl.h);
	return r;
    }
#endif
    DEBUGF("midi_close: close fd=%d", hndl.fd);
    r = close(hndl.fd);
    return r;
}

static int midi_event_handle(midi_handle_t hndl)
{
    return hndl.fd;
}


#ifdef ALSA
static int alsa_event_handle(snd_rawmidi_t* h, int events)
{
    int i, n;
    struct pollfd pfd[4];
    n = snd_rawmidi_poll_descriptors_count(h);
    snd_rawmidi_poll_descriptors(h, pfd, n);
    if (n == 1)
	return pfd[0].fd;
    for (i = 0; i < n; i++) {
	if (pfd[i].events & events)
	    return pfd[i].fd;
    }
    return -1;
}
#endif


static ERL_NIF_TERM make_error(ErlNifEnv* env, int e)
{
    return enif_make_tuple2(env,
			    ATOM(error),
			    enif_make_atom(env, erl_errno_id(e)));
}

static void midi_dev_close(midi_dev_t* dp)
{
    if (midi_handle_is_valid(dp->in)) {
	midi_close(dp->in);
    }
    if (midi_handle_is_valid(dp->out)) {
	if (midi_event_handle(dp->in) != midi_event_handle(dp->out))
	    midi_close(dp->out);
	midi_handle_clear(&dp->out);
    }
    midi_handle_clear(&dp->in);
}

static void midi_dtor(ErlNifEnv* env, midi_dev_t* dp)
{
    UNUSED(env);

    DEBUGF("midi_dtor: close in=%lx, out=%lx",
	   (intptr_t)dp->in.fd, (intptr_t)dp->out.fd);
    midi_dev_close(dp);
}

static void midi_stop(ErlNifEnv* env, midi_dev_t* dp,
		      ErlNifEvent event, int is_direct_call)
{
    UNUSED(env);
    DEBUGF("midi_stop: event=%lx", (intptr_t)event);
    midi_dev_close(dp);
}

static void midi_down(ErlNifEnv* env, midi_dev_t* dp,
		      const ErlNifPid* pid, const ErlNifMonitor* mon)
{
    UNUSED(env);
    DEBUGF("midi_down: in=%lx, out=%lx",
	   (intptr_t)dp->in.fd, (intptr_t)dp->out.fd);
}

static void midi_scan_init(midi_dev_t* dp)
{
    dp->state = STATE_STATUS;
    dp->pos = 0;
    dp->running_in = 0;
    dp->status_in  = 0;
    dp->pnum = 0;
    dp->last_time = enif_monotonic_time(ERL_NIF_USEC);
}

static int send_event(ErlNifEnv* env, ErlNifUInt64 delta,
		      midi_dev_t* dp, ERL_NIF_TERM event)
{
    ERL_NIF_TERM msg;
    ErlNifPid pid;

    if (dp->timestamp_mode) {
	// send as {midi, Fd, Event, DeltaTime[ms]}
	msg = enif_make_tuple4(env, ATOM(midi),
			       enif_make_resource(env, dp),
			       event,
			       enif_make_uint64(env, delta));
    }
    else {
	// send as {midi, Fd, Event}
	msg = enif_make_tuple3(env, ATOM(midi),
			       enif_make_resource(env, dp),
			       event);
    }
    enif_send(env, enif_self(env, &pid), NULL, msg);
    return 0;
}

static int send_sys_event(ErlNifEnv* env, ErlNifUInt64 delta,
			  midi_dev_t* dp, int num)
{
    ERL_NIF_TERM event;
    ERL_NIF_TERM type = ATOM(error);

    switch(num) {
    case 0x8: type = ATOM(timing_clock); break;
    case 0xA: type = ATOM(start); break;
    case 0xB: type = ATOM(continue); break;
    case 0xC: type = ATOM(stop); break;
    case 0xE: type = ATOM(active_sensing);break;
    case 0xF: type = ATOM(system_reset);break;
    default: return 0;
    }
    event = enif_make_tuple3(env, ATOM(sys), type, ATOM(undefined));
    return send_event(env, delta, dp, event);
}

static int midi_scan(ErlNifEnv* env, ErlNifUInt64 delta,
		     midi_dev_t* dp, uint8_t* ptr, size_t len)
{
    ERL_NIF_TERM event;
    int count = 0;
    int state = dp->state;
    uint8_t* end = ptr + len;
    uint8_t b;
    int pnum = dp->pnum;
again:
    while(ptr < end) {
	b = ptr[0];
	// check for real time messages
	if ((b >= 0xf8) && (b <= 0xff)) {
	    send_sys_event(env, delta, dp, (b & 0x0f));
	    ptr++;
	    continue;
	}
	switch(state) {
	case STATE_STATUS:
	    state = STATE_PARAMS;
	    dp->pos = 0;
	    if ((b & 0x80) == 0) {
		dp->status_in = dp->running_in;
	    }
	    else {
		if ((b & 0xf0) == 0xf0) {
		    if (b <= 0xf7)
			dp->running_in = 0;
		}
		else
		    dp->running_in = b;
		dp->status_in = b;
		ptr++;
	    }
	    switch(dp->status_in & 0xf0) {
	    case MIDI_EVENT_NOTEOFF: pnum=2; break;
	    case MIDI_EVENT_NOTEON:  pnum=2; break;
	    case MIDI_EVENT_AFTERTOUCH: pnum=2; break;
	    case MIDI_EVENT_CONTROLCHANGE: pnum=2; break;
	    case MIDI_EVENT_PITCHBEND: pnum=2; break;
	    case MIDI_EVENT_PROGRAMCHANGE: pnum=1; break;
	    case MIDI_EVENT_PRESSURE: pnum=1; break;
	    case MIDI_EVENT_SYS:
		switch(dp->status_in & 0x0f) {
		case 0: state = STATE_PARAMS_EOX; break;
		case 1: pnum=1; break; // quarter frame
		case 2: pnum=2; break; // song position
		case 3: pnum=1; break; // song select
		case 4: state = STATE_STATUS; break;
		case 5: state = STATE_STATUS; break;
		case 6: pnum=0; break; // tune request
		case 7: state = STATE_STATUS; break;
		    break;
		}
		break;
	    }
	    break;
	case STATE_PARAMS_EOX:
	    if ((b & 0x80)) { // non-real time status (terminates)
		if (b == 0xf7)
		    ptr++;
		goto done;
	    }
	    else {
		if (dp->pos < MAX_MIDI_BUF) 
		    dp->params[dp->pos++] = b;
		ptr++;
	    }
	    break;
	case STATE_PARAMS:
	    if (pnum > 0) {
		if (dp->pos < MAX_MIDI_BUF)
		    dp->params[dp->pos++] = b;
		ptr++;
		pnum--;
	    }
	    if (pnum == 0)
		goto done;
	    break;
	}
    }
    dp->pnum = pnum;
    dp->state = state;
    return count;

done:
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
	ERL_NIF_TERM type;
	if (dp->bin_mode) {
	    uint8_t* ptr = enif_make_new_binary(env, dp->pos, &data);
	    memcpy(ptr, dp->params, dp->pos);
	}
	else {
	    data = enif_make_string_len(env,(char*)dp->params,dp->pos,
					ERL_NIF_LATIN1);
	}
	switch(dp->status_in & 0x0f) {
	case 0: type = ATOM(ex); break;
	case 1: type = ATOM(sys1); break;
	case 2: type = ATOM(song_position); break;
	case 3: type = ATOM(song_select); break;
	case 4: type = ATOM(sys4); break;
	case 5: type = ATOM(sys5); break;
	case 6: type = ATOM(tune_request); break;
	case 7: type = ATOM(eox); break;
	default: type = ATOM(error); break;
	}
	event = enif_make_tuple3(env, ATOM(sys), type, data);
	break;
    }
    default:
	event = ATOM(undefined);
	break;
    }
    
    send_event(env, delta, dp, event);

    delta = 0;
    state = STATE_STATUS;
    dp->pos = 0;
    dp->pnum = 0;
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
    
    midi_handle_clear(&in);
    midi_handle_clear(&out);

#ifdef ALSA
    if ((strncmp(devicename, "hw:", 3) == 0) ||
	(strcmp(devicename, "hw") == 0) ||
	(strcmp(devicename, "virtual") == 0)) {
	int mode = SND_RAWMIDI_SYNC;
	if (snd_rawmidi_open(&in.h, &out.h, devicename, mode) < 0)
	    return make_error(env, errno);
	snd_rawmidi_nonblock(in.h, 1);
 	snd_rawmidi_nonblock(out.h, 1);
	in.fd = alsa_event_handle(in.h, POLLIN);
	out.fd = alsa_event_handle(out.h, POLLOUT);
	// snd_rawmidi_drain / snd_rawmidi_drop ?
    }
    else
#endif
    {
	int fd;
	if ((fd = open(devicename, O_RDWR)) < 0)
	    return make_error(env, errno);
	else {
	    int flags = fcntl(fd, F_GETFL, 0);
	    fcntl(fd, F_SETFL, flags|O_NONBLOCK);
	}
	in.fd = fd;
	out.fd = fd;
    }

    if ((dp = enif_alloc_resource(midi_r, sizeof(midi_dev_t))) == NULL) {
	int e = errno;
	midi_close(in);
	if (in.fd != out.fd)
	    midi_close(out);
	return make_error(env, e);
    }
    DEBUGF("midi_nif:open: in=%lx,fd=%d, out=%lx,fd=%d",
	   (intptr_t) in.h, in.fd,
	   (intptr_t) out.h, out.fd);
    
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
    int closed = 0;
    int fd;
    if (!enif_get_resource(env, argv[0], midi_r, (void**)&dp))
	return enif_make_badarg(env);
    DEBUGF("midi_nif:close: in=%lx,fd=%d, out=%lx,fd=%d",
	   (intptr_t)dp->in.h, dp->in.fd,
	   (intptr_t)dp->out.h, dp->out.fd);
    // DRAIN(dp->out) ?
    if (midi_handle_is_valid(dp->in)) {
	int sel;
	ErlNifPid pid;
	enif_self(env, &pid);
	fd = midi_event_handle(dp->in);
	sel = enif_select(env, (ErlNifEvent) fd,
			  ERL_NIF_SELECT_STOP, dp, &pid, ATOM(undefined));
	DEBUGF("nif_close: enif_select IN sel = %d", sel);
	if (sel < 0) {
	    // error
	}
	else if (sel & ERL_NIF_SELECT_STOP_CALLED) {
	    DEBUGF("midi_nif:close: %d select IN stop called", fd);
	    closed |= 1;
	}
	else if (sel & ERL_NIF_SELECT_STOP_SCHEDULED) {
	    DEBUGF("midi_nif:close: %d select IN stop scheduled", fd);
	    closed |= 1;
	}
    }
    if (closed && (midi_event_handle(dp->in) == midi_event_handle(dp->out)))
	closed |= 2;
    else if (midi_handle_is_valid(dp->out)) {
	int sel;
	ErlNifPid pid;
	enif_self(env, &pid);
	fd = midi_event_handle(dp->out);
	sel = enif_select(env, (ErlNifEvent)fd,
			  ERL_NIF_SELECT_STOP,
			  dp, &pid, ATOM(undefined));
	DEBUGF("nif_close: enif_select OUT sel = %d", sel);
	if (sel < 0) {
	    // error
	}	
	else if (sel & ERL_NIF_SELECT_STOP_CALLED) {
	    DEBUGF("midi_nif:close: %d select OUT stop called", fd);
	    closed |= 2;
	}
	else if (sel & ERL_NIF_SELECT_STOP_SCHEDULED) {
	    DEBUGF("midi_nif:close: %d select OUT stop scheduled", fd);
	    closed |= 2;
	}
    }
    switch(closed) {
    case 0: midi_dev_close(dp); break;
    case 3: midi_handle_clear(&dp->in); midi_handle_clear(&dp->out); break;
    case 1: midi_close(dp->out); midi_handle_clear(&dp->out); break;
    case 2: midi_close(dp->in); midi_handle_clear(&dp->in); break;
    }
    return ATOM(ok);
}

// write(Synth::integer(), Data::binary())
// FIXME: running mode!
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
    if (binary.size >= 1) {
	n = midi_write(dp->out,binary.data,binary.size);
    }
    if (n < 0) {
	if (errno == EAGAIN)
	    return enif_make_tuple2(env, ATOM(error), ATOM(eagain));
	return make_error(env, errno);
    }
    return enif_make_tuple2(env, ATOM(ok), enif_make_int(env, n));
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
        
    n = midi_read(dp->in, buffer, sizeof(buffer));
    if (n < 0) {
	if (errno == EAGAIN)
	    return enif_make_tuple2(env, ATOM(error), ATOM(eagain));
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


static ERL_NIF_TERM nif_select(ErlNifEnv* env, int argc,
			       const ERL_NIF_TERM argv[])
{
    midi_dev_t* dp;
    enum ErlNifSelectFlags mode = 0;
    ErlNifPid pid;
    
    if (!enif_get_resource(env, argv[0], midi_r, (void**)&dp))
	return enif_make_badarg(env);
    if (argv[1] == ATOM(read))
	mode = ERL_NIF_SELECT_READ;
    else if (argv[1] == ATOM(write))
	mode = ERL_NIF_SELECT_WRITE;
    else if (argv[1] == ATOM(read_write))
	mode = (ERL_NIF_SELECT_WRITE|ERL_NIF_SELECT_READ);
    else if (argv[1] == ATOM(cancel_read))
	mode = ERL_NIF_SELECT_CANCEL|ERL_NIF_SELECT_READ;
    else if (argv[1] == ATOM(cancel_write))
	mode = ERL_NIF_SELECT_CANCEL|ERL_NIF_SELECT_WRITE;    
    else
	return enif_make_badarg(env);	

    enif_self(env, &pid);
    if (mode & ERL_NIF_SELECT_CANCEL) {
	if (mode & ERL_NIF_SELECT_READ) {
	    switch(enif_select(env, dp->in.fd,mode,dp,&pid,ATOM(undefined))) {
	    case ERL_NIF_SELECT_READ_CANCELLED:
		return ATOM(ok);
	    default:
		return ATOM(error);
	    }
	}
	else {
	    switch(enif_select(env, dp->out.fd,mode,dp,&pid,ATOM(undefined))) {
	    case ERL_NIF_SELECT_WRITE_CANCELLED:
		return ATOM(read);
	    default:
		return ATOM(error);
	    }
	}
    }
    else {
	if (mode & ERL_NIF_SELECT_READ)
	    enif_select(env, dp->in.fd, ERL_NIF_SELECT_READ, dp,
			&pid, ATOM(undefined));
	if (mode & ERL_NIF_SELECT_WRITE)
	    enif_select(env, dp->out.fd, ERL_NIF_SELECT_WRITE, dp,
			&pid, ATOM(undefined));
    }
    return ATOM(ok);
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

#define NIF(name, arity, func)						\
    static ERL_NIF_TERM trace##_##func##_##arity(ErlNifEnv* env, int argc,const ERL_NIF_TERM argv[]) \
    {									\
	ERL_NIF_TERM result;						\
	enif_fprintf(stdout, "ENTER %s", (name));			\
	trace_print_arg_list(env, argc, argv);				\
	enif_fprintf(stdout, "\r\n");					\
	result = func(env, argc, argv);					\
	enif_fprintf(stdout, "  RESULT=%T\r\n", (result));		\
	enif_fprintf(stdout, "LEAVE %s\r\n", (name));			\
	return result;							\
    }

NIF_LIST

#endif

static int load_atoms(ErlNifEnv* env)
{
    LOAD_ATOM(ok);
    LOAD_ATOM(error);
    LOAD_ATOM(undefined);
    
    // events
    LOAD_ATOM(midi);    
    LOAD_ATOM(note_off);
    LOAD_ATOM(note_on);
    LOAD_ATOM(after_touch);
    LOAD_ATOM(control_change);
    LOAD_ATOM(pitch_bend);
    LOAD_ATOM(program_change);
    LOAD_ATOM(pressure);
    LOAD_ATOM(sys);

    // sys events
    LOAD_ATOM(ex);
    LOAD_ATOM(sys1);
    LOAD_ATOM(song_position);
    LOAD_ATOM(song_select);
    LOAD_ATOM(sys4);
    LOAD_ATOM(sys5);
    LOAD_ATOM(tune_request);
    LOAD_ATOM(eox);
    LOAD_ATOM(timing_clock);
    LOAD_ATOM(sys9);
    LOAD_ATOM(start);
    LOAD_ATOM(continue);
    LOAD_ATOM(stop);
    LOAD_ATOM(sys13);
    LOAD_ATOM(active_sensing);
    LOAD_ATOM(system_reset);    
    
    // open mode
    LOAD_ATOM(raw);
    LOAD_ATOM(event);
    LOAD_ATOM(binary);
    LOAD_ATOM(list);
    LOAD_ATOM(running);
    LOAD_ATOM(timestamp);

    // select direction
    LOAD_ATOM(read);
    LOAD_ATOM(write);
    LOAD_ATOM(read_write);
    LOAD_ATOM(cancel_read);
    LOAD_ATOM(cancel_write);    
    
    // posix errors
    LOAD_ATOM(eagain);
    LOAD_ATOM(eintr);
    LOAD_ATOM(estrpipe);
    LOAD_ATOM(epipe);

    // errors
    LOAD_ATOM(bad_param);
    LOAD_ATOM(no_such_handle);
    LOAD_ATOM(select_already);
    
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
