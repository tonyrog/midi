%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    midi driver
%%% @end
%%% Created : 29 Dec 2017 by Tony Rogvall <tony@rogvall.se>

-module(midi).

-on_load(init/0).
-export([close/1, read/1, write/2]).
-export([note_on/4, note_off/4, note_off/3, 
	 pitch_bend/3, pressure/3,
	 all_off/2, reset_all/2,
	 program_change/3]).
-export([pan/3, pan2/3]).
-export([bank/3, bank2/3]).
-export([volume/3, volume2/3]).
-export([control7/4, control14/5]).

-export([expression/3, expression2/3]).

-export([start/0, start/1, stop/0]).
-export([setup_synth/0, open_synth/0]).
-export([devices/0]).
-export([find_synth_device/2]).
-export([find_device_by_port/2]).
-export([find_device_by_name/2]).
-export([find_synth_input_port/1]).

-export([io_prog/1, input_prog/2]).
-export([proxy/2]).
-export([shared_input/1]).
-export([tparam/2]).
-export([tparam_set_tempo/2]).
-export([tparam_set_time_signature/2]).

%% nifs
-export([open/2, close_/1, read_/1, write_/2]).
-export([backend/0, synth/0]).

%% util
-export([play_file/1]).

-define(DEFAULT_MIDI_SYNTH, midi_fluid).
-define(DEFAULT_MIDI_BACKEND, midi_alsa).

-include("../include/midi.hrl").

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(midi), "midi_alsa"),
    erlang:load_nif(Nif, 0).

start() ->
    case os:cmd("lsmod | grep snd_virmidi") of
	[] ->
	    error(no_snd_virmidi);
	_ ->
	    application:ensure_all_started(midi)
    end.

start([File]) when is_atom(File) ->
    start(),
    play_file(File).

play_file(Filename) when is_atom(Filename) ->
    play_file(atom_to_list(Filename));
play_file(Filename) when is_list(Filename) ->    
    case filename:extension(Filename) of
	".mid" ->
	    midi_play:file(Filename);
	".abc" ->
	    midi_abc:play_file(Filename)
    end.    


stop() ->
    application:stop(midi).

%% open midi device
open(_DeviceName, _OptionList) ->
    ?nif_stub().

% close midi device
close(Name) when is_atom(Name) ->
    close_(midi_reg:whereis(Name));
close(Fd) ->
    close_(Fd).

close_(_Fd) ->
    ?nif_stub().

write(Name,Data) when is_atom(Name) ->
    write_(midi_reg:whereis(Name), Data);
write(Fd,Data) ->
    write_(Fd,Data).

write_(_Fd, _Data) ->
    ?nif_stub().

read(Name) when is_atom(Name) ->
    read_(midi_reg:whereis(Name));
read(Fd) ->
    read_(Fd).

read_(_Fd) ->
    ?nif_stub().

note_on(Synth, Chan, Note, Velocity) ->
    write(Synth, <<?MIDI_EVENT_NOTEON:4,Chan:4,0:1,Note:7,0:1,Velocity:7>>).

%% note! using ?MIDI_NOTE_ON(Chan), Note, 0>> 
%% may save a byte in the running status when stopping a chord
note_off(Synth, Chan, Note) ->
    write(Synth, <<?MIDI_EVENT_NOTEON:4,Chan:4,0:1,Note:7,0>>).
note_off(Synth, Chan, Note, Velocity) ->
    write(Synth, <<?MIDI_EVENT_NOTEOFF:4,Chan:4,0:1,Note:7,
		   0:1,Velocity:7>>).

volume(Synth, Chan, Volume) ->
    control7(Synth, Chan, 
	     ?MIDI_CTRL_VOLUME,
	     unsigned7(Volume)).

volume2(Synth, Chan, Volume) ->
    control14(Synth,Chan,
	      ?MIDI_CTRL_VOLUME,?MIDI_CTRL_VOLUME_FINE,
	      unsigned14(Volume)).

pan(Synth, Chan, Pan) ->
    Value7 = signed7(Pan),
    io:format("pan=~w\n", [Value7]),
    control7(Synth, Chan, 
	     ?MIDI_CTRL_PAN_POSITION, 
	     Value7).

pan2(Synth, Chan, Pan) ->
    Value14 = signed14(Pan),
    io:format("pan=~w\n", [Value14]),
    control14(Synth,Chan,
	      ?MIDI_CTRL_PAN_POSITION,
	      ?MIDI_CTRL_PAN_POSITION_FINE,
	      Value14).

pitch_bend(Synth, Chan, Bend) ->
    Bend1 = signed14(Bend),
    write(Synth, <<?MIDI_EVENT_PITCHBEND:4,Chan:4,
		     0:1,Bend1:7, 0:1,(Bend1 bsr 7):7>>).

pressure(Synth, Chan, Pressure) ->
    write(Synth, <<?MIDI_EVENT_PRESSURE:4,Chan:4,0:1,Pressure:7>>).

all_off(Synth, Chan) ->
    control7(Synth, Chan, ?MIDI_CTRL_ALL_NOTES_OFF, 0).

reset_all(Synth, Chan) ->
    control7(Synth, Chan, ?MIDI_CTRL_ALL_CONTROLLERS_OFF, 0).

bank(Synth, Chan, Bank) when is_integer(Bank), Bank >=0 ->
    control7(Synth, Chan, 
	     ?MIDI_CTRL_BANK_SELECT,
	     Bank).

bank2(Synth, Chan, Bank) when is_integer(Bank), Bank >= 0 ->
    control14(Synth, Chan, 
	      ?MIDI_CTRL_BANK_SELECT,
	      ?MIDI_CTRL_BANK_SELECT_FINE, 
	      Bank).




expression(Synth, Chan, Value) ->
    control7(Synth, Chan, 
	     ?MIDI_CTRL_EXPRESSION,
	     unsigned7(Value)).

expression2(Synth, Chan, Value) ->
    control14(Synth, Chan, 
	      ?MIDI_CTRL_EXPRESSION,
	      ?MIDI_CTRL_EXPRESSION_FINE,
	      unsigned14(Value)).

signed7(Value) when is_integer(Value) ->
    (Value+16#40) band 16#7f;
signed7(Value) when is_float(Value), abs(Value) =< 1.0 ->
    trunc((Value+1.0)*16#3f) band 16#7f.

unsigned7(Value) when is_integer(Value), Value >= 0 ->
    Value band 16#7f;
unsigned7(Value) when is_float(Value), Value >= 0, Value =< 1.0 ->
    trunc(Value * 16#7f) band 16#7f.

signed14(Value) when is_integer(Value) ->
    (Value+16#2000) band 16#3fff;
signed14(Value) when is_float(Value), abs(Value) =< 1.0 ->
    trunc((Value+1.0)*16#1fff) band 16#3fff.

unsigned14(Value) when is_integer(Value), Value >= 0 ->
    Value band 16#3fff;
unsigned14(Value) when is_float(Value), Value >= 0, Value =< 1.0 ->
    trunc((Value*16#1fff) + 16#2000) band 16#3fff.


control7(Synth, Chan, Control, Arg) ->
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1,Control:7, 0:1,Arg:7>>).

control14(Synth, Chan, Control, ControlFine, Arg) ->
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1,Control:7, 0:1, (Arg bsr 7):7>>),
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1,ControlFine:7, 0:1, Arg:7>>).


program_change(Synth, Chan, Prog) ->
    write(Synth, <<?MIDI_EVENT_PROGRAMCHANGE:4,Chan:4,0:1,Prog:7>>).

%% time parameters

%% time_us()
%% WaitUs = (Now - Ticks)*TParam#tparam.uspp,
tparam(BPM, Division) ->
    MPQN = ?USEC_PER_MINUTE/BPM,
    PPQN = if Division >= 0 -> Division; true -> 1.0 end,
    USPP =
	if Division >= 0 ->
		MPQN / PPQN;
	   true ->
		Div = trunc(Division),
		TicksPerFrame = Div band 16#ff,
		FramesPerSec = 
		    case Div bsr 8 of
			-24 -> 24.0;
			-25 -> 25.0;
			-29 -> 29.97;
			-30 -> 30.0
		    end,
		MPQN / (FramesPerSec*TicksPerFrame)
	end,
    #tparam{mpqn=MPQN,ppqn=PPQN,bpm=BPM,uspp=USPP}.

tparam_set_tempo(TParam=#tparam{ppqn=PPQN}, MPQN) ->
    BPM  = ?USEC_PER_MINUTE / MPQN,
    USPP = MPQN / PPQN,  %% micro seconds per (midi) pulse
    TParam#tparam{mpqn=MPQN,bpm=BPM,uspp=USPP}.

tparam_set_time_signature(TParam, [NN,DD,CC,BB]) ->
    {ok, TParam#tparam{sig={NN,(1 bsl DD)},cc=CC,bb=BB}}.

%% 
proxy(InputDevice, OutputDevice) ->
    {ok,In}  = open(InputDevice,[event,list,running]),
    {ok,Out} = open(OutputDevice, [running]),
    proxy_in(In,Out).

proxy_in(In,Out) ->
    case read(In) of
	select ->
	    receive
		{select,In,undefined,ready_input} ->
		    proxy_in(In,Out)
	    end;
	{ok,N} when is_integer(N) -> %% got N events
	    proxy_out(In,Out);
	Error->
	    Error
    end.

proxy_out(In, Out) ->
    receive
	{midi,In,Event} -> %% driver handle packet
	    io:format("midi event ~p\n", [Event]),
	    write(Out, midi_codec:event_encode(Event)),
	    proxy_out(In,Out);
	{midi,In,Event,_Delta} -> %% driver handle packet
	    io:format("midi event ~p\n", [Event]),
	    write(Out, midi_codec:event_encode(Event)),
	    proxy_out(In,Out)
    after 0 ->
	    proxy_in(In,Out)
    end.
	    
io_prog(DeviceName) ->
    input_prog(DeviceName, 
	       fun(_Fd,Event) ->
		       io:format("~p\n", [Event])
	       end).

input_prog(DeviceName, CallBack) ->
    case open(DeviceName,[event,list,running]) of
	{ok,Fd} ->
	    input_loop(Fd, CallBack, midi_codec:init([]));
	Error ->
	    Error
    end.

input_loop(Fd, CallBack, State) ->
    case read(Fd) of
	{ok,Chars} when is_list(Chars) ->
	    case midi_codec:scan(Chars, State) of
		{more, State1} ->
		    input_loop(Fd, CallBack, State1);
		{{Status,Params},State1} ->
		    Event = midi_codec:event(Status, Params),
		    CallBack(Fd, Event),
		    input_loop(Fd, CallBack, State1)
	    end;
	{ok,_N} when is_integer(_N) -> %% got N events
	    message_loop(Fd, CallBack, State);
	select ->
	    receive
		{select,Fd,undefined,ready_input} ->
		    input_loop(Fd,CallBack,State)
	    end;	    
	Error ->
	    Error
    end.

message_loop(Fd, CallBack, State) ->
    receive
	{midi,Fd,Event} -> %% driver handle packet
	    io:format("midi event ~p\n", [Event]),
	    CallBack(Fd, Event),
	    message_loop(Fd, CallBack, State);
	{midi,Fd,Event,_Delta} -> %% driver handle packet
	    io:format("midi event ~p\n", [Event]),
	    CallBack(Fd, Event),
	    message_loop(Fd, CallBack, State)
    after 0 ->
	    input_loop(Fd, CallBack, State)
    end.

synth() ->
    application:get_env(midi, midi_synth, ?DEFAULT_MIDI_SYNTH).

backend() ->
    application:get_env(midi, midi_backend, ?DEFAULT_MIDI_BACKEND).

devices() ->
    (backend()):devices().

%% open "shared" input midi device (keyboard etc) by first try to locate
%% a virtual device already connected to the midi devie.
%% otherwise find a "free" virtual device and connect the
%% then return that device for application to open instead of "real"
%% hardware device that is mostly one at the time access...

shared_input(Name) ->
    B = backend(),    
    Devices = B:devices(),
    case find_device_by_name(Name, Devices) of
	false ->
	    {error, enoent};
	#{output := [Port]} -> %% already connected
	    case midi:find_device_by_port(Port, Devices) of
		false ->
		    {error, port_not_found};  %% maybe closing, retry?
		Parent ->
		    %% maybe check that "Parent" is a virtual device?
		    Parent
	    end;
	Input ->
	    %% find free virtual device
	    case B:find_free_virtual_port(Devices) of
		false ->
		    io:format("forgot to install the virmid? run setup.sh\n"),
		    {error, no_ports_available};
		Virtual = #{ device := _DeviceName } ->
		    case B:connect(Input, Virtual) of
			"" -> Virtual;
			Err -> {error, Err}
		    end
	    end
    end.

open_synth() ->
    case setup_synth() of
	{ok,#{ device:=Device} } -> open(Device, [running]);
	Error -> Error
    end.

%% try start synth as server 
setup_synth() ->
    B = backend(),
    S = synth(),
    Ds = B:devices(),
    case find_synth_device(S, Ds) of
	false ->
	    start_synth(S),
	    Ds1 = B:devices(),
	    %% retry lookup
	    case find_synth_device(S, Ds1) of
		false -> {error, synth_not_started};
		Synth -> setup_synth(Synth,Ds1)
	    end;
	Synth ->
	    setup_synth(Synth,Ds)
    end.

setup_synth(Synth,Ds) ->
    case find_synth_input_port(Ds) of
	false -> setup_synth_input(Synth,Ds);
	Input -> {ok,Input}
    end.

setup_synth_input(Synth,Ds) ->
    B = backend(),
    case B:find_free_virtual_port(Ds) of
	false ->
	    io:format("forgot to install the virmid? run setup.sh\n"),
	    {error, no_ports_available};
	Virt ->
	    case B:connect(Virt, Synth) of
		"" -> {ok,Virt};
		Err -> {error,Err}
	    end
    end.


start_synth(S) when is_atom(S) ->
    S:start();
start_synth(Name) when is_list(Name) ->
    undefined.

find_synth_device(S, Ds) when is_atom(S) ->
    Name = S:portname(),
    find_device_by_name(Name, Ds);
find_synth_device(Name, Ds) when is_list(Name) ->
    find_device_by_name(Name, Ds).


%% Lookup Device given a port 
find_device_by_port(Port, [D=#{port:=Port}|_Ds]) ->
    D;
find_device_by_port(Port, [_|Ds]) ->
    find_device_by_port(Port, Ds);
find_device_by_port(_Port, []) ->
    false.

%% Lookup Device given client name (actually by prefix)
find_device_by_name(Name, [D=#{client_name:=ClientName}|Ds]) ->
    case lists:prefix(Name, ClientName) of
	true -> D;
	false -> find_device_by_name(Name, Ds)
    end;
find_device_by_name(_Name, []) ->
    false.

%% Locate the synth input port
find_synth_input_port(Ds) ->
    S = synth(),
    case find_synth_device(S, Ds) of
	false -> false;
	Synth -> find_input_port(maps:get(port,Synth),Ds)
    end.

%% Locate the input port/device 
find_input_port(Port,[D=#{output:=OUT}|Ds]) ->
    case lists:member(Port, OUT) of
	true -> D;
	false -> find_input_port(Port,Ds)
    end;
find_input_port(_Port,[]) ->
    false.
