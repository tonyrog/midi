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
	 control/4, volume/3, all_off/2, reset_all/2,
	 bank/3, program_change/3]).
-export([control_fine/4,
	 volume_fine/3,
	 bank_fine/3]).

-export([start/0, start/1, stop/0]).
-export([setup_synth/0, open_synth/0]).
-export([devices/0]).
-export([find_device_by_port/2]).
-export([find_device_by_name/2]).
-export([find_synth_input_port/1]).

-export([io_prog/1, input_prog/2]).
-export([proxy/2]).
%% nifs
-export([open/2, close_/1, read_/1, write_/2]).

-define(SYNTH, midi_fluid).
-define(BACKEND, midi_alsa).

-include("midi.hrl").

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(midi), "midi_alsa"),
    erlang:load_nif(Nif, 0).

start() ->
    application:ensure_all_started(midi).

start([File]) when is_atom(File) ->
    application:ensure_all_started(midi),    
    midi_play:file(atom_to_list(File)).

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

control(Synth, Chan, Control, Arg) ->
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1,Control:7, 0:1,Arg:7>>).

control_fine(Synth, Chan, Control, Arg) ->
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1,Control:7, 0:1, (Arg bsr 7):7>>),
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1,(Control+1):7, Arg:7>>).

volume(Synth, Chan, Volume) ->
    control(Synth, Chan, ?MIDI_CTRL_VOLUME, Volume).

volume_fine(Synth, Chan, Volume) ->
    control_fine(Synth, Chan, ?MIDI_CTRL_VOLUME, Volume).

pitch_bend(Synth, Chan, Bend) ->
    Bend1 = Bend + 16#2000,
    write(Synth, <<?MIDI_EVENT_PITCHBEND:4,Chan:4,
		     0:1,Bend1:7, 0:1,(Bend1 bsr 7):7>>).

pressure(Synth, Chan, Pressure) ->
    write(Synth, <<?MIDI_EVENT_PRESSURE:4,Chan:4,0:1,Pressure:7>>).

all_off(Synth, Chan) ->
    control(Synth, Chan, ?MIDI_CTRL_ALL_NOTES_OFF, 0).

reset_all(Synth, Chan) ->
    control(Synth, Chan, ?MIDI_CTRL_ALL_CONTROLLERS_OFF, 0).

bank(Synth, Chan, Bank) ->
    control(Synth, Chan, ?MIDI_CTRL_BANK_SELECT, Bank).

bank_fine(Synth, Chan, Bank) ->
    control_fine(Synth, Chan, ?MIDI_CTRL_BANK_SELECT, Bank).

program_change(Synth, Chan, Prog) ->
    write(Synth, <<?MIDI_EVENT_PROGRAMCHANGE:4,Chan:4,0:1,Prog:7>>).

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
	    message_loop(Fd, CallBack, State)
    after 0 ->
	    input_loop(Fd, CallBack, State)
    end.

devices() ->
    ?BACKEND:devices().

open_synth() ->
    case setup_synth() of
	{ok,#{ device:=Device} } -> open(Device, [running]);
	Error -> Error
    end.

%% try start synth as server 
setup_synth() ->
    Ds = ?BACKEND:devices(),  %% fixme: backend
    case ?SYNTH:find_port(Ds) of
	false ->
	    ?SYNTH:start(?BACKEND),
	    Ds1 = ?BACKEND:devices(),
	    case ?SYNTH:find_port(Ds1) of
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
    case ?BACKEND:find_free_virtual_port(Ds) of
	false ->
	    io:format("forgot to install the virmid? run setup.sh\n"),
	    {error, no_ports_available};
	Virt ->
	    case ?BACKEND:connect(Virt, Synth) of
		"" -> {ok,Virt};
		Err -> {error,Err}
	    end
    end.

%% Lookup Device given a port 
find_device_by_port(Port, [D=#{port:=Port}|_Ds]) ->
    D;
find_device_by_port(Port, [_|Ds]) ->
    find_device_by_port(Port, Ds);
find_device_by_port(_Port, []) ->
    false.

%% Lookup Device given client name
find_device_by_name(Name, [D=#{client_name:=Name}|_Ds]) ->
    D;
find_device_by_name(Name, [_|Ds]) ->
    find_device_by_name(Name, Ds);
find_device_by_name(_Name, []) ->
    false.

%% Locate the synth input port
find_synth_input_port(Ds) ->
    case ?SYNTH:find_port(Ds) of
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
