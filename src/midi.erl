%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    midi driver
%%% @end
%%% Created : 29 Dec 2017 by Tony Rogvall <tony@rogvall.se>

-module(midi).

-on_load(init/0).
-export([open/2, close/1, read/1, write/2]).
-export([note_on/4, note_off/4, control/4,
	 volume/3, all_off/2, reset_all/2,
	 bank_msb/3, program_change/3]).
-export([setup_synth/0, open_synth/0]).

%% fixme: move to separate module midi_alsa?
-export([devices/0]).
-export([devmidi/0]).
-export([find_fluid_port/1]).
-export([find_fluid_input_port/1]).
-export([find_device_by_port/2]).
-export([connect/2]).
-export([io_prog/1, input_prog/2]).


-include("midi.hrl").

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(midi), "midi_drv"),
    erlang:load_nif(Nif, 0).

%% open midi device
open(_DeviceName, _OptionList) ->
    ?nif_stub().

% close midi device
close(_Synth) ->
    ?nif_stub().

write(_Synth, _Data) ->
    ?nif_stub().

read(_Synth) ->
    ?nif_stub().

note_on(Synth, Chan, Note, Velocity) ->
    write(Synth, <<?MIDI_NOTE_ON(Chan), Note, Velocity>>).

note_off(Synth, Chan, Note, Velocity) ->
    write(Synth, <<?MIDI_NOTE_OFF(Chan), Note, Velocity>>).

control(Synth, Chan, Control, Arg) ->
    write(Synth, <<?MIDI_CONTROL_CHANGE(Chan), Control, Arg>>).

volume(Synth, Chan, Volume) ->
    control(Synth, Chan, ?MIDI_CTRL_VOLUME, Volume).

all_off(Synth, Chan) ->
    control(Synth, Chan, ?MIDI_CTRL_ALL_NOTES_OFF, 0).

reset_all(Synth, Chan) ->
    control(Synth, Chan, ?MIDI_CTRL_ALL_CONTROLLERS_OFF, 0).

bank_msb(Synth, Chan, Bank) ->
    control(Synth, Chan, ?MIDI_CTRL_BANK_SELECT, Bank).

program_change(Synth, Chan, Prog) ->
    write(Synth, <<?MIDI_PROGRAM_CHANGE(Chan), Prog>>).

io_prog(DeviceName) ->
    input_prog(DeviceName, 
	       fun(_Fd,Event) ->
		       io:format("~p\n", [Event])
	       end).

input_prog(DeviceName, CallBack) ->
    case open(DeviceName,[event,list]) of
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

open_synth() ->
    case setup_synth() of
	{ok,#{ device:=Dev}} ->
	    open(Dev, []);
	Error -> Error
    end.

%% try start fluid synth as server 
setup_synth() ->
    Ds = devices(),
    case find_fluid_port(Ds) of
	false ->
	    start_fluid_synth(),
	    timer:sleep(1000), %% allow device to be created
	    Ds1 = devices(),
	    case find_fluid_port(Ds1) of
		false ->
		    {error, fluid_synth_not_started};
		Fluid -> setup_fluid(Fluid,Ds1)
	    end;
	Fluid ->
	    setup_fluid(Fluid,Ds)
    end.

setup_fluid(Fluid,Ds) ->
    case find_fluid_input_port(Ds) of
	false ->
	    setup_input(Fluid,Ds);
	Input -> {ok,Input}
    end.

setup_input(Fluid,Ds) ->
    case find_free_virtual_port(Ds) of
	false ->
	    io:format("forgot to install the virmid? run setup.sh\n"),
	    {error, no_ports_available};
	Virt ->
	    case connect(maps:get(port,Virt), maps:get(port,Fluid)) of
		"" -> {ok,Virt};
		Err -> {error,Err}
	    end
    end.

start_fluid_synth() ->
    spawn(
      fun() ->
	      Port = open_port({spawn,"fluidsynth --server --audio-driver=alsa -o audio.alsa.device=hw:0 /usr/share/sounds/sf2/FluidR3_GM.sf2"},
			       [eof,exit_status]),
	      port_loop(Port)
      end).

port_loop(Port) ->
    receive
	{Port,eof} ->
	    io:format("fluidsynth closed\n", []),
	    ok;
	{Port,What} ->
	    io:format(">~p\n", [What]),
	    port_loop(Port)
    end.

%% Find available virtual midi port (to connect to fluid synth)
find_free_virtual_port([D=#{port_name := "VirMIDI "++_, output := []}|_Ds]) ->
    D;
find_free_virtual_port([_|Ds]) ->
    find_free_virtual_port(Ds);
find_free_virtual_port([]) ->
    false.

%% Lookup Device given a port 
find_device_by_port(Port, [D=#{port:=Port}|_Ds]) ->
    D;
find_device_by_port(Port, [_|Ds]) ->
    find_device_by_port(Port, Ds);
find_device_by_port(_Port, []) ->
    false.

%% Locate the fluid synth input port
find_fluid_input_port(Ds) ->
    case find_fluid_port(Ds) of
	false -> false;
	Fluid -> find_input_port(maps:get(port,Fluid),Ds)
    end.

%% Locate the input port/device 
find_input_port(Port,[D=#{output:=OUT}|Ds]) ->
    case lists:member(Port, OUT) of
	true -> D;
	false -> find_input_port(Port,Ds)
    end;
find_input_port(_Port,[]) ->
    false.

%% find fluid synth port (device)
find_fluid_port([D=#{client_name := "FLUID"++_}|_Ds]) ->
    D;
find_fluid_port([_|Ds]) ->
    find_fluid_port(Ds);
find_fluid_port([]) ->
    false.

connect(_Port1={Cli1,Con1}, _Port2={Cli2,Con2}) ->
    os:cmd("aconnect "++
	       integer_to_list(Cli1)++":"++integer_to_list(Con1)++
	       " "++
	       integer_to_list(Cli2)++":"++integer_to_list(Con2)).

%% maybe parse /proc/asound/seq/clients instead? 
%% or write a driver?
devices() ->
    LIST = os:cmd("aconnect -l"),
    Lines = string:tokens(LIST, "\n"),
    DevMidi = devmidi(),
    dev_parse(Lines,undefined,undefined,[],DevMidi).

%% list the /dev/midixx devices and lookup the ID
devmidi() ->
    lists:foldl(
      fun(Dev,Acc) ->
	      B = filename:basename(Dev),
	      F = filename:join(["/sys/class/sound",B,"device","id"]),
	      case file:read_file(F) of
		  {ok,ID} ->
		      [{string:trim(binary_to_list(ID)),Dev}|Acc];
		  _ ->
		      Acc
	      end
      end, [], filelib:wildcard("/dev/midi*")).


dev_parse(["client "++Data|Lines],_Client,Node,Acc,DevMidi) ->
    {ok,[{integer,_,Cli},{':',_},
	 {atom,_,Name},
	 {'[',_},{atom,_,type},{'=',_},
	 {atom,_,Type},{']',_}],_} = erl_scan:string(Data),
    Name1 = string:trim(atom_to_list(Name)),
    Client = #{ client => Cli, client_name => Name1, type => Type },
    dev_parse(Lines,Client,undefined,add_node(Node,Acc,DevMidi),DevMidi);

dev_parse(["    "++Data|Lines],Client=#{ client := Cli},Node,Acc,DevMidi) ->
    {ok,[{integer,_,Con},{atom,_,ConName}],_} = erl_scan:string(Data),
    Name1 = string:trim(atom_to_list(ConName)),
    Node1 = Client#{ port => {Cli,Con}, port_name => Name1,
		     input => [], output => []},
    dev_parse(Lines,Client,Node1,add_node(Node,Acc,DevMidi),DevMidi);

dev_parse(["\tConnected From: "++String|Lines],Client,Node=#{input:=IN},Acc,DevMidi) ->
    {ok,[{integer,_,Cli},{':',_},{integer,_,Con}],_} = erl_scan:string(String),
    Node1 = Node#{ input => [{Cli,Con}|IN]},
    dev_parse(Lines,Client,Node1,Acc,DevMidi);

dev_parse(["\tConnecting To: "++String|Lines],Client,Node=#{output:=OUT},Acc,DevMidi) ->
    {ok,[{integer,_,Cli},{':',_},{integer,_,Con}],_} = erl_scan:string(String),
    Node1 = Node#{ output => [{Cli,Con}|OUT]},
    dev_parse(Lines,Client,Node1,Acc,DevMidi);

dev_parse([],_Client,Node,Acc,DevMidi) ->
    add_node(Node,Acc,DevMidi).

add_node(undefined,Acc,_DevMidi) -> Acc;
add_node(Node,Acc,DevMidi) -> 
    %% find real device name if any
    Node1 = 
	case maps:get(port_name, Node) of
	    Name="VirMIDI "++_ ->
		case io_lib:fread("VirMIDI ~d-~d", Name) of
		    {ok,[C,D],""} ->
			Dev = "/dev/snd/midi"++"C"++integer_to_list(C)++
			    "D"++integer_to_list(D),
			Node#{ device => Dev };
		    _ ->
			Node
		end;
	    Name ->
		case port_name_to_device_name(Name, DevMidi) of
		    false ->
			Node;
		    Dev ->
			Node#{ device => Dev }
		end
	end,
    [Node1|Acc].

port_name_to_device_name(PortName, [{ID,Dev}|Ds]) ->
    case string:prefix(PortName,ID) of
	nomatch -> port_name_to_device_name(PortName,Ds);
	_Rest -> Dev
    end;
port_name_to_device_name(_, []) ->
    false.
