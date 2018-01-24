%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    ALSA specfic commands
%%% @end
%%% Created : 16 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_alsa).

-export([name/0]).
-export([audio_device/0]).
-export([devices/0]).
-export([devmidi/0]).
-export([connect/2]).
-export([disconnect/2]).
-export([find_free_virtual_port/1]).

name() -> %% backend name (to fluid synth)
    "alsa".

audio_device() ->  %% audio device to used (fixme, card config?)
    "hw:0".

connect(A, B) ->
    connect_("", A, B).

disconnect(A, B) ->
    connect_("-d", A, B).

connect_(Flag, Sender, Receiver) ->
    {Cli1,Con1} = maps:get(port,Sender),
    {Cli2,Con2} = maps:get(port,Receiver),
    os:cmd("aconnect "++Flag++" "++
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
			Node#{ device => Dev,
			       hw => "hw:"++integer_to_list(C)++"," ++
				   integer_to_list(D)
			     };
		    _ ->
			Node
		end;
	    Name ->
		case port_name_to_device_name(Name, DevMidi) of
		    false ->
			Node;
		    Dev ->
			case Dev of  %% FIXME
			    "/dev/midi"++N ->
				Node#{ device => Dev,
				       hw => "hw:"++N++",0,0" };
			    _ ->
				Node#{ device => Dev }
			end
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

%% Find available virtual midi port, maybe not so picky?
find_free_virtual_port([D=#{port_name := "VirMIDI "++_, output := []}|_Ds]) ->
    D;
find_free_virtual_port([_|Ds]) ->
    find_free_virtual_port(Ds);
find_free_virtual_port([]) ->
    false.
