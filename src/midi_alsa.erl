%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    ALSA specfic commands
%%% @end
%%% Created : 16 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_alsa).

-export([devices/0]).
-export([devmidi/0]).
-export([connect/2]).
-export([disconnect/2]).
-export([find_free_virtual_port/1]).

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
    case erl_scan:string(Data) of
	{ok,[{integer,_,Cli},{':',_},
	     {atom,_,Name},{'[',_},
	     {atom,_,type},{'=',_},{atom,_,Type},
	     {']',_}],_} ->
	    Name1 = string:trim(atom_to_list(Name)),
	    Client = #{ client => Cli, client_name => Name1, type => Type },
	    dev_parse(Lines,Client,undefined,
		      add_node(Node,Acc,DevMidi),DevMidi);
	{ok,[{integer,_,Cli},{':',_},
	     {atom,_,Name},
	     {'[',_},
	     {atom,_,type},{'=',_},{atom,_,Type},
	     {',',_},
	     {atom,_,pid},{'=',_},{integer,_,Pid},
	     {']',_}],_} ->
	    Name1 = string:trim(atom_to_list(Name)),
	    Client = #{ client => Cli, client_name => Name1, type => Type, 
			pid => Pid },
	    dev_parse(Lines,Client,undefined,
		      add_node(Node,Acc,DevMidi),DevMidi);
	{ok,[{integer,_,Cli},{':',_},
	     {atom,_,Name},
	     {'[',_},
	     {atom,_,type},{'=',_},{atom,_,Type},
	     {',',_},
	     {atom,_,card},{'=',_},{integer,_,Card},
	     {']',_}],_} ->
	    Name1 = string:trim(atom_to_list(Name)),
	    Client = #{ client => Cli, client_name => Name1, type => Type, 
			card => Card },
	    dev_parse(Lines,Client,undefined,
		      add_node(Node,Acc,DevMidi),DevMidi)
    end;
dev_parse(["    "++Data|Lines],Client=#{ client := Cli},Node,Acc,DevMidi) ->
    case erl_scan:string(Data) of
	{ok,[{integer,_,Con},{atom,_,ConName}],_} ->
	    Name1 = string:trim(atom_to_list(ConName)),
	    Node1 = Client#{ port => {Cli,Con}, 
			     port_name => Name1,
			     input => [], output => []},
	    dev_parse(Lines,Client,Node1,add_node(Node,Acc,DevMidi),DevMidi)
    end;
dev_parse(["\tConnected From: "++String|Lines],Client,Node=#{input:=IN},Acc,DevMidi) ->
    case erl_scan:string(String) of
	{ok, Ts, _} ->
	    IN1 = parse_connect_from(Ts, IN),
	    Node1 = Node#{ input => IN1 },
	    dev_parse(Lines,Client,Node1,Acc,DevMidi)
%%	{ok,[{integer,_,Cli},{':',_},{integer,_,Con}],_} ->
%%	    Node1 = Node#{ input => [{Cli,Con}|IN]},
%%	    dev_parse(Lines,Client,Node1,Acc,DevMidi);
%%	{ok,[{integer,_,Cli},{':',_},{integer,_,Con},
%%	     {'[',_},{atom,_,real},{':',_},{integer,_,_Hw},{']',_}],_} ->
%%	    Node1 = Node#{ input => [{Cli,Con}|IN]},
%%	    dev_parse(Lines,Client,Node1,Acc,DevMidi);
%%	{ok,[{integer,_,Cli1},{':',_},{integer,_,Con1},{',',_},
%%	     {integer,_,Cli2},{':',_},{integer,_,Con2},
%%	     {'[',_},{atom,_,real},{':',_},{integer,_,_Hw},{']',_}],_} ->
%%	    Node1 = Node#{ input => [{Cli1,Con1},{Cli2,Con2}|IN]},
%%	    dev_parse(Lines,Client,Node1,Acc,DevMidi)
    end;
dev_parse(["\tConnecting To: "++String|Lines],Client,Node=#{output:=OUT},Acc,DevMidi) ->
    case erl_scan:string(String) of
	{ok,Ts,_} ->
	    OUT1 = parse_connecting_to(Ts, OUT),
	    Node1 = Node#{ output => OUT1},
	    dev_parse(Lines,Client,Node1,Acc,DevMidi)
%%	{ok,[{integer,_,Cli},{':',_},{integer,_,Con}],_} ->
%%	    Node1 = Node#{ output => [{Cli,Con}|OUT]},
%%	    dev_parse(Lines,Client,Node1,Acc,DevMidi);
%%	{ok,[{integer,_,Cli},{':',_},{integer,_,Con},
%%	     {'[',_},{atom,_,real},{':',_},{integer,_,_Hw},{']',_}],_} ->
%%	    Node1 = Node#{ output => [{Cli,Con}|OUT]},
%%	    dev_parse(Lines,Client,Node1,Acc,DevMidi)
    end;

dev_parse([],_Client,Node,Acc,DevMidi) ->
    add_node(Node,Acc,DevMidi).

%% parse connections:  <cli>':'<con> [,<cli>':'<con>]* ['['real':'<hw>']']
parse_connect_from([{integer,_,Cli},{':',_},{integer,_,Con}|Ts], Acc) ->
    parse_connect_from(Ts, [{Cli,Con}|Acc]);
parse_connect_from([{',',_}|Ts], Acc) ->
    parse_connect_from(Ts, Acc);
parse_connect_from([], Acc) ->
    Acc;
parse_connect_from([{'[',_},{atom,_,real},{':',_},
		    {integer,_,_Hw},{']',_}], Acc) ->
    Acc.

%% parse: Connecting To:  <cli>':'<con> [,<cli>':'<con>]* ['['real':'<hw>']']
parse_connecting_to([{integer,_,Cli},{':',_},{integer,_,Con}|Ts], Acc) ->
    parse_connecting_to(Ts, [{Cli,Con}|Acc]);
parse_connecting_to([{',',_}|Ts], Acc) ->
    parse_connecting_to(Ts, Acc);
parse_connecting_to([], Acc) ->
    Acc;
parse_connecting_to([{'[',_},{atom,_,real},{':',_},
		     {integer,_,_Hw},{']',_}], Acc) ->
    Acc.






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
