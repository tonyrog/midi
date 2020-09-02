%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    MIDI recorder
%%% @end
%%% Created : 30 Aug 2020 by Tony Rogvall <tony@rogvall.se>

-module(midi_rec).

-export([start/0, start/1]).
-export([lpk25/0, vmpk/0]).

-include("../include/midi.hrl").

lpk25() ->
    start([{device,"LPK25"},{division,32}]).

vmpk() ->
    start([{device,"VMPK Output"},{division,32}]).

start() -> 
    start([{division,32}]).
start(Opts) ->
    midi:start(),
    spawn(fun() -> init(maps:from_list(Opts)) end).

init(Opts) when is_map(Opts) ->
    %% setup synth
    IN = open_input(Opts),
    BPM = maps:get(bpm, Opts, ?USEC_PER_MINUTE/?DEFAULT_MPQN),
    Division = maps:get(division,Opts,1),
    TParam = midi:tparam(BPM, Division),
    io:format("tparam = ~p\n", [TParam]),
    Status = flush_midi_in(IN),
    USPP = round(TParam#tparam.uspp),
    io:format("running uspp=~w\n", [USPP]),
    {ok,Fd} = file:open("rec.mid", [write,raw,binary]),
    ok = midi_file:write_tune(Fd, _Format=0, _NumTracks=1, Division),
    ok = midi_file:write_track(Fd, <<>>),
    {ok,Pos} = file:position(Fd, cur),   %% current start of data
    io:format("pos = ~w\n", [Pos]),
    loop(IN, Status, USPP, Fd, Pos, false).

loop(IN,Status,USPP,Fd,Pos,Flush) ->
    receive
	{midi,IN,Event,Delta} ->
	    case Status of
		{ok,N} ->
		    D = Delta div USPP,
		    EventList = read_midi_in(N-1,IN,USPP,[{D,Event}]),
		    output(Fd,EventList),
		    Status1 = midi:read(IN),
		    loop(IN,Status1,USPP,Fd,Pos,true);
		select ->
		    D = Delta div USPP,
		    EventList = [{D,Event}],
		    output(Fd,EventList),
		    loop(IN,Status,USPP,Fd,Pos,true)
	    end;

	{select,IN,undefined,ready_input} ->
	    {ok,N} = midi:read(IN),
	    EventList = read_midi_in(N,IN,USPP,[]),
	    output(Fd,EventList),
	    Status1 = midi:read(IN),
	    loop(IN,Status1,USPP,Fd,Pos,true)

    after 2000 ->
	    if Flush ->
		    {ok,Cur} = file:position(Fd, cur),
		    output(Fd,[{1000,{meta,end_of_track,[]}}]),
		    {ok,End} = file:position(Fd, cur),
		    Length = End - Pos,
		    io:format("length = ~p\n", [Length]),
		    file:pwrite(Fd, Pos-4, <<Length:32>>),
		    {ok,_} = file:position(Fd, Cur),
		    file:sync(Fd),
		    loop(IN,Status,USPP,Fd,Pos,false);
	       true ->
		    io:format("not moved\n"),
		    loop(IN,Status,USPP,Fd,Pos,false)
	    end
    end.

%%
%% Next step is to store events and timestamps in 
%% logfiles (eloge?) to be able to replay various 
%% parts of jam sessions and impros during several 
%% days/weeks maybe years... perhaps use a key 
%% (any key) to mark a time position in the data
%% The keys (searched for) could be high lighted to
%% be able to scroll for data in a fast way.
%%
%% A web server could be used to serv as a web interface
%% for finding and replaying the varous parts, either 
%% send to web client a MIDI file, or start playing on 
%% current synth device.
%%
output(Fd, [{Delta,E1}|Es]) ->
    {Es0, Es1} = lists:splitwith(fun({Di,_}) -> Di =:= 0 end, Es),
    EventList = [E1 | [E || {_,E} <- Es0]],
    io:format("outp: d=~w, ~p\n", [Delta, EventList]),
    Data = midi_codec:events_encode(EventList),
    file:write(Fd, [midi_codec:length_encode(Delta), Data]),
    output(Fd, Es1);
output(_Fd, []) ->
    ok.

%% flush until select
flush_midi_in(undefined) ->
    undefined;
flush_midi_in(IN) ->
    flush_midi_events(IN),
    flush_midi_in_(IN).

flush_midi_in_(IN) ->
    case midi:read(IN) of
	select -> select;
	{ok,N} ->
	    read_midi_in(N,IN,1,[]),
	    flush_midi_in(IN)
    end.

flush_midi_events(IN) ->
    flush_midi_events(IN,0).
flush_midi_events(IN,I) ->
    receive
	{midi,IN,_Event,_Delta} ->
	    flush_midi_events(IN,I+1)
    after 0 ->
	    I
    end.

%% read N events
read_midi_in(0,_IN,_USPP,Acc) -> 
    lists:reverse(Acc);
read_midi_in(I,IN,USPP,Acc) ->
    receive
	{midi,IN,Event,Delta} ->
	    D = Delta div USPP,
	    read_midi_in(I-1,IN,USPP,[{D,Event}|Acc])
    end.

%% A number of posibilities
%% - device is device name, then it starts with a slash
%% - it is a device with a device name that can be used directly
%% - it is a virtual device already connected to a virtual midi
%% - it is a virtual device that needs to be connected to a virtual midi
%%   that we need to find and connect
open_input(Opts) ->
    case maps:get(device, Opts, undefined) of
	undefined -> undefined;
	DeviceName = "/"++_ ->
	    open_device(DeviceName);
	Name ->
	    case midi:shared_input(Name) of
		#{device := DeviceName } ->
		    open_device(DeviceName);
		Error = {error,_} ->
		    Error
	    end
    end.

open_device(DeviceName) ->
    case midi:open(DeviceName,
		   [event,list,running,timestamp]) of
	{ok,Fd} -> Fd;
	Error = {error,Reason} ->
	    io:format("warnig: unable to open device ~s for input: ~p\n",
		      [DeviceName, Reason]),
	    Error
    end.
