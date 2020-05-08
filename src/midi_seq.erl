%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%      MIDI loop sequencer
%%% @end
%%% Created : 13 May 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_seq).

-export([start/0, start/1]).
-export([add/2, set/2, clear/1, stop/1]).

-export([test/0]).

-include("../include/midi.hrl").


-define(USEC_PER_MINUTE, 60000000).
-define(DEFAULT_MPQN,    500000).

-define(CLICK_CHAN, 9).
-define(CLICK_NOTE, ?GM_DRUM_Closed_Hi_hat).

-record(tparam,
	{
	  ppqn = 0.0 :: float(),  %% pulses per quarter note
	  mpqn = 0   :: number(), %% micro seconds per quarter note
	  bpm  = 0.0 :: float(),  %% beats per minute (USEC_PER_MINUTE/MPQN)
	  uspp = 1.0 :: float(),  %% Micro seconds per pulse (MPQN/PPQN)
	  sig  = {4,4} :: {integer(),integer()}, %% time signature
	  cc   = 0     :: integer(), %% # MIDI clocks in a metronome click
	  bb   = 0     :: integer()  %% # 32nd-notes in a MIDI quarter-note
	}).

test() ->
    midi:start(),
    start([{device,"LPK25"},{division,4}]).

start() -> start([]).
start(Opts) ->
    spawn(fun() -> init(Opts) end).

add(Pid,Es) when is_pid(Pid), is_list(Es) ->
    Pid ! {add_events,Es}.

set(Pid,Es) when is_pid(Pid), is_list(Es) ->
    Pid ! {set_events,Es}.

clear(Pid) when is_pid(Pid) ->
    Pid ! clear.

stop(Pid) ->
    Pid ! stop.

init(Opts) ->
    Out = synth,  %% default output synth
    IN = open_input(Opts),
    BPM = proplists:get_value(bpm,Opts,?USEC_PER_MINUTE/?DEFAULT_MPQN),
    Bank = proplists:get_value(bank,Opts,0),
    Division = proplists:get_value(division,Opts,1.0),
    midi:reset_all(Out, 0),
    if Bank > 0 ->
	    lists:foreach(
	      fun(I) -> midi:bank(Out, I, Bank) end, lists:seq(0, 15));
       true ->
	    ok
    end,
    midi:program_change(Out, 0, ?GM_MIDI_Acoustic_Grand_Piano),
    midi:program_change(Out, 1, ?GM_MIDI_Acoustic_Guitar_nylon),
    midi:program_change(Out, 2, ?GM_MIDI_Acoustic_Bass),

    MPQN = ?USEC_PER_MINUTE/BPM,
    PPQN = if Division >= 0 -> Division; true -> 1.0 end,
    USPP =
	if Division >= 0 ->
		MPQN / PPQN;
	   true ->
		TicksPerFrame = Division band 16#ff,
		FramesPerSec = 
		    case Division bsr 8 of
			-24 -> 24.0;
			-25 -> 25.0;
			-29 -> 29.97;
			-30 -> 30.0
		    end,
		?DEFAULT_MPQN / (FramesPerSec*TicksPerFrame)
	end,
    TParam = #tparam{mpqn=MPQN,ppqn=PPQN,bpm=BPM,uspp=USPP},
    io:format("TParam ppqn=~w,uspp=~w\n", 
	      [TParam#tparam.ppqn,TParam#tparam.uspp]),
    PPL = trunc(PPQN * 4 * 4),
    Loop = erlang:make_tuple(PPL, []),
    Status = flush_midi_in(IN),
    loop(Out,IN,Status,1,Loop,1,time_us(),TParam).

%% Idea: maybe tick double speed so that we can sort in events
%% can be inserted in the correct slot instead of next slot, this
%% affects slower speeds alot.

loop(OUT,IN,Status,I,Loop,Ticks,TimeUs,TParam) ->
    WaitUs = Ticks*TParam#tparam.uspp,
    NextUs = trunc(TimeUs + WaitUs),
    End = wait_until(NextUs),
    %%io:format("waitus: ~w, nextus: ~w, end: ~w\n",[trunc(WaitUs),NextUs,End]),
    if End - NextUs > 1200 -> io:format("td = ~w\n", [End-NextUs]);
       true -> ok
    end,
    update(OUT,IN,Status,I,Loop,Ticks+1,TimeUs,TParam).

update(OUT,IN,Status,I,Loop,Ticks,TimeUs,TParam) ->
    INext = if I >= tuple_size(Loop) -> 1; true -> I + 1 end,
    Es0 = case (I-1) rem TParam#tparam.ppqn of
	      0 -> [{note_on,?CLICK_CHAN,?CLICK_NOTE,127}];
	      1 -> [{note_off,?CLICK_CHAN,?CLICK_NOTE,127}];
	      _ -> []
	  end,
    receive
	{midi,IN,E} ->
	    case Status of
		{ok,N} ->
		    Es1 = read_midi_in(N-1,IN,[E]),
		    send(I, OUT,element(I,Loop) ++ Es1 ++ Es0),
		    Status1 = midi:read(IN),
		    Loop1 = setelement(I,Loop,Es1),
		    loop(OUT,IN,Status1,INext,Loop1,Ticks,TimeUs,TParam);
		select ->
		    send(I, OUT, element(I,Loop)++Es0),
		    loop(OUT,IN,Status,INext,Loop,Ticks,TimeUs,TParam)
	    end;
	{select,IN,undefined,ready_input} ->
	    {ok,N} = midi:read(IN),
	    Es1 = read_midi_in(N,IN,[]),
	    send(I, OUT,element(I,Loop)++Es1++Es0),
	    Status1 = midi:read(IN),
	    Loop1 = setelement(I,Loop,Es1),
	    loop(OUT,IN,Status1,INext,Loop1,Ticks,TimeUs,TParam);
	{add_events,NEs} ->
	    %% fixme: join and clean
	    Es1 = element(I,Loop) ++ NEs,
	    send(I,OUT,Es1++Es0),
	    Loop1 = setelement(I,Loop,Es1),
	    loop(OUT,IN,Status,INext,Loop1,Ticks,TimeUs,TParam);
	{set_events,Es1} ->
	    send(I,OUT,Es1++Es0),
	    Loop1 = setelement(I,Loop,Es1),
	    loop(OUT,IN,Status,INext,Loop1,Ticks,TimeUs,TParam);
	clear ->
	    %% fixme: send note_off on all active channels
	    Loop1 = erlang:make_tuple(tuple_size(Loop), []),
	    midi:reset_all(OUT, 0),
	    loop(OUT,IN,Status,INext,Loop1,Ticks,TimeUs,TParam);
	stop ->
	    midi:reset_all(OUT, 0),
	    ok
    after 0 ->
	    send(I,OUT,element(I,Loop)++Es0),
	    loop(OUT,IN,Status,INext,Loop,Ticks,TimeUs,TParam)
    end.

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
	    read_midi_in(N,IN,[]),
	    flush_midi_in(IN)
    end.

flush_midi_events(IN) ->
    flush_midi_events(IN,0).
flush_midi_events(IN,I) ->
    receive
	{midi,IN,_} ->
	    flush_midi_events(IN,I+1)
    after 0 ->
	    I
    end.

%% read N events
read_midi_in(0,_IN,Acc) -> 
    lists:reverse(Acc);
read_midi_in(I,IN,Acc) ->
    receive
	{midi,IN,Event} ->
	    read_midi_in(I-1,IN,[Event|Acc])
    end.

send(I, Out, Events) ->
    io:format("~w: ~w\n", [I, Events]),
    send(Out, Events).

send(_OUT, []) ->
    ok;
send(OUT, [E|Events]) ->
    Bytes = midi_codec:event_encode(E),
    ok = midi:write(OUT, Bytes),
    send(OUT, Events).

open_input(Opts) ->
    case proplists:get_value(device, Opts) of
	undefined -> undefined;
	Name = "/"++_ ->
	    case midi:open(Name,[event,list,running]) of
		{ok,Fd} -> Fd;
		{error,Reason} ->
		    io:format("warnig: unable to open input ~p\n",
			      [Reason]),
		    undefined
	    end;
	Name ->
	    case midi:find_device_by_name(Name, midi:devices()) of
		false -> undefined;
		#{device := Device } ->
		    case midi:open(Device,[event,list,running]) of
			{ok,Fd} -> Fd;
			{error,Reason} ->
			    io:format("warnig: unable to open input ~p\n",
				      [Reason]),
			    undefined
		    end
	    end
    end.

time_us() ->
    %%erlang:system_time(micro_seconds).
    erlang:monotonic_time(micro_seconds).

wait_until(End) ->
    Now = time_us(),
    if Now < End ->
	    Delta = End - Now,
	    if Delta >= 2000 ->
		    timer:sleep((Delta-1000) div 1000),
		    wait_until(End);
	       true ->
		    spin_until(End)
	    end;
       true ->
	    Now
    end.

%% faster spin, only use yield to be polite
spin_until(End) ->
    Now = time_us(),
    if Now < End -> 
	    erlang:yield(),
	    spin_until(End);
       true ->
	    Now
    end.
