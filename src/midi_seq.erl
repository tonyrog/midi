%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%      MIDI loop sequencer
%%% @end
%%% Created : 13 May 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_seq).

-export([start/0, start/1]).
-export([add/2, set/2, clear/1, stop/1]).

-export([lpk25/0, vmpk/0]).

-include("../include/midi.hrl").

-define(USEC_PER_MINUTE, 60000000).
-define(DEFAULT_MPQN,    500000).

-define(CLICK_CHAN, 9).
-define(CLICK_NOTE, ?GM_DRUM_Closed_Hi_hat).

-define(TICK_NEXT(I,N), (((((I)+1)-1) rem (N))+1)).

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

-record(wparam,
	{
	 width  :: integer(),
	 height :: integer(),
	 window :: epx:window(),
	 screen :: epx:pixmap()
	}).
	 
lpk25() ->
    start([{device,"LPK25"},{division,4}]).

vmpk() ->
    start([{device,"VMPK Output"},{division,4}]).

start() -> 
    start([{division,4}]).
start(Opts) ->
    epx:start(),
    midi:start(),
    spawn(fun() -> init(maps:from_list(Opts)) end).

add(Pid,Es) when is_pid(Pid), is_list(Es) ->
    Pid ! {add_events,Es}.

set(Pid,Es) when is_pid(Pid), is_list(Es) ->
    Pid ! {set_events,Es}.

clear(Pid) when is_pid(Pid) ->
    Pid ! clear.

stop(Pid) ->
    Pid ! stop.


init(Opts) when is_map(Opts) ->
    %% setup input/output window
    W  = maps:get(width, Opts, 640),
    H = maps:get(height, Opts, 480),
    %% button_press,button_release
    Window = epx:window_create(50,50,W,H,
			       [key_press,key_release,no_auto_repeat]),
    epx:window_attach(Window),
    Screen = epx:pixmap_create(W,H,argb),
    epx:pixmap_fill(Screen, white),
    epx:pixmap_attach(Screen),
    WParam = #wparam { window=Window, screen=Screen, width=W, height=H},
    %% setup synth
    Out = synth,  %% default output synth
    IN = open_input(Opts),
    BPM = maps:get(bpm, Opts, ?USEC_PER_MINUTE/?DEFAULT_MPQN),
    Bank = maps:get(bank,Opts,0),
    Division = maps:get(division,Opts,1.0),
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
    Bars = 4,
    PPL = trunc(PPQN * 4 * Bars),
    draw_grid(PPQN, Bars, WParam),
    Loop = erlang:make_tuple(PPL, #{}),
    Status = flush_midi_in(IN),
    loop(Out,IN,Status,1,Loop,1,time_us(),TParam,WParam,#{}).

%% Idea: maybe tick double speed so that we can sort in events
%% can be inserted in the correct slot instead of next slot, this
%% affects slower speeds alot.

loop(OUT,IN,Status,I,Loop,Ticks,TimeUs,TParam,WParam,Active) ->
    WaitUs = Ticks*TParam#tparam.uspp,
    NextUs = trunc(TimeUs + WaitUs),
    draw(WParam),
    End = wait_until(NextUs),
    %%io:format("waitus: ~w, nextus: ~w, end: ~w\n",[trunc(WaitUs),NextUs,End]),
    if End - NextUs > 1200 -> io:format("td = ~w\n", [End-NextUs]);
       true -> ok
    end,
    update(OUT,IN,Status,I,Loop,Ticks+1,TimeUs,TParam,WParam,Active).

update(OUT,IN,Status,I,Loop,Ticks,TimeUs,TParam,WParam,Active) ->
    receive
	{midi,IN,E} ->
	    Es1 = collect_events(WParam#wparam.window,[]),
	    case Status of
		{ok,N} ->
		    Es2 = read_midi_in(N-1,IN,[E|Es1]),
		    Status1 = midi:read(IN),
		    next(Es2,OUT,IN,Status1,I,Loop,Ticks,TimeUs,
			 TParam,WParam,Active);
		select ->
		    next(Es1,OUT,IN,Status,I,Loop,Ticks,TimeUs,
			 TParam,WParam,Active)
	    end;
	{select,IN,undefined,ready_input} ->
	    Es1 = collect_events(WParam#wparam.window,[]),
	    {ok,N} = midi:read(IN),
	    Es2 = read_midi_in(N,IN,Es1),
	    Status1 = midi:read(IN),
	    next(Es2,OUT,IN,Status1,I,Loop,Ticks,TimeUs,
		 TParam,WParam,Active);
	{add_notes,Notes} ->
	    Loop1 = add_notes(Notes, Loop),
	    next([],OUT,IN,Status,I,Loop1,Ticks,TimeUs,
		 TParam,WParam,Active);
	clear ->
	    %% fixme: send note_off on all active channels
	    Loop1 = erlang:make_tuple(tuple_size(Loop), #{}),
	    midi:reset_all(OUT, 0),
	    next([],OUT,IN,Status,I,Loop1,Ticks,TimeUs,
		 TParam,WParam,Active);

	{epx_event,Window,{key_press, Sym, _Mod, _Code}}
	  when Window =:= WParam#wparam.window ->
	    Es1 = case make_key_note(Sym, #note_on{}) of
		      false -> [];
		      N -> [N]
		  end,
	    Es2 = collect_events(WParam#wparam.window,Es1),
	    next(Es2,OUT,IN,Status,I,Loop,Ticks,TimeUs,
		 TParam,WParam,Active);

	{epx_event,Window,{key_release, Sym, _Mod, _Code}} 
	  when Window =:= WParam#wparam.window ->
	    Es1 = case make_key_note(Sym, #note_off{}) of
		      false -> [];
		      N -> [N]
		  end,
	    Es2 = collect_events(WParam#wparam.window,Es1),
	    next(Es2,OUT,IN,Status,I,Loop,Ticks,TimeUs,
		 TParam,WParam,Active);

	{epx_event,Window,close} when Window =:= WParam#wparam.window ->
	    epx:window_detach(WParam#wparam.window),
	    epx:pixmap_detach(WParam#wparam.screen),
	    midi:reset_all(OUT, 0),
	    ok;
	stop ->
	    epx:window_detach(WParam#wparam.window),
	    epx:pixmap_detach(WParam#wparam.screen),
	    midi:reset_all(OUT, 0),
	    ok
    after 0 ->
	    next([],OUT,IN,Status,I,Loop,Ticks,TimeUs,
		 TParam,WParam,Active)
    end.

next([],OUT,IN,Status,I,Loop,Ticks,TimeUs,TParam,WParam,Active) ->
    PPL = tuple_size(Loop),
    INext = if I >= PPL -> 1; true -> I + 1 end,
    Click = case (I-1) rem trunc(TParam#tparam.ppqn) of
		0 -> [{note_on,?CLICK_CHAN,?CLICK_NOTE,127}];
		1 -> [{note_off,?CLICK_CHAN,?CLICK_NOTE,127}];
		_ -> []
	    end,
    Es = get_midi_events(I, Loop),
    send(I,OUT,Click++Es),
    {Active1,Deactive1} = active_events(Es, I, Active,[]),
    draw_active(Active1,Deactive1,I,PPL,WParam),
    loop(OUT,IN,Status,INext,Loop,Ticks,TimeUs,TParam,WParam,Active1);
next(Events,OUT,IN,Status,I,Loop,Ticks,TimeUs,TParam,WParam,Active) ->
    PPL = tuple_size(Loop),
    INext = if I >= PPL -> 1; true -> I + 1 end,
    Click = case (I-1) rem trunc(TParam#tparam.ppqn) of
		0 -> [{note_on,?CLICK_CHAN,?CLICK_NOTE,127}];
		1 -> [{note_off,?CLICK_CHAN,?CLICK_NOTE,127}];
		_ -> []
	    end,
    Es = get_midi_events(I, Loop),
    send(I,OUT,Click++Events++Es),
    {Active1,Deactive1} = active_events(Es, I, Active,[]),
    draw_notes(I, Events, WParam),
    {Loop1,Active2,Deactive2}=put_midi_events(Events,I,Loop,Active1,Deactive1),
    draw_active(Active2, Deactive2, I, PPL, WParam),
    loop(OUT,IN,Status,INext,Loop1,Ticks,TimeUs,TParam,WParam,Active2).

%% set notes {note,Chan,Note,Velocity,Start,Length}
add_notes([{note,Chan,Note,Velocity,Start,Length} | Notes], Loop) ->
    I = ((Start-1) rem tuple_size(Loop))+1,
    J = ((Start+Length-1) rem tuple_size(Loop))+1,
    On = maps:put({on,Chan,Note},{I,J,Velocity}, element(I, Loop)),
    Off = maps:put({off,Chan,Note},{I,J,Velocity}, element(J, Loop)),
    Loop1 = setelement(I, Loop, On),
    Loop2 = setelement(J, Loop1, Off),
    add_notes(Notes, Loop2);
add_notes([], Loop) ->
    Loop.

get_midi_events(I, Loop) ->
    get_midi_events_(maps:to_list(element(I, Loop))).

get_midi_events_([{{on,Chan,Note},{_I,_J,V}}|Es]) ->
    [#note_on{chan=Chan,note=Note,velocity=V} | get_midi_events_(Es)];
get_midi_events_([{{off,Chan,Note},{_I,_J,V}}|Es]) ->
    [#note_off{chan=Chan,note=Note,velocity=V} | get_midi_events_(Es)];
get_midi_events_([]) ->
    [].

put_midi_events(Es,I,Loop,Active,Deactive) ->
    put_midi_events_(Es,element(I, Loop),I,Loop,Active,Deactive).

put_midi_events_([#note_on{chan=Chan,note=Note,velocity=Velocity}|Es],
		MapI,I,Loop,Active,Deactive) ->
    MapI_1 = maps:put({on,Chan,Note},{I,0,Velocity},MapI),
    put_midi_events_(Es,MapI_1,I,Loop,Active#{ {Chan,Note} => {I,Velocity}},
		     Deactive);
put_midi_events_([#note_off{chan=Chan,note=Note,velocity=_V}|Es],
		 MapJ,J,Loop,Active,Deactive) ->
    case maps:get({Chan,Note}, Active, false) of
	false ->
	    put_midi_events_(Es,MapJ,J,Loop,Active,Deactive);
	{I,V} ->
	    MapJ_1 = maps:put({off,Chan,Note},{I,J,V}, MapJ),
	    MapI = element(I, Loop),
	    MapI_1 = maps:put({on,Chan,Note},{I,J,V}, MapI),
	    Loop1 = setelement(I, Loop, MapI_1),
	    Active1 = maps:remove({Chan,Note}, Active),
	    put_midi_events_(Es, MapJ_1, J, Loop1, Active1,
			     [{{Chan,Note},{I,V}}|Deactive])
    end;
put_midi_events_([], MapI, I, Loop, Active, Decative) ->
    {setelement(I, Loop, MapI),Active, Decative}.

%% keep track on current events
%% {Chan,Note} => {StartTick, Velocity}

active_events([#note_on{chan=Chan,note=Note,velocity=V}|Es], I,
	      Active, Deactive) ->
    active_events(Es, I, maps:put({Chan,Note}, {I,V}, Active), Deactive);
active_events([#note_off{chan=Chan,note=Note}|Es],I,Active,Deactive) ->
    {S,V} = maps:get({Chan,Note}, Active),
    active_events(Es, I, maps:remove({Chan,Note}, Active),
		  [{{Chan,Note},{S,V}}|Deactive]);
active_events([_|Es], I, Active, Deactive) ->
    active_events(Es, I, Active, Deactive);
active_events([], _I, Active, Deactive) ->
    {Active,Deactive}.

collect_events(Window, Es) ->
    receive
	{epx_event,Window,{key_press, Sym, _Mod, _Code}}  ->
	    case make_key_note(Sym, #note_on{}) of
		false ->
		    collect_events(Window, Es);
		N -> 
		    collect_events(Window, [N|Es])
	    end;
	{epx_event,Window,{key_release, Sym, _Mod, _Code}}  ->
	    case make_key_note(Sym, #note_off{}) of
		false ->
		    collect_events(Window, Es);
		N -> 
		    collect_events(Window, [N|Es])
	    end
	after 0 ->
		Es
	end.

draw(#wparam{window=Window,screen=Screen,width=W,height=H}) ->
    epx:pixmap_draw(Screen,Window,0,0,0,0,W,H),
    epx:sync(Screen,Window).
%%
%% draw grid in 3 octaves in 5 pixels per note (12 notes per octave)
%% 60 pixels per octave gives 60*3 = 180 pixels in height
%% length = 5 pixels * PPL  + NQN  = (division=4) 4*
%%
-define(NOTE_HEIGHT, 5).
-define(NOTE_WIDTH,  5).
-define(NUM_OCTAVES, 3).
-define(START_NOTE,  48).  %% C1
-define(END_NOTE, 83).     %% B3

draw_grid(PPQN, Bars, #wparam{screen=Screen}) ->
    NQN = trunc(PPQN * 4),
    PPL = trunc(NQN * Bars),
    Height = ?NOTE_HEIGHT * 12 * ?NUM_OCTAVES,
    Width  = ?NOTE_WIDTH * PPL,
    epx_gc:set_foreground_color(lightGray),
    lists:foreach(
      fun(Y) ->
	      epx:draw_line(Screen, 0, Y, Width-1, Y)
      end, lists:seq(0,Height,?NOTE_HEIGHT)),
    lists:foreach(
      fun(X) ->
	      epx:draw_line(Screen, X, 0, X, Height-1)
      end, lists:seq(0,Width,?NOTE_WIDTH)),
    ok.


draw_notes(I, [#note_on{note=N,velocity=V}|Es], WParam) ->
    if V > 0 ->
	    draw_note_on(I, N, WParam);
       true ->
	    ok
    end,
    draw_notes(I, Es, WParam);
draw_notes(I, [_|Es], WParam) ->
    draw_notes(I, Es, WParam);
draw_notes(_I, [], _WParam) ->
    ok.
        
draw_note_on(I, Note, #wparam{screen=Screen}) when
      Note >= ?START_NOTE, Note =< ?END_NOTE ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(red),
    X = I*?NOTE_WIDTH,
    J = Note - ?START_NOTE,
    Y = J*?NOTE_HEIGHT,
    epx:draw_rectangle(Screen, X, Y, ?NOTE_WIDTH, ?NOTE_HEIGHT).

%% draw active note as white line
draw_active(Active, Deactive, I, N, WParam) ->
    clear_deactive(Deactive, I, N, WParam),
    draw_active_(maps:to_list(Active), I, WParam).

clear_deactive([{{_Chan,Note},{S,_V}}|Ds],I,N,WParam=#wparam{screen=Screen}) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(red),
    X = S*?NOTE_WIDTH,
    J = Note - ?START_NOTE,
    Y = J*?NOTE_HEIGHT,
    epx:draw_rectangle(Screen, X, Y, ?NOTE_WIDTH, ?NOTE_HEIGHT),
    S1 = ?TICK_NEXT(S, N),
    clear_until_(Y, S1, I, N, Screen),
    clear_deactive(Ds, I, N, WParam);
clear_deactive([], _I, _N, _WParam) ->
    ok.

clear_until_(_Y, S, S, _N, _Screen) -> ok;
clear_until_(Y, S, I, N, Screen) -> 
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(red),
    X = S*?NOTE_WIDTH,
    epx:draw_rectangle(Screen, X, Y, ?NOTE_WIDTH, ?NOTE_HEIGHT),
    clear_until_(Y, ?TICK_NEXT(S,N), I, N, Screen).
    

draw_active_([{{_Chan,Note},{S,_V}}|As],I,WParam=#wparam{screen=Screen}) ->
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(white),
    X = I*?NOTE_WIDTH,
    J = Note - ?START_NOTE,
    Y = J*?NOTE_HEIGHT,
    W = if I =:= S -> 1; true -> 0 end,
    epx:draw_rectangle(Screen, X+W, Y+1, ?NOTE_WIDTH-W, ?NOTE_HEIGHT-2),
    draw_active_(As, I, WParam);
draw_active_([], _I, _WParam) ->
    ok.

make_key_note(Sym, Note) ->
    case Sym of
	$z -> make_note(?C, Note);
	$s -> make_note(?C+1, Note);
	$x -> make_note(?D, Note);
	$d -> make_note(?D+1, Note);
	$c -> make_note(?E, Note);
	$v -> make_note(?F, Note);
	$g -> make_note(?F+1, Note);
	$b -> make_note(?G, Note);
	$h -> make_note(?G+1, Note);
	$n -> make_note(?A, Note);
	$j -> make_note(?A+1, Note);
	$m -> make_note(?B, Note);

	$q -> make_note(?c, Note);
	$2 -> make_note(?c+1, Note);
	$w -> make_note(?d, Note);
	$3 -> make_note(?d+1, Note);
	$e -> make_note(?e, Note);
	$r -> make_note(?f, Note);
	$5 -> make_note(?f+1, Note);
	$t -> make_note(?g, Note);
	$6 -> make_note(?g+1, Note);
	$y -> make_note(?a, Note);
	$7 -> make_note(?a+1, Note);
	$u -> make_note(?b, Note);
	_ -> false
    end.

make_note(Note, N=#note_on{}) -> 
    N#note_on{chan=0,note=Note,velocity=127};
make_note(Note, N=#note_off{}) -> 
    N#note_off{chan=0,note=Note,velocity=127}.

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
	    Devices = midi:devices(),
	    case midi:find_device_by_name(Name, Devices) of
		false -> undefined;
		#{device := DeviceName } ->
		    open_device(DeviceName);
		#{output := [Port]} -> %% already connected
		    case midi:find_device_by_port(Port, Devices) of
			false ->
			    {error, port_not_found};
			#{device := DeviceName } ->
			    open_device(DeviceName);
			_Device -> %% go further?
			    {error, device_not_found}
		    end;
		Device ->  %% program VMPK ...
		    B = midi:backend(),
		    case B:find_free_virtual_port(Devices) of
			false ->
			    io:format("forgot to install the virmid? run setup.sh\n"),
			    {error, no_ports_available};
			Virt = #{ device := DeviceName } ->
			    case B:connect(Device, Virt) of
				"" ->
				    open_device(DeviceName);
				Err -> 
				    {error,Err}
			    end
		    end
	    end
    end.

open_device(DeviceName) ->
    case midi:open(DeviceName,
		   [event,list,running]) of
	{ok,Fd} -> Fd;
	Error = {error,Reason} ->
	    io:format("warnig: unable to open device ~s for input: ~p\n",
		      [DeviceName, Reason]),
	    Error
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
