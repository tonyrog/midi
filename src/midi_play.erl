%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Play midi file
%%% @end
%%% Created :  4 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_play).
-export([file/1, file/2]).

-export([chordname_to_notes/1, chordname_to_notes/2]).
-export([notename_to_note/1]).

-export([note_to_frequency/1]).
-export([frequency_to_note/1]).
-export([octave/1, note_number/1]).
-export([chord/1, chord/2]).
-export([chord_1st/1, chord_1st/2]).
-export([chord_2nd/1, chord_2nd/2]).
-export([chord/4]).

-export([tracks/5]).

-define(USEC_PER_MINUTE, 60000000).
-define(DEFAULT_MPQN,    500000).

-include("midi.hrl").

-define(format_record(T,R), format_record((R), record_info(fields,T))).
	
%% time parameters

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

file(File) ->
    file(File, synth).

file(File, Device) ->
    file(File, Device, ?USEC_PER_MINUTE / ?DEFAULT_MPQN, 0).

file(File, Device, BPM, Bank) ->
    case midi_file:load(File) of
	{ok,{1,_NumTracks,Division},Tracks} ->
	    Fd = if Device =:= synth -> synth;
		    is_list(Device) -> %% device name
			 {ok,Fd1} = midi:open(Device,[raw,list]), Fd1;
		    is_reference(Device) -> Device
		 end,
	    Tracks1 = [T || {_TID,T} <- Tracks],
	    tracks(Fd, Tracks1, BPM, Bank, Division),
	    if is_atom(Fd) -> ok;  %% do not close "standard" synth
	       Device =/= Fd -> midi:close(Fd);
	       true -> ok
	    end,
	    ok;
	Error ->
	    Error
    end.

tracks(Fd, Tracks, BPM, Bank, Division) ->
    midi:reset_all(Fd, 0),
    if Bank > 0 ->
	    lists:foreach(
	      fun(I) -> midi:bank(Fd, I, Bank) end, lists:seq(0, 15));
       true ->
	    ok
    end,
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
    play(Fd,Tracks,TParam).

play(Fd,Trs,TParam) ->
    {Trs1,Ts1} = init(Trs,[],[],TParam),
    play_(Fd,Trs1,Ts1,0,time_us(),TParam).

play_(Fd,Trs,Ts,Ticks,TimeUs,TParam) ->
    Now = lists:min(Ts),
    WaitUs = (Now - Ticks)*TParam#tparam.uspp,
    NextUs = trunc(TimeUs + WaitUs),
    End = wait_until(NextUs),
    if End - NextUs > 500 -> io:format("td = ~w\n", [End-NextUs]);
       true -> ok
    end,
    next_(Fd,Trs,Ts,[],[],Now,Ticks,TimeUs,TParam).

next_(Fd,[[E|Es]|Trs],[T|Ts],Trs1,Ts1,Now,Ticks,TimeUs,TParam)
  when T =< Now ->
    case exec(Fd,E,TParam) of
	{tempo,TParam1} ->
	    next__(Fd,Es,T,Trs,Ts,Trs1,Ts1,Now,Now,time_us(),TParam1);
	{ok,TParam1} ->
	    next__(Fd,Es,T,Trs,Ts,Trs1,Ts1,Now,Ticks,TimeUs,TParam1);
	eot ->
	    next_(Fd,Trs,Ts,Trs1,Ts1,Now,Ticks,TimeUs,TParam)
    end;
next_(Fd,[Es|Trs],[T|Ts],Trs1,Ts1,Now,Ticks,TimeUs,TParam) ->
    next_(Fd,Trs,Ts,[Es|Trs1],[T|Ts1],Now,Ticks,TimeUs,TParam);
next_(_Fd,[],[],[],[],_Now,_Ticks,_TimeUs,_TParam) -> 
    ok;
next_(Fd,[],[],Trs1,Ts1,_Now,Ticks,TimeUs,TParam) ->
    play_(Fd,lists:reverse(Trs1),lists:reverse(Ts1),Ticks,TimeUs,TParam).

next__(Fd,[D|Es],T,Trs,Ts,Trs1,Ts1,Now,Ticks,TimeUs,TParam) ->
    next_(Fd,[Es|Trs],[T+D|Ts],Trs1,Ts1,Now,Ticks,TimeUs,TParam);
%%    next_(Fd,Trs,Ts,[Es|Trs1],[T+D|Ts1],Now,Ticks,TimeUs,TParam);
next__(Fd,[],_T,Trs,Ts,Trs1,Ts1,Now,Ticks,TimeUs,TParam) ->
    io:format("warn: eot not signaled\n", []),
    next_(Fd,Trs,Ts,Trs1,Ts1,Now,Ticks,TimeUs,TParam).


%% load initial delta time from 0
init([[D|Es] | Trs], Trs1, Ts, TParam) when is_integer(D) ->
    init(Trs, [Es|Trs1], [D|Ts], TParam);
init([], Trs, Ts, _TParam) -> 
    {lists:reverse(Trs), lists:reverse(Ts)}.

exec(_Fd,{meta,Meta,Value},TParam) ->
    case Meta of
	end_of_track ->
	    eot;
	tempo ->
	    PPQN = TParam#tparam.ppqn,
	    MPQN = Value,
	    BPM  = ?USEC_PER_MINUTE / MPQN,
	    USPP = MPQN / PPQN,  %% micro seconds per (midi) pulse
	    TParam1 = TParam#tparam{mpqn=MPQN,bpm=BPM,uspp=USPP},
	    {tempo,TParam1};
	time_signature ->
	    [NN,DD,CC,BB] = Value,
	    TParam1 = TParam#tparam{sig={NN,(1 bsl DD)},cc=CC,bb=BB},
	    {ok,TParam1};
	_ ->
	    {ok,TParam}
    end;    
exec(Fd, E, TParam) ->
    Bytes = midi_codec:event_encode(E),
    ok = midi:write(Fd, Bytes),
    {ok,TParam}.

time_us() ->
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

%% Util to play various chords (testing)
chord(ChordName) ->
    chord(ChordName,1000).

chord(ChordName,Len) ->
    chord(synth,0,ChordName,Len).

chord(Synth,Chan,ChordName,Len) ->
    chord_(Synth,Chan,chordname_to_notes(ChordName),Len).

chord_1st(ChordName) ->
    chord_1st(ChordName,1000).

chord_1st(ChordName,Len) ->
    chord_1st(synth,0,ChordName,Len).

chord_1st(Synth,Chan,ChordName,Len) ->
    chord_(Synth,Chan,first_inversion(chordname_to_notes(ChordName)),Len).

chord_2nd(ChordName) ->
    chord_2nd(ChordName,1000).

chord_2nd(ChordName,Len) ->
    chord_2nd(synth,0,ChordName,Len).

chord_2nd(Synth,Chan,ChordName,Len) ->
    chord_(Synth,Chan,second_inversion(chordname_to_notes(ChordName)),Len).

chord_(Synth,Chan,Notes,Len) ->
    lists:foreach(fun(Note) -> midi:note_on(Synth,Chan,Note,100) end, Notes),
    timer:sleep(Len),
    lists:foreach(fun(Note) -> midi:note_off(Synth,Chan,Note) end, Notes),
    ok.

%% translate note name to note value
notename_to_note([$~|Cs]) -> notename_to_note(Cs);
notename_to_note([C|Cs]) when C >= $A, C =< $G ->
    R = element((C-$A)+1, {?A,?B,?C,?D,?E,?F,?G}),
    notename_to_note(R, Cs);
notename_to_note([C|Cs]) when C >= $a, C =< $g ->
    R = element((C-$a)+1, {?a,?b,?c,?d,?e,?f,?g}),
    notename_to_note(R, Cs).

notename_to_note(R,[$,|Cs]) -> notename_to_note(R-12,Cs);
notename_to_note(R,[$'|Cs]) -> notename_to_note(R+12,Cs);
notename_to_note(R,[$#|Cs]) -> notename_to_note(R+1,Cs);
notename_to_note(R,[$b|Cs]) -> notename_to_note(R-1,Cs);
notename_to_note(R,Cs) -> {R,Cs}.
    
%% get a list of midi notes given a chord name
chordname_to_notes(Cs) ->
    {R,Cs1} = notename_to_note(Cs),
    chordname_to_notes(R, Cs1).

chordname_to_notes(R,"")       -> [R,R+4,R+7];  %% major
chordname_to_notes(R,"maj")    -> [R,R+4,R+7];
chordname_to_notes(R,"6")      -> [R,R+4,R+7,R+9];
chordname_to_notes(R,"7")      -> [R,R+4,R+7,R+10];
chordname_to_notes(R,"9")      -> [R,R+4,R+7,R+9,R+13];
chordname_to_notes(R,"11")     -> [R,R+4,R+7,R+10,R+14,R+17];
chordname_to_notes(R,"13")     -> [R,R+10,R+14,R+17,R+21];
chordname_to_notes(R,"add9")   -> [R,R+4,R+7,R+14];
chordname_to_notes(R,"maj7b5") -> [R,R+4,R+6,R+10];
chordname_to_notes(R,"maj7")   -> [R,R+4,R+7,R+11]; %% 7?
chordname_to_notes(R,"maj9")   -> [R,R+4,R+7,R+11,R+14];
chordname_to_notes(R,"min")    ->  [R,R+3,R+7];
chordname_to_notes(R,"min7")   -> [R,R+3,R+7,R+10];
chordname_to_notes(R,"min#7")  -> [R,R+3,R+7,R+11];
chordname_to_notes(R,[$m|Cs]) when Cs =:= []; hd(Cs) =/= $i ->
    chordname_to_notes(R,"min"++Cs);
chordname_to_notes(R,"dim")    -> [R,R+3,R+6];
chordname_to_notes(R,"dim7")   -> [R,R+3,R+6,R+9].

first_inversion([R,A,B|Ns]) -> [R,A-12,B-12|Ns].

second_inversion([R,A,B|Ns]) -> [R,A,B-12|Ns].

%% F = 440*2^((N-69)/12)
%% N = 12*(log2(F)-log2(440))+69
%%
frequency_to_note(F) ->
    trunc(12*(math:log2(F)-math:log2(440))+69).

note_to_frequency(Note) ->
    440.0 * math:pow(2.0, (Note-69.0) / 12.0).

%% octave from note
octave(Note) -> Note div 12.

%% note number with in octave
note_number(Note) -> Note rem 12.


format_record(R, Fs) ->
    ["#",atom_to_list(element(1,R)),"{",
     fmt_fld(R, 2, hd(Fs)), fmt_flds(R,3,tl(Fs)),"}"].

fmt_flds(_R, _I, []) -> [];
fmt_flds(R, I, [F|Fs]) -> [",",fmt_fld(R,I,F),fmt_flds(R,I+1,Fs)].

fmt_fld(R, I, F) ->
    [atom_to_list(F),"=",io_lib:format("~p",[element(I,R)])].
     
    
    
