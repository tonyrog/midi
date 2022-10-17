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

-include("../include/midi.hrl").

%% time parameters

file(File) ->
    file(File, synth).

file(File, Device) ->
    file(File, Device, ?USEC_PER_MINUTE / ?DEFAULT_MPQN, 0).

file(File, Device, BPM, Bank) ->
    case midi_file:load(File) of
	{ok,{_I,_NumTracks,Division},Tracks} ->
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
    lists:foreach(
      fun(Chan) ->
	      midi:all_off(Fd, Chan),
	      midi:reset_all(Fd, Chan)
      end, lists:seq(0, 15)),
    if Bank > 0 ->
	    lists:foreach(
	      fun(I) -> midi:bank(Fd, I, Bank) end, lists:seq(0, 15));
       true ->
	    ok
    end,
    TParam = midi:tparam(BPM, Division),
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
	    {tempo, midi:tparam_set_tempo(TParam, Value)};
	time_signature ->
	    {ok,midi:tparam_set_time_signature(TParam,Value)};
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
    %% 440.0 * math:pow(2.0, (Note-69.0) / 12.0).
    element(Note+1,
	    {
	     8.18,    8.66,   9.18,   9.72,   10.30,  10.91,  11.56,  12.25, 
	     12.98,  13.75,  14.57,  15.43,
	     %% C0
	     16.35,  17.32,  18.35,  19.45,  20.60,  21.83,  23.12,  24.50,
	     25.96,  27.50,  29.14,  30.87,
	     %% C1
	     32.70,  34.65,  36.71,  38.89,  41.20,  43.65,  46.25,  49.00,
	     51.91,  55.00,  58.27,  61.74,
	     %% C2
	     65.41,  69.30,  73.42,  77.78,  82.41,  87.31,  92.50,  98.00,
	     103.83, 110.00, 116.54, 123.47,
	     %% C3
	     130.81, 138.59, 146.83, 155.56,
	     164.81, 174.61, 185.00, 196.00, 207.65, 220.00, 233.08, 246.94, 
	     %% C4
	     261.63, 277.18, 293.66, 311.13, 329.63, 349.23, 369.99, 392.00, 
	     415.30, 440.00, 466.16, 493.88, 
	     %% C5
	     523.25, 554.37, 587.33, 622.25, 659.26, 698.46, 739.99, 783.99,
	     830.61, 880.00, 932.33, 987.77,
	     %% C6
	     1046.50, 1108.73, 1174.66, 1244.51, 1318.51, 1396.91, 1479.98,
	     1567.98, 1661.22, 1760.00, 1864.66, 1975.53,
	     %% C7
	     2093.00, 2217.46, 2349.32, 2489.02, 2637.02, 2793.83, 2959.96,
	     3135.96, 3322.44, 3520.00, 3729.31, 3951.07, 
	     %% C8
	     4186.01, 4434.92, 4698.64, 4978.03, 5274.04, 5587.65, 5919.91, 
	     6271.93, 6644.88, 7040.00, 7458.62, 7902.13, 
	     %% C9
	     8372.02, 8869.84, 9397.27, 9956.06, 10548.08, 11175.30, 
	     11839.82, 12543.85
	    }).

%% octave from note
octave(Note) -> Note div 12.

%% note number with in octave
note_number(Note) -> Note rem 12.

-ifdef(not_use).

-define(format_record(T,R), format_record((R), record_info(fields,T))).

format_record(R, Fs) ->
    ["#",atom_to_list(element(1,R)),"{",
     fmt_fld(R, 2, hd(Fs)), fmt_flds(R,3,tl(Fs)),"}"].

fmt_flds(_R, _I, []) -> [];
fmt_flds(R, I, [F|Fs]) -> [",",fmt_fld(R,I,F),fmt_flds(R,I+1,Fs)].

fmt_fld(R, I, F) ->
    [atom_to_list(F),"=",io_lib:format("~p",[element(I,R)])].
-endif.
     
