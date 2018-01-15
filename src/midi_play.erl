%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Play midi file
%%% @end
%%% Created :  4 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_play).
-export([file/2]).

-export([chord/4]).
-export([chordname_to_notes/1, chordname_to_notes/2]).
-export([notename_to_note/1]).
-export([maj/4, maj7/4, add9/4, maj9/4, maj7b5/4]).
-export([min/4, min7/4, min_sharp7/4]).
-export([dim/4, dim7/4]).

-export([note_to_frequency/1]).
-export([frequency_to_note/1]).
-export([octave/1, note_number/1]).

-define(USEC_PER_MINUTE, 60000000).
-define(DEFAULT_MPQN,    500000).

-include("midi.hrl").
%%
%% PPQN = pulses per quarter note
%% MPQN = micro seconds per quarter beat
%% BPM  = beats per minute, = (USEC_PER_MINUTE/MPQN)
%% MidiTick = Number of ticks per microsecond = (MPQN/PPQN)
%%

file(File, Device) ->
    file(File, Device, ?USEC_PER_MINUTE / ?DEFAULT_MPQN, 0).

file(File, Device, BPM, Bank) ->
    case midi_file:load(File) of
	{ok,{1,_NumTracks,Division},Tracks} ->
	    Fd = if is_list(Device) ->
			 {ok,Fd1} = midi:open(Device,[raw,list]), Fd1;
		    is_reference(Device) -> Device
		 end,
	    midi:reset_all(Fd, 0),
	    if Bank > 0 ->
		    lists:foreach(
		      fun(I) ->
			      midi:bank_msb(Fd, I, Bank)
		      end, lists:seq(0, 15));
	       true ->
		    ok
	    end,
	    MPQN = ?USEC_PER_MINUTE/BPM,
	    PPQN = if Division >= 0 -> Division; true -> 1.0 end,
	    MidiTick =
		if Division >= 0 ->
			MPQN / PPQN;
		   true ->
			TicksPerFrame = Division band 16#ff,
			FramesPerSec = 
			    case Division bsr 8 of
				-24 -> 24.0;
				-25 -> 25.0;
				-29 -> 39.97;
				-30 -> 30.0;
				_ -> 0.0
			    end,
			?DEFAULT_MPQN / (FramesPerSec*TicksPerFrame)
		end,
	    io:format("wait MPQN=~w,PPQN=~w,BPM=~w,~w us/tick\n",
		      [MPQN, PPQN, BPM, MidiTick]),
	    %% midi baud rate is 31250 => 1302 events / sec
	    %% remove track id for now, not used
	    Tracks1 = [T || {_TID,T} <- Tracks],
	    play_(Fd,Tracks1,{MPQN,PPQN,BPM,MidiTick}),
	    if File =/= Fd ->
		    midi:close(Fd);
	       true -> ok
	    end,
	    ok;
	Error ->
	    Error
    end.

%% fixme: tempo!
play_(_Fd, [], _TParam) ->
    ok;
play_(Fd, Ts, TParam) ->
    {Wait,Acc,TParam1} = next_track(Fd,Ts,[],1000000000,TParam),
    wait_ticks(Wait,TParam1),
    Ts1 = delta(Acc,[],Wait),
    play_(Fd,Ts1,TParam1).

wait_ticks(Ticks, {_MPQN,_PPQN,_BPM,MidiTick}) ->
    io:format("wait MPQN=~w,PPQN=~w,BPM=~w,~w us/tick\n",
	      [_MPQN, _PPQN, _BPM, MidiTick]),
    WaitMs = trunc((Ticks * MidiTick) / 1000),
    receive
    after WaitMs ->
	    ok
    end.

delta([[D|Es]|Ts], Acc, Wait) when is_integer(D) ->
    delta(Ts, [[D-Wait|Es]|Acc], Wait);
delta([], Acc, _Wait) ->
    Acc.

play_tracks(Fd,Es0=[D|Es],Ts,Acc,Wait,TParam) when is_integer(D) ->
    if D =< 0 ->
	    play_tracks(Fd,Es,Ts,Acc,Wait,TParam);
       true ->
	    next_track(Fd,Ts,[Es0|Acc],erlang:min(D,Wait),TParam)
    end;
play_tracks(Fd,[{meta,Meta,Value}|Es],Ts,Acc,Wait,TParam) ->
    case Meta of
	end_of_track ->
	    io:format("end of track\n", []),
	    next_track(Fd,Ts,Acc,Wait,TParam);
	tempo ->
	    {_,PPQN,_BPM,_MidiTick} = TParam,
	    MPQN = Value,
	    BPM  = ?USEC_PER_MINUTE / MPQN,
	    MidiTick = MPQN / PPQN,
	    io:format("set MPQN=~w,PPQN=~w,BPM=~w,~w us/tick\n",
		      [MPQN, PPQN, BPM, MidiTick]),
	    TParam1 = {MPQN,PPQN,BPM,MidiTick},
	    play_tracks(Fd,Es,Ts,Acc,Wait,TParam1);
	_ ->
	    io:format("Meta ~p ~p\n", [Meta,Value]),
	    play_tracks(Fd,Es,Ts,Acc,Wait,TParam)
    end;
play_tracks(Fd,[E|Es],Ts,Acc,Wait,TParam) ->
    Bytes = midi_codec:event_encode(E),
    midi:write(Fd, Bytes),
    play_tracks(Fd,Es,Ts,Acc,Wait,TParam);
play_tracks(Fd,[],Ts,Acc,Wait,TParam) ->
    io:format("track reached end not meta\n", []),
    next_track(Fd,Ts,Acc,Wait,TParam).

next_track(_Fd,[],Acc,Wait,TParam) -> %% one complete round
    {Wait,Acc,TParam};
next_track(Fd,[T|Ts],Acc,Wait,TParam) -> 
    play_tracks(Fd,T,Ts,Acc,Wait,TParam).

%% Util to play various chords
chord(Synth, Chan, Notes, Len) ->
    lists:foreach(fun(Note) -> midi:note_on(Synth,Chan,Note,100) end, Notes),
    timer:sleep(Len),
    lists:foreach(fun(Note) -> midi:note_off(Synth,Chan,Note,100) end, Notes),
    ok.

maj(Synth, Chan, R, Len) -> %% X | Xmaj
    chord(Synth, Chan, [R,R+4,R+7],Len).

add9(Synth, Chan, R, Len) -> %% Xadd9
    chord(Synth, Chan, [R,R+4,R+7,R+14],Len).

maj7b5(Synth, Chan, R, Len) -> %% X7b5
    chord(Synth, Chan, [R,R+4,R+6,R+10],Len).

maj7(Synth, Chan, R, Len) -> %% Xmaj7
    chord(Synth, Chan, [R,R+4,R+7,R+11],Len).

maj9(Synth, Chan, R, Len) -> %% Xmaj9
    chord(Synth, Chan, [R,R+4,R+7,R+11,R+14],Len).

min(Synth, Chan, R, Len) -> %% Xm|Xmin
    chord(Synth, Chan, [R,R+3,R+7],Len).

min7(Synth, Chan, R, Len) -> %% Xm7
    chord(Synth, Chan, [R,R+3,R+7,R+10],Len).

min_sharp7(Synth, Chan, R, Len) -> %% Xm#7
    chord(Synth, Chan, [R,R+3,R+7,R+11],Len).

dim(Synth, Chan, R, Len) -> %% Xdim
    chord(Synth, Chan, [R,R+3,R+6],Len).

dim7(Synth, Chan, R, Len) -> %% Xdim7
    chord(Synth, Chan, [R,R+3,R+6,R+9],Len).

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
