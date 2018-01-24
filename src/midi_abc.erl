%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    ABC format parser/player
%%% @end
%%% Created :  7 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_abc).

-export([parse_file/1]).
-export([parse_string/1]).
-export([play_file/1, play_file/2]).
-export([play_string/1, play_string/2]).
-export([play/1,play/2]).
-export([play_file/4, play_string/4, play/4]).

-compile(export_all).

-include("midi.hrl").

-define(is_digit(C),   (((C) >= $0) andalso ((C) =< $9))).

play_file(File) ->
    play(synth, File).

play_file(Fd, File) ->
    play_file(Fd, File, first, first).

play_file(Fd, File, Index, Voice) ->
    case parse_file(File) of
	{error,Reason} -> {error,Reason};
	Ns -> play(Fd, Ns, Index, Voice)
    end.

parse_file(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    parse_string(binary_to_list(Bin),strict);
	Error ->
	    Error
    end.

parse_string(String) ->
    parse_string(String,relaxed).

parse_string(String,Mode) ->
    put(line, 1),
    parse_tunes(String,Mode).

play_string(String) ->
    play_string(synth,String).
play_string(Fd,String) ->
    play_string(Fd,String,first,first).

play_string(Fd,String,Index,Voice) ->
    Tunes = parse_tunes(String,relaxed),
    play(Fd,Tunes,Index,Voice).

play(Ns) ->
    play(synth,Ns).
play(Fd, Ns) ->
    play(Fd, Ns, first, first).

play(Fd, Ns, Index, Voice) ->
    midi:program_change(Fd, 0, ?GM_MIDI_Acoustic_Grand_Piano),
    midi:program_change(Fd, 1, ?GM_MIDI_Acoustic_Guitar_nylon),
    midi:program_change(Fd, 2, ?GM_MIDI_Acoustic_Bass),
    T = select_tune(Ns, Index),
    Ns1 = select_voice(T, Voice),
    Ctx = #{ tempo => 120,
	     unit_note_length => {1,4},
	     voice => 0,
	     channel => 0,
	     program => 0
	   },
    Signature = #{},
    play_notes(Fd,Ns1,Ctx,Signature,[],[]).

play_notes(Fd, [{repeat,N}|Ns],Ctx,Sig,SP,RP) ->
    Sig1 = drop_note_sig(Sig),
    play_notes(Fd,Ns,Ctx,Sig1,[{repeat,N}|SP],[N|RP]);
play_notes(Fd, [repeat_end|Ns],Ctx,Sig,SP,RP) ->
    Sig1 = drop_note_sig(Sig),
    case RP of
	[] ->
	    play_notes(Fd,Ns,Ctx,Sig1,[],[]);
	[0] ->
	    play_notes(Fd,Ns,Ctx,Sig1,[],[]);
	[0|RP1] ->
	    play_notes(Fd,Ns,Ctx,Sig1,[repeat_end|SP],RP1);
	[I|RP1] ->
	    {Ns1,SP1} = pop_notes([repeat_end|Ns],SP),
	    play_notes(Fd,Ns1,Ctx,Sig1,SP1,[I-1|RP1])
    end;
play_notes(Fd,[],_Ctx,_Sig,_SP,_RP) ->
    stop_guitar(Fd), %% stop old chord
    ok;
play_notes(Fd,[{part,Name,Ns1}|Ns],Ctx,Sig,SP,RP) ->
    %% play all parts 
    io:format("PART: ~p\n", [Name]),
    play_notes(Fd,Ns1++Ns,Ctx,Sig,SP,RP);

play_notes(Fd,Ns,Ctx,Sig,SP,RP) ->
    case get_note(Ns,Sig) of
	false ->
	    play_notes_(Fd,Ns,Ctx,Sig,SP,RP);
	{E={c,Notes,As},Ns1,Sig1} ->
	    play_chord(Fd,Notes,As,100,Ctx),
	    play_notes(Fd,Ns1,Ctx,Sig1,[E|SP],RP);
	{E={g,Notes,Bass,As},Ns1,Sig1} ->
	    play_guitar(Fd,Notes,As,Bass,100,Ctx),
	    play_notes(Fd,Ns1,Ctx,Sig1,[E|SP],RP);
	{E={n,Note,Len,As},Ns1,Sig1} ->
	    play_note(Fd,Note,As,100,Ctx,Len),
	    play_notes(Fd,Ns1,Ctx,Sig1,[E|SP],RP)
    end.

%% fixme: use modal?
play_notes_(Fd,[bar|Ns],Ctx,Sig,SP,RP) ->
    Sig1 = drop_note_sig(Sig),
    play_notes(Fd,Ns,Ctx,Sig1,SP,RP);
play_notes_(Fd,[double_bar|Ns],Ctx,Sig,SP,RP) ->
    Sig1 = drop_note_sig(Sig),
    play_notes(Fd,Ns,Ctx,Sig1,SP,RP);
play_notes_(Fd,[thin_thick|Ns],Ctx,Sig,SP,RP) ->
    Sig1 = drop_note_sig(Sig),
    play_notes(Fd,Ns,Ctx,Sig1,SP,RP);
play_notes_(Fd,[think_thin|Ns],Ctx,Sig,SP,RP) ->
    Sig1 = drop_note_sig(Sig),
    play_notes(Fd,Ns,Ctx,Sig1,SP,RP);

play_notes_(Fd, [{key,Key,_Modal}|Ns],Ctx,_Sig,SP,RP) ->
    io:format("key: ~s ~s\n", [Key,_Modal]),
    Sig1 = map_key(Key),
    io:format("signature: ~w\n", [Sig1]),
    play_notes(Fd,Ns,Ctx,Sig1,SP,RP);
play_notes_(Fd, [{title,T}|Ns],Ctx,Sig,SP,RP) ->
    io:format("title: ~s\n", [T]),
    play_notes(Fd,Ns,Ctx,Sig,SP,RP);
play_notes_(Fd, [{unit_note_length,A,B}|Ns],Ctx,Sig,SP,RP) ->
    Ctx1 = Ctx#{ unit_note_length => {A,B} }, 
    play_notes(Fd,Ns,Ctx1,Sig,SP,RP);
play_notes_(Fd, [{tempo,T}|Ns],Ctx,Sig,SP,RP) ->
    Ctx1 = Ctx#{ tempo => T},
    play_notes(Fd,Ns,Ctx1,Sig,SP,RP);
play_notes_(Fd, [{tempo,ULen,T}|Ns],Ctx,Sig,SP,RP) ->
    Ctx1 = Ctx#{ tempo => T, unit_note_length => ULen },
    play_notes(Fd,Ns,Ctx1,Sig,SP,RP);
play_notes_(Fd, [{midi,channel,Chan}|Ns],Ctx,Sig,SP,RP) ->
    Ctx1 = Ctx#{channel => Chan},
    play_notes(Fd,Ns,Ctx1,Sig,SP,RP);
play_notes_(Fd, [{midi,program,Prog}|Ns],Ctx=#{channel:=Chan},Sig,SP,RP) ->
    Ctx1 = Ctx#{program => Prog},
    midi:program_change(Fd,Chan,Prog),    
    play_notes(Fd,Ns,Ctx1,Sig,SP,RP);
play_notes_(Fd,[_|Ns],Ctx,Sig,SP,RP) ->
    %% more...
    play_notes(Fd,Ns,Ctx,Sig,SP,RP).

%% at repeat end move notes back for play
pop_notes(Ns, SP) ->
    pop_notes(Ns, 0, SP).

pop_notes(Ns, 0, SP=[{repeat,_}|_]) ->
    {Ns, SP};
pop_notes(Ns, I, [E={repeat,_}|SP1]) ->
    pop_notes([E|Ns],I-1,SP1); 
pop_notes(Ns, I, [repeat_end|SP1]) ->
    pop_notes([repeat_end|Ns],I+1,SP1);
pop_notes(Ns, I, [E|SP1]) ->
    pop_notes([E|Ns],I,SP1).

%% get note, process ^A, _B,  A>B, A>>B, A<<B
%% FIXME: handle grace notes! A<{g}A = A/2{g}A3/2
%%
%% FIXME: a tie should result in longer played note
%%        a slur is played long, then short /legato
%%        multiple notes in slur, short on last?
%%
get_note(Ns, Sig) ->
    case get_note_(Ns,Sig,[]) of
	false -> false;
	{E={c,_Notes,_As},Ns1,Sig1} -> {E,Ns1,Sig1};
	{E={g,_Notes,_Bass,_As},Ns1,Sig1} -> {E,Ns1,Sig1};
	{E={n,Note1,Len1,As1},Ns1,Sig1} ->
	    case get_dots(Ns1) of
		false ->
		    {E,Ns1,Sig1};
		{D1,D2,Ns2} ->
		    Len11 = mult(Len1,D1),
		    case get_note_(Ns2,Sig1,[]) of
			{{n,Note2,Len2,As2},Ns3,Sig2} ->
			    {{n,Note1,Len11,As1},
			     [{n,Note2,mult(Len2,D2),As2}|Ns3],Sig2}
		    end
	    end
    end.

get_note_([E={n,_Note,_Len,_As}|Ns],Sig,_) -> {E,Ns,Sig};
get_note_([E={c,_Notes,_As}|Ns],Sig,_) -> {E,Ns,Sig};
get_note_([E={g,_Notes,_Bass,_As}|Ns],Sig,_) -> {E,Ns,Sig};

%% sharpen should apply to the next bar ( unless a tie is used )
get_note_([sharpen,{note,Note,Len}|Ns],Sig,As) ->
    %% signature does not apply
    {Note1,Ns1,As1} = get_suffix(Ns,Note,[Note,sharpen|As]),
    {{n,Note1+1,Len,As1}, Ns1, [{Note1,Note1+1}|Sig]};

%% flatten should apply to the next bar ( unless a tie is used )
get_note_([flatten,{note,Note,Len}|Ns],Sig,As) ->
    %% signature does not apply
    {Note1,Ns1,As1} = get_suffix(Ns,Note,[Note,flatten|As]),
    {{n,Note1-1,Len,As1}, Ns1, [{Note1,Note1-1}|Sig]};

get_note_([staccato|Ns],Sig,As) ->
    get_note_(Ns,Sig,[staccato|As]);

get_note_([naturalise,{note,Note,Len}|Ns],Sig,As) ->
    %% signature does not apply
    {Note1,Ns1,As1} = get_suffix(Ns,Note,[Note,naturalise|As]),
    {{n,Note1,Len,As1}, Ns1, [{Note1,Note1}|Sig]};

get_note_([{note,Note,Len}|Ns],Sig,As) ->
    {Note1,Ns1,As1} = get_suffix(Ns,Note,[Note|As]),
    Note2 = get_note_sig(Note1, Sig),
    {{n,Note2,Len,As1}, Ns1, Sig};
get_note_([{rest,Len}|Ns],Sig,As) ->
    {{n,rest,Len,[rest|As]},Ns,Sig};
get_note_([{space,Len}|Ns],Sig,As) ->
    {{n,rest,Len},Ns,[space|As],Sig};
get_note_([{chord,Notes}|Ns],Sig,As) ->
    {{c,get_chord(Notes,Sig),As},Ns,Sig};
get_note_([{guitar,Note,Accidental,Type,Bass}|Ns],Sig,As) ->
    {g,Ns1,Bs} = get_guitar_chord(Note,Accidental,Type,Bass,Sig),
    {{g,Ns1,Bs,As},Ns,Sig};
get_note_(_,_Sig,_As) ->
    false.

get_suffix([tick|Ns],Note,As) -> get_suffix(Ns,Note+12,[tick|As]);
get_suffix([comma|Ns],Note,As) -> get_suffix(Ns,Note-12,[comma|As]);
get_suffix(Ns,Note,As) -> {Note,Ns,As}.


get_chord([], _Sig) ->
    [];
get_chord(Ns, Sig) ->
    case get_note_(Ns,Sig,[]) of
	{{n,rest,_Len1,_As},Ns1,Sig1} ->
	    get_chord(Ns1,Sig1);
	{{n,Note1,Len1,_As},Ns1,Sig1} ->
	    [{Note1,Len1}|get_chord(Ns1,Sig1)]
    end.

drop_note_sig([{_,_}|Sig]) -> drop_note_sig(Sig);
drop_note_sig(Sig) when is_map(Sig) -> Sig.

%% map note via signature and flattend, sharpend and neutralised notes
get_note_sig(Note, [{Note,Note1}|_Sig]) ->
    Note1;
get_note_sig(Note, [{_,_}|Sig]) ->
    get_note_sig(Note, Sig);
get_note_sig(Note, Sig) when is_map(Sig) ->
    case maps:find(Note, Sig) of
	error -> Note;
	{ok,Note1} -> Note1
    end.

%% Note = C,D,E,F,G,A,B
%% Accidental = # | b
%% Type = maj|min|sus|dim|+|7|9|11|#5...
%% Bass = c,d,e,f,g,a,b (accidental) +?
get_guitar_chord(NoteName,Accidental,Type,Bass,Sig) ->
    {Note0,""} = midi_play:notename_to_note(NoteName++Accidental),
    Note       = get_note_sig(Note0, Sig),  %% translate to signature
    Ns = midi_play:chordname_to_notes(Note, Type),
    Bs =
	if Bass =:= "" -> [];
	   true -> 
		{B0,_} = midi_play:notename_to_note(Bass), %% fixme: +?
		B1 = get_note_sig(B0, Sig),  %% translate to signature
		[B1]
	end,
    %% reverse notes in the chord to get Base the lowest note
    {g,[{N,{1,1}} || N <- Ns],Bs}.

%% dots factor is (2-1/2^n) = ((2^(n+1)-1)/2^n)
%% 
get_dots([dot|Ns]) -> get_dots(Ns,2);
get_dots([dotm|Ns]) -> get_dotms(Ns,2);
get_dots(_) -> false.

get_dots([dot|Ns],I) -> get_dots(Ns,2*I);
get_dots(Ns,N) -> {{2*N-1,N},{N-1,N},Ns}.

get_dotms([dotm|Ns],I) -> get_dotms(Ns,2*I);
get_dotms(Ns,N) -> {{N-1,N},{2*N-1,N},Ns}.

%% Tempo is number of default note lengths per minute
play_note(_Fd,rest,_As,_Velocity,#{tempo := Tempo},{D,E}) ->
    %% io:format("rest length=~w/~w\n", [D,E]),
    RestLen = trunc(((60*1000)/Tempo)*(D/E)),
    timer:sleep(RestLen);
play_note(Fd,Note,As,Velocity,#{tempo := Tempo, channel:=Chan},{D,E}) ->
    %% io:format("play note ~w [~s] chan=~w, length=~w/~w\n", 
    %%  [Note,fmt_note(As),Chan,D,E]),
    NoteLen = trunc(((60*1000)/Tempo)*(D/E)),
    case lists:member(staccato,As) of
	true ->
	    OnLen = trunc(NoteLen/4),
	    midi:note_on(Fd,Chan,Note,Velocity),
	    timer:sleep(OnLen),
	    midi:note_off(Fd,Chan,Note),
	    timer:sleep(NoteLen - OnLen);
	false ->
	    midi:note_on(Fd,Chan,Note,Velocity),
	    timer:sleep(NoteLen),
	    midi:note_off(Fd,Chan,Note)
    end.

%% Tempo is number of default note lengths per minute
play_chord(Fd,Ns,As,Velocity,#{tempo := Tempo,channel:=Chan}) ->
    Notes = [N || {N,_} <- Ns],
    %% fixme: now we find max length, but we should check instead
    Lens = [{A/B,{A,B}} || {_,{A,B}} <- Ns],
    {_,{D,E}} = lists:max(Lens),
    %% io:format("play note ~w length=~w/~w\n", [Note,D,E]),
    NoteLen = trunc(((60*1000)/Tempo)*(D/E)),
    case lists:member(staccato,As) of
	true ->
	    OnLen = trunc(NoteLen/4),
	    lists:foreach(fun(Note) -> 
				  midi:note_on(Fd,Chan,Note,Velocity) end, 
			  Notes),
	    timer:sleep(OnLen),
	    lists:foreach(fun(Note) -> 
				  midi:note_off(Fd,Chan,Note) end, 
			  Notes),
	    timer:sleep(NoteLen - OnLen);
	false ->
	    lists:foreach(fun(Note) -> 
				  midi:note_on(Fd,Chan,Note,Velocity) end, 
			  Notes),
	    timer:sleep(NoteLen),
	    lists:foreach(fun(Note) -> 
				  midi:note_off(Fd,Chan,Note) end, 
			  Notes)
    end.
	    
%% fixme: what channel to play guitar chords / bass on?
play_guitar(Fd,Ns0,_As,Bs,Velocity,_Ctx) ->
    stop_guitar(Fd), %% stop old chord
    Ns = [N || {N,_}<-Ns0],
    %% fixme: now we find max length, but we should check instead
    %% Lens = [{A/B,{A,B}} || {_,{A,B}} <- Ns],
    %% {_,{D,E}} = lists:max(Lens),
    %% io:format("play note ~w length=~w/~w\n", [Note,D,E]),
    %% NoteLen = trunc(((60*1000)/Tempo)*(D/E)),
    %% io:format("Ns = ~w, Bs = ~w\n", [Ns, Bs]),
    lists:foreach(fun(N) -> midi:note_on(Fd,2,N-24,Velocity) end, Bs),
    lists:foreach(fun(N) -> midi:note_on(Fd,1,N-24,Velocity) end, Ns),
    put(g, {Ns,Bs}).

stop_guitar(Fd) ->
    case get(g) of
	undefined -> ok;
	{Ns,Bs} ->
	    lists:foreach(fun(N) -> midi:note_off(Fd,1,N-24) end, Ns),
	    lists:foreach(fun(N) -> midi:note_off(Fd,2,N-24) end, Bs),
	    erase(g)
    end.

fmt_note(As) ->
    fmt_note(As,[]).

fmt_note([],Acc) -> Acc;
fmt_note([staccato|As],Acc) -> fmt_note(As,[$.|Acc]);
fmt_note([sharpen|As],Acc) -> fmt_note(As,[$^|Acc]);
fmt_note([flatten|As],Acc) -> fmt_note(As,[$_|Acc]);
fmt_note([naturalise|As],Acc) -> fmt_note(As,[$=|Acc]);
fmt_note([tick|As],Acc) -> fmt_note(As,[$'|Acc]);
fmt_note([comma|As],Acc) -> fmt_note(As,[$,|Acc]);
fmt_note([Note|As],Acc) when is_integer(Note) ->
    case Note of
	?C -> fmt_note(As,[$C|Acc]);
	?D -> fmt_note(As,[$D|Acc]);
	?E -> fmt_note(As,[$E|Acc]);
	?F -> fmt_note(As,[$F|Acc]);
	?G -> fmt_note(As,[$G|Acc]);
	?A -> fmt_note(As,[$A|Acc]);
	?B -> fmt_note(As,[$B|Acc]);
	?c -> fmt_note(As,[$c|Acc]);
	?d -> fmt_note(As,[$d|Acc]);
	?e -> fmt_note(As,[$e|Acc]);
	?f -> fmt_note(As,[$f|Acc]);
	?g -> fmt_note(As,[$g|Acc]);
	?a -> fmt_note(As,[$a|Acc]);
	?b -> fmt_note(As,[$b|Acc]);
	_ -> fmt_note(As,[integer_to_list(Note)|Acc])
    end.

%% 
%% Parse ABC format
%%
%% {tune,I,Header,Body}
%% Header=[{index,I},
%%         ...,
%%         {title,T},
%%         {meter,M},
%%         {unit_note_length,L},
%%         ...,
%%         {voices, n},
%%         {part,"A2B"}
%%         {key,K}]
%% 
%% Body=
%% [{part,"A",[{voice,V1,_},...,{voice,Vn,_}]},
%%  {part,"B",[{voice,V1,_},...,{voice,Vn,_}]},
%%  ..
%%  {part,"Z",[{voice,V1,_},...,{voice,Vn,_}]}]
%%
parse_tunes(Cs,Mode) ->
    parse_tunes(Cs,[],Mode).

parse_tunes([],Acc,_Mode) -> 
    lists:reverse(Acc);
parse_tunes(Cs,Acc,Mode) ->
    try parse_h(Cs,[]) of
	{[],[]} -> Acc;
	{Cs1,Header0} ->
	    {Cs2,Body0} = parse_b(Cs1,[]),
	    Tune = proplists:get_value(index,Header0,0),
	    Header1 = translate_parts(Header0),
	    {Header,Body1} = move_bad_voices(Header1, lists:reverse(Body0)),
	    BodyParts = parts(Body1),
	    parse_tunes(Cs2,[{tune,Tune,Header,BodyParts}|Acc],Mode)
    catch
	error:_ when Acc =:= [], Mode =:= relaxed ->
	    {_Cs1,Body0} = parse_b(Cs,[]),
	    [{tune,0,[],lists:reverse(Body0)}]
    end.

%% parse part header into parts
translate_parts([{part,Parts}|Hs]) ->
    [{parts,parse_parts(Parts)}|Hs];
translate_parts([H|Hs]) ->
    [H|translate_parts(Hs)];
translate_parts([]) ->
    [].

parse_parts(Cs) ->
    parse_parts(Cs,[[]]).

parse_parts([$(|Cs],Stack) ->
    parse_parts(Cs,[[]|Stack]);
parse_parts([$)|Cs],[Es,Es1|Stack]) ->
    parse_parts(Cs,[[lists:reverse(Es)|Es1]|Stack]);
parse_parts([$\s|Cs],Stack) ->
    parse_parts(Cs,Stack);
parse_parts([C|Cs],[[E|Es]|Stack]) when C >= $1, C =< $9 ->
    parse_parts(Cs, [[{C-$0,E}|Es]|Stack]);
parse_parts([C|Cs],[Es|Stack]) ->
    parse_parts(Cs, [[[C]|Es]|Stack]);
parse_parts([], [Es]) ->
    lists:reverse(Es).

%% select tune if specified, [] if not found
select_tune(Ns, first) ->
    case lists:keyfind(tune,1,Ns) of
	false -> Ns; %% return the notes
	T -> T
    end;
select_tune(Ns, Ix) when is_integer(Ix) ->
    case lists:keyfind(Ix,2,Ns) of
	false -> [];
	T -> T
    end.

%% select a voice within tune
select_voice({tune,_,Hs,Bs}, Ix) ->
    Hs ++ select_voice_(Bs, Ix);
select_voice(Ns, Ix) when is_list(Ns) ->
    select_voice_(Ns, Ix).

select_voice_(Ns, first) ->
    case lists:keyfind(voice,1,Ns) of
	false -> Ns; %% return the notes
	{voice,_,Ns1} -> Ns1
    end;
select_voice_(Ns, Ix) when is_integer(Ix) ->
    case lists:keyfind(Ix,2,Ns) of
	false -> [];
	{voice,_,Ns1} -> Ns1
    end.

%% hack to move badly positioned voice declaration from
%% body to header, header MUST not be reverse before this
%% call but body MUST.
move_bad_voices([K={key,_,_}|Header], [V={voice,_,Ps}|Body]) 
  when Ps =/= [] ->
    move_bad_voices([K,V|Header], Body);
move_bad_voices(Header, Body) ->
    {lists:reverse(Header), Body}.

%% split body into body parts - if present
parts(Body) ->
    parts(Body,[]).

parts([{part,P}|Bs],Ps) ->
    parts(Bs,P,[],Ps);
parts([B|Bs],Ps) ->
    parts(Bs,[B|Ps]);
parts([],Ps) ->
    voices(lists:reverse(Ps)).

parts([{part,P2}|Bs],P1,Acc,Ps) ->
    Vs = voices(lists:reverse(Acc)),
    parts(Bs,P2,[],[{part,P1,Vs}|Ps]);
parts([B|Bs],P1,Acc,Ps) ->
    parts(Bs,P1,[B|Acc],Ps);
parts([],P1,Acc,Ps) ->
    Vs = voices(lists:reverse(Acc)),
    lists:reverse([{part,P1,Vs}|Ps]).

%% split part into voices - if present
voices(Part) ->
    voices(Part,[]).

voices([{voice,V,[]}|Ps],Vs) ->
    voices(Ps,V,[],Vs);
voices([P|Ps],Vs) ->
    voices(Ps,[P|Vs]);
voices([],Vs) ->
    lists:reverse(Vs).

voices([{voice,V2,[]}|Ps],V1,Acc,Vs) ->
    voices(Ps,V2,[],[{voice,V1,lists:reverse(Acc)}|Vs]);
voices([P|Ps],V1,Acc,Vs) ->
    voices(Ps,V1,[P|Acc],Vs);
voices([],V1,Acc,Vs) ->
    lists:reverse([{voice,V1,lists:reverse(Acc)}|Vs]).

parse_h({Cs,Acc}) ->
    parse_h(Cs,Acc).

parse_h(Cs,Acc) ->
    case Cs of
	[] -> {[],Acc};
	[$\s|Cs1] -> parse_h(Cs1,Acc); %% fixme, groups
	[$\t|Cs1] -> parse_h(Cs1,Acc); %% fixme, groups
	[$\\,$\n|Cs1] -> new_line(), parse_h(Cs1,Acc);  %% line continue
	[$\n|Cs1] -> new_line(), parse_h(Cs1,Acc); %% fixme: delete?
	[$\v|Cs1] -> parse_h(Cs1,Acc);  %% special for inline key/values
	[$\r|Cs1] -> parse_h(Cs1,Acc);
	[$%|Cs1]  -> {_Line,Cs2} = get_line(Cs1),parse_h(Cs2,Acc);

	[$A,$:|Cs1] -> parse_h(parse_arg(Cs1,area,Acc));
	[$B,$:|Cs1] -> parse_h(parse_arg(Cs1,book,Acc));
	[$C,$:|Cs1] -> parse_h(parse_arg(Cs1,composer,Acc));
	[$D,$:|Cs1] -> parse_h(parse_arg(Cs1,discography,Acc));
	[$F,$:|Cs1] -> parse_h(parse_arg(Cs1,filename,Acc));
	[$G,$:|Cs1] -> parse_h(parse_arg(Cs1,group,Acc));
	[$H,$:|Cs1] -> parse_h(parse_arg(Cs1,history,Acc));
	[$I,$:|Cs1] -> parse_h(parse_arg(Cs1,information,Acc));
	%% J?
	[$K,$:|Cs1] -> %% mandatory (LAST)
	    parse_key(Cs1,Acc);
	[$L,$:|Cs1] ->  %% mandatory (4)
	    parse_h(parse_note_length(Cs1,Acc));
	[$M,$:|Cs1] ->  %% mandatory (3)
	    parse_h(parse_meter(Cs1,Acc));
	%% special hack c: p: midi channel/program
	[$c,$:|Cs1=[C|_]] when ?is_digit(C) ->
	    parse_h(parse_iarg(Cs1,[midi,channel],1,Acc));
	[$p,$:|Cs1=[C|_]] when ?is_digit(C) ->
	    parse_h(parse_iarg(Cs1,[midi,program],1,Acc));

	[$N,$:|Cs1] -> parse_h(parse_arg(Cs1,notes,Acc));
	[$O,$:|Cs1] -> parse_h(parse_arg(Cs1,origin,Acc));
	[$P,$:|Cs1] -> parse_h(parse_arg(Cs1,part,Acc));
	[$Q,$:|Cs1] -> parse_h(parse_tempo(Cs1,Acc));
	[$R,$:|Cs1] -> parse_h(parse_arg(Cs1,rhythm,Acc));
	[$S,$:|Cs1] -> parse_h(parse_arg(Cs1,source,Acc));
	[$T,$:|Cs1] ->
	    %% mandatory (2)
	    parse_h(parse_arg(Cs1,title,Acc));
	[$U,$:|Cs1] -> parse_h(parse_arg(Cs1,u,Acc));
	[$V,$:|Cs1] -> parse_h(parse_voice(Cs1,Acc));
	[$W,$:|Cs1] -> parse_h(parse_arg(Cs1,words,Acc));
	[$w,$:|Cs1] -> parse_h(parse_arg(Cs1,w,Acc));
	[$X,$:|Cs1] ->  %% mandatory (FIRST) check this?
	    parse_h(parse_index(Cs1,Acc));
	%% Y?
	[$Z,$:|Cs1] -> parse_h(parse_arg(Cs1,transcriber,Acc))
    end.

parse_b({Cs,Acc}) ->
    parse_b(Cs,Acc).

parse_b(Cs,Acc) ->
    case Cs of
	[] -> {[],Acc};
	[$X,$:|_] -> {Cs,Acc};
	[$\s|Cs1] -> parse_b(Cs1,Acc); %% fixme, groups
	[$\t|Cs1] -> parse_b(Cs1,Acc); %% fixme, groups
	[$\\,$\n|Cs1] -> new_line(), parse_b(Cs1,Acc);  %% line continue
	[$\n|Cs1]   -> new_line(), parse_b(Cs1,Acc); %% fixme: delete?
	[$\v|Cs1]   -> parse_b(Cs1,Acc);  %% special for inline key/values
	[$\r|Cs1]   -> parse_b(Cs1,Acc);
	[$%,$%,$M,$I,$D,$I,$\s|Cs1] ->  %% fixme, handle more stylesheet
	    {MIDI,Cs2} = get_line(Cs1),
	    case erl_scan:string(MIDI) of
		{ok,[{atom,_,channel},{integer,_,Chan}],_} ->
		    parse_b(Cs2,[{midi,channel,Chan}|Acc]);
		{ok,[{atom,_,program},{integer,_,Prog}],_} ->
		    parse_b(Cs2,[{midi,program,Prog}|Acc]);
		{ok,[{atom,_,transpose},{integer,_,Offset}],_} ->
		    parse_b(Cs2,[{midi,transpose,Offset}|Acc]);
		{ok,Ts,_} ->
		    parse_b(Cs2,[{midi,Ts}|Acc])
	    end;
	[$%|Cs1] -> {_Line,Cs2} = get_line(Cs1), parse_b(Cs2,Acc);
	%% special short midi
	[$c,$:|Cs1=[C|_]] when ?is_digit(C) ->
	    parse_b(parse_iarg(Cs1,[midi,channel],1,Acc));
	[$p,$:|Cs1=[C|_]] when ?is_digit(C) ->
	    parse_b(parse_iarg(Cs1,[midi,program],1,Acc));
	
	%% headers in body
	[$P,$:|Cs1] -> parse_b(parse_arg(Cs1,part,Acc));
	[$V,$:|Cs1] -> parse_b(parse_voice(Cs1,Acc));
	[$w,$:|Cs1] -> parse_b(parse_arg(Cs1,w,Acc));
	[$K,$:|Cs1] -> parse_b(parse_key(Cs1,Acc));
	[$L,$:|Cs1] -> parse_b(parse_note_length(Cs1,Acc));

	%% Notes
	[$C|Cs1]    -> parse_b(Cs1,[{note,?C,{1,1}}|Acc]);
	[$D|Cs1]    -> parse_b(Cs1,[{note,?D,{1,1}}|Acc]);
	[$E|Cs1]    -> parse_b(Cs1,[{note,?E,{1,1}}|Acc]);
	[$F|Cs1]    -> parse_b(Cs1,[{note,?F,{1,1}}|Acc]);
	[$G|Cs1]    -> parse_b(Cs1,[{note,?G,{1,1}}|Acc]);
	[$A|Cs1]    -> parse_b(Cs1,[{note,?A,{1,1}}|Acc]);
	[$B|Cs1]    -> parse_b(Cs1,[{note,?B,{1,1}}|Acc]);
	[$c|Cs1]    -> parse_b(Cs1,[{note,?c,{1,1}}|Acc]);
	[$d|Cs1]    -> parse_b(Cs1,[{note,?d,{1,1}}|Acc]);
	[$e|Cs1]    -> parse_b(Cs1,[{note,?e,{1,1}}|Acc]);
	[$f|Cs1]    -> parse_b(Cs1,[{note,?f,{1,1}}|Acc]);
	[$g|Cs1]    -> parse_b(Cs1,[{note,?g,{1,1}}|Acc]);
	[$a|Cs1]    -> parse_b(Cs1,[{note,?a,{1,1}}|Acc]);
	[$b|Cs1]    -> parse_b(Cs1,[{note,?b,{1,1}}|Acc]);

	%% Drums (hack)
	[$@|Cs1] -> parse_drum(Cs1,Acc);

	[$z|Cs1]    -> parse_b(Cs1,[{rest,{1,1}}|Acc]);
	[$x|Cs1]    -> parse_b(Cs1,[{space,{1,1}}|Acc]);
	[$'|Cs1]    -> parse_b(Cs1,[tick|Acc]);
	[$,|Cs1]    -> parse_b(Cs1,[comma|Acc]);
	[$|,$||Cs1]  -> parse_b(Cs1,[double_bar|Acc]);
	[$|,$]|Cs1]  -> parse_b(Cs1,[thin_thick|Acc]);
	[$[,$||Cs1]  -> parse_b(Cs1,[thick_thin|Acc]);
	[$|,$:|Cs1] ->  parse_b(parse_iarg(Cs1,[repeat],1,Acc));
	[$:,$||Cs1=[C|_]] when ?is_digit(C) ->
	    parse_b(parse_iarg(Cs1,[alt_end],1,[repeat_end|Acc]));
	[$:,$||Cs1]  ->
	    parse_b(Cs1,[repeat_end|Acc]);
	[$:,$:|Cs1]  -> parse_b(Cs1,[{repeat,1},repeat_end|Acc]);
	[$||Cs1=[C|_]] when ?is_digit(C) ->
	    parse_b(parse_iarg(Cs1,[alt_end],1,[bar|Acc]));
	[$||Cs1]     -> parse_b(Cs1,[bar|Acc]);
	[$[|Cs1=[C|_]] when ?is_digit(C) ->
	    parse_b(parse_iarg(Cs1,[repeat],1,Acc));
	[$(|Cs1=[C|_]] when ?is_digit(C) ->
	    parse_b(parse_iarg(Cs1,[tuple],1,Acc));
	[$[ | Cs1]  -> parse_block(Cs1,[],Acc);
	[${ | Cs1]  -> parse_grace(Cs1,[],Acc);
	[$( | Cs1]  -> parse_slur(Cs1,Acc);
	[$" | Cs1]  -> parse_str(Cs1,[],Acc);
	[$! | Cs1]  -> parse_symbol(Cs1,[],Acc);
	[$>|Cs1]    -> parse_b(Cs1,[dot|Acc]);
	[$<|Cs1]    -> parse_b(Cs1,[dotm|Acc]);
	[$^|Cs1]    -> parse_b(Cs1,[sharpen|Acc]);     %% prefix
	[$=|Cs1]    -> parse_b(Cs1,[naturalise|Acc]);  %% prefix
	[$_|Cs1]    -> parse_b(Cs1,[flatten|Acc]);     %% prefix
	[$u|Cs1]    -> parse_b(Cs1,[up_bow|Acc]);      %% suffix
	[$v|Cs1]    -> parse_b(Cs1,[down_bow|Acc]);    %% suffix
	[$.|Cs1]    -> parse_b(Cs1,[staccato|Acc]);    %% prefix
	[$-|Cs1]    -> parse_b(Cs1,[tie|Acc]);         %% binary
	[$~|Cs1]    -> parse_b(Cs1,[ornament|Acc]);    %% prefix
	[$&|Cs1]    -> parse_b(Cs1,[short_part|Acc]);  %% binary (FIXME!)
	[$/|Cs1] -> parse_length_shorten(Cs1,Acc);
	Cs1=[C|_] when ?is_digit(C) -> parse_length_extend(Cs1,Acc)
    end.

parse_drum(Cs, Acc) ->
    case drum_name(Cs,[]) of
	{0,Cs1} -> parse_b(Cs1, Acc);
	{D,Cs1} -> parse_b(Cs1,[{note,D,{1,1}}|Acc])
    end.

drum_name([$@|Cs],Acc) ->
    case string:to_lower(lists:reverse(Acc)) of
	"bassdrum1"  -> {?GM_DRUM_Bass_Drum_1,Cs};
	"bassdrum2"  -> {?GM_DRUM_Bass_Drum_2,Cs};
	"bassdrum"   -> {?GM_DRUM_Bass_Drum_1,Cs};
	"bass"       -> {?GM_DRUM_Bass_Drum_1,Cs};
	"b"          -> {?GM_DRUM_Bass_Drum_1,Cs};
	"snaredrum1" -> {?GM_DRUM_Snare_Drum_1,Cs};
	"snaredrum2" -> {?GM_DRUM_Snare_Drum_2,Cs};
	"snaredrum"  -> {?GM_DRUM_Snare_Drum_1,Cs};
	"snare"      -> {?GM_DRUM_Snare_Drum_1,Cs};
	"s"          -> {?GM_DRUM_Snare_Drum_1,Cs};

	"openhihat"  -> {?GM_DRUM_Open_Hi_hat,Cs};
	"oh"         -> {?GM_DRUM_Open_Hi_hat,Cs};
	"closedhihat" -> {?GM_DRUM_Closed_Hi_hat,Cs};
	"ch"         -> {?GM_DRUM_Closed_Hi_hat,Cs};
	"pedalhihat" -> {?GM_DRUM_Pedal_Hi_hat,Cs};
	"ph"         -> {?GM_DRUM_Pedal_Hi_hat,Cs};

	"lowtom1"  -> {?GM_DRUM_Low_Tom_1,Cs};
	"lowtom2"  -> {?GM_DRUM_Low_Tom_2,Cs};
	"lowtom"   -> {?GM_DRUM_Low_Tom_1,Cs};
	"lt"       -> {?GM_DRUM_Low_Tom_1,Cs};
	"midtom1"  -> {?GM_DRUM_Mid_Tom_1,Cs};
	"midtom2"  -> {?GM_DRUM_Mid_Tom_2,Cs};
	"midtom"   -> {?GM_DRUM_Mid_Tom_1,Cs};
	"mt"       -> {?GM_DRUM_Mid_Tom_1,Cs};
	"hightom1" -> {?GM_DRUM_High_Tom_1,Cs};
	"hightom2" -> {?GM_DRUM_High_Tom_2,Cs};
	"hightom"  -> {?GM_DRUM_High_Tom_1,Cs};
	"ht"       -> {?GM_DRUM_High_Tom_1,Cs};

	"crashcymbal1" -> {?GM_DRUM_Crash_Cymbal_1,Cs};
	"crashcymbal2" -> {?GM_DRUM_Crash_Cymbal_2,Cs};
	"crashcymbal" -> {?GM_DRUM_Crash_Cymbal_1,Cs};
	"ridecymbal1" -> {?GM_DRUM_Ride_Cymbal_1,Cs};
	"ridecymbal2" -> {?GM_DRUM_Ride_Cymbal_2,Cs};
	"ridecymbal" -> {?GM_DRUM_Ride_Cymbal_1,Cs};
	"splashcymbal" -> {?GM_DRUM_Splash_Cymbal,Cs};

	"chinesecymbal" -> {?GM_DRUM_Chinese_Cymbal,Cs};
	"sidestick" -> {?GM_DRUM_Side_Stick,Cs};
	"handclap" -> {?GM_DRUM_Hand_Clap,Cs};
	"ridebell" -> {?GM_DRUM_Ride_Bell,Cs};
	"tambourine" -> {?GM_DRUM_Tambourine,Cs};
	"cowbell" -> {?GM_DRUM_Cowbell,Cs};
	"vibraslap" -> {?GM_DRUM_Vibra_Slap,Cs};
	"highbongo" -> {?GM_DRUM_High_Bongo,Cs};
	"lowbongo" -> {?GM_DRUM_Low_Bongo,Cs};
	"mutehighconga" -> {?GM_DRUM_Mute_High_Conga,Cs};
	"openhighconga" -> {?GM_DRUM_Open_High_Conga,Cs};
	"lowconga" -> {?GM_DRUM_Low_Conga,Cs};
	"hightimbale" -> {?GM_DRUM_High_Timbale,Cs};
	"lowtimbale" -> {?GM_DRUM_Low_Timbale,Cs};
	"highagogo" -> {?GM_DRUM_High_Agogo,Cs};
	"lowagogo" -> {?GM_DRUM_Low_Agogo,Cs};
	"cabasa" -> {?GM_DRUM_Cabasa,Cs};
	"maracas" -> {?GM_DRUM_Maracas,Cs};
	"shortwhistle" -> {?GM_DRUM_Short_Whistle,Cs};
	"longwhistle" -> {?GM_DRUM_Long_Whistle,Cs};
	"shortguiro" -> {?GM_DRUM_Short_Guiro,Cs};
	"longguiro" -> {?GM_DRUM_Long_Guiro,Cs};
	"claves" -> {?GM_DRUM_Claves,Cs};
	"highwoodblock" -> {?GM_DRUM_High_Wood_Block,Cs};
	"lowwoodblock" -> {?GM_DRUM_Low_Wood_Block,Cs};
	"mutecuica" -> {?GM_DRUM_Mute_Cuica,Cs};
	"opencuica" -> {?GM_DRUM_Open_Cuica,Cs};
	"mutetriangle" -> {?GM_DRUM_Mute_Triangle,Cs};
	"opentriangle" -> {?GM_DRUM_Open_Triangle,Cs};
	_ -> {0,Cs}
    end;
drum_name([$_|Cs],Acc) -> drum_name(Cs, Acc);
drum_name([$-|Cs],Acc) -> drum_name(Cs, Acc);
drum_name([C|Cs],Acc) when
      C >= $0, C =< $9;
      C >= $a, C =< $z;
      C >= $A, C =< $Z -> drum_name(Cs, [C|Acc]);
drum_name(Cs,Acc) -> 
    io:format("warning: drum name error ~s\n", [lists:reverse(Acc)]),
    {0,Cs}.

drum_(Cs,D,Acc) ->
    parse_b(Cs,[{note,D,{1,1}}|Acc]).

%% parse length of note/rest and combine into a note with length
%% {note,MidiValue,{A,B}}

%% assume first character of Cs to be a digit
parse_length_extend(Cs,Acc) ->
    {L1,Cs1} = parse_unsigned(Cs),
    length_note(Cs1,{L1,1},Acc,[]).

%% we have seen a / character
parse_length_shorten(Cs,Acc) ->
    case parse_unsigned(Cs) of
	false    -> length_note(Cs,{1,2},Acc,[]);
	{L1,Cs1} -> length_note(Cs1,{1,L1},Acc,[])
    end.

%% multiply rest or note length with Len, may be suffixed with ' or ,
length_note(Cs, Len, [tick|Acc],Bs) ->
    length_note(Cs, Len, Acc, [tick|Bs]);
length_note(Cs, Len, [comma|Acc],Bs) ->
    length_note(Cs, Len, Acc, [comma|Bs]);
length_note(Cs, Len, [{note,N,L}|Acc],Bs) ->
    parse_b(Cs,lists:reverse([{note,N,mult(L,Len)}|Bs],Acc));
length_note(Cs, Len, [{rest,L}|Acc],[]) -> %% can not be prefixed
    parse_b(Cs,[{rest,mult(L,Len)}|Acc]);
length_note(Cs, Len, [{space,L}|Acc],[]) -> %% can not be prefixed
    parse_b(Cs,[{space,mult(L,Len)}|Acc]).


parse_iarg([C1,C2|Cs1],Ts,_D,Acc) when ?is_digit(C1), ?is_digit(C2) ->
    {Cs1,[build_tuple(Ts, (C1-$0)*10+(C2-$0))|Acc]};
parse_iarg([C1|Cs1],Ts,_D,Acc) when ?is_digit(C1) ->
    {Cs1,[build_tuple(Ts,(C1-$0))|Acc]};
parse_iarg(Cs1,Ts,D,Acc) ->
    {Cs1,[build_tuple(Ts,D)|Acc]}.

%% if '_' is an element the replace '_' with Value
%% otherwise append Value to Tuple
build_tuple(Ts, Value) ->
    case index('_', Ts) of
	0 -> list_to_tuple(Ts++[Value]);
	I -> setelement(I, list_to_tuple(Ts), Value)
    end.

%% find element H in list L and return position (1 based) otr
%% 0 if not found
index(H, L) -> index(H,L,1).
index(H, [H|_], I) -> I;
index(H, [_|L], I) -> index(H, L, I+1);
index(_H, [], _) -> 0.

parse_block([$]|Cs],Bs,Acc) ->
    Bs1 = tr(lists:reverse(Bs), #{ $\s => $\v }),
    %% maybe call parse and then decide?
    case Bs1 of 
	[_C,$:|_] -> %% assume block
	    {[],Acc1} = parse_h(Bs1,Acc),
	    parse_b(Cs,Acc1);
	_ -> %% assume chord
	    {[],RNs} = parse_b(Bs1,[]),
	    parse_b(Cs,[{chord,lists:reverse(RNs)}|Acc])
    end;
parse_block([C|Cs],Bs,Acc) ->
    parse_block(Cs,[C|Bs],Acc).

	    
parse_grace([$}|Cs],Bs,Acc) ->
    Bs1 = lists:reverse(Bs),
    {[],Gs} = parse_b(Bs1,[]),
    Grace = lists:reverse(Gs),
    parse_b(Cs, [{grace,Grace}|Acc]);
parse_grace([C|Cs],Bs,Acc) ->
    parse_grace(Cs,[C|Bs],Acc);
parse_grace([],Bs,Acc) ->
    Bs1 = lists:reverse(Bs),
    {[],Gs} = parse_b(Bs1,[]),
    Grace = lists:reverse(Gs),
    parse_b([], [{grace,Grace}|Acc]).

%% Parse slur, may be nested and contain annotation and symbols!!!
parse_slur(Cs1,Acc) ->
    parse_slur(Cs1,[],0,false,false,Acc).

parse_slur([$)|Cs],Bs,0,false,false,Acc) ->
    Bs1 = lists:reverse(Bs),
    {[],As} = parse_b(Bs1,[]),
    Slur = lists:reverse(As),
    parse_b(Cs, [{slur,Slur}|Acc]);
%% skip scan inside symbol
parse_slur([$!|Cs],Bs,L,false,false,Acc) ->
    parse_slur(Cs,[$!|Bs],L,true,false,Acc);
parse_slur([$!|Cs],Bs,L,true,false,Acc) ->
    parse_slur(Cs,[$!|Bs],L,false,false,Acc);
parse_slur([C|Cs],Bs,L,true,false,Acc) ->
    parse_slur(Cs,[C|Bs],L,true,false,Acc);
%% skip scan inside annotaion
parse_slur([$"|Cs],Bs,L,false,false,Acc) ->
    parse_slur(Cs,[$"|Bs],L,false,true,Acc);
parse_slur([$"|Cs],Bs,L,false,true,Acc) ->
    parse_slur(Cs,[$"|Bs],L,false,false,Acc);
parse_slur([C|Cs],Bs,L,false,true,Acc) ->
    parse_slur(Cs,[C|Bs],L,false,true,Acc);

parse_slur([$(,C|Cs],Bs,L,false,false,Acc) when ?is_digit(C) ->
    parse_slur(Cs,[C,$(|Bs],L,false,false,Acc);
parse_slur([$(|Cs],Bs,L,false,false,Acc) ->
    parse_slur(Cs,[$(|Bs],L+1,false,false,Acc); 
parse_slur([$)|Cs],Bs,L,false,false,Acc) ->
    parse_slur(Cs,[$)|Bs],L-1,false,false,Acc);
parse_slur([C|Cs],Bs,L,false,false,Acc) ->
    parse_slur(Cs,[C|Bs],L,false,false,Acc);
parse_slur([],Bs,1,false,false,Acc) ->
    Bs1 = lists:reverse(Bs),
    {[],As} = parse_b(Bs1,[]),
    Grace = lists:reverse(As),
    parse_b([], [{slur,Grace}|Acc]).

parse_symbol([$!|Cs],Bs,Acc) ->
    parse_b(Cs,[{symbol,lists:reverse(Bs)}|Acc]);
parse_symbol([C|Cs],Bs,Acc) ->
    parse_symbol(Cs,[C|Bs],Acc);
parse_symbol([],Bs,Acc) ->
    parse_b([],[{symbol,lists:reverse(Bs)}|Acc]).

parse_str([$"|Cs],Bs,Acc) ->
    parse_str_(lists:reverse(Bs),skip_ws(Cs),Acc);
parse_str([C|Cs], Bs, Acc) ->
    parse_str(Cs,[C|Bs],Acc);
parse_str([], Bs, Acc) ->
    parse_str_(lists:reverse(Bs),[],Acc).

skip_ws([$\s|Cs]) -> skip_ws(Cs);
skip_ws([$\t|Cs]) -> skip_ws(Cs);
skip_ws(Cs) -> Cs.

parse_str_([$^|String],Cs,Acc) ->
    parse_b(Cs, [{annotate,above,String}|Acc]);
parse_str_([$_|String],Cs,Acc) ->
    parse_b(Cs, [{annotate,below,String}|Acc]);
parse_str_([$<|String],Cs,Acc) ->
    parse_b(Cs, [{annotate,left,String}|Acc]);
parse_str_([$>|String],Cs,Acc) ->
    parse_b(Cs, [{annotate,right,String}|Acc]);
parse_str_([$@|String],Cs,Acc) ->
    parse_b(Cs, [{annotate,any,String}|Acc]);

parse_str_(String,[$| | Cs],Acc) ->
    parse_b(Cs, [bar,{part,String}|Acc]);
parse_str_(String,[$[,$| | Cs],Acc) ->
    parse_b(Cs, [thick_thin,{part,String}|Acc]);

parse_str_([$~|Chord],Cs,Acc) ->
    parse_guitar_chord_(Chord,"~",Cs,Acc);
parse_str_(Chord,Cs,Acc) ->
    parse_guitar_chord_(Chord,"",Cs,Acc).

parse_guitar_chord_(String,P,Cs,Acc) ->
    case String of
	[C,$#|Type] when C >= $A, C =< $G ->
	    parse_guitar_type_(Type,P++[C],[$#],Cs,Acc);
	[C,$b|Type] when C >= $A, C =< $G ->
	    parse_guitar_type_(Type,P++[C],[$b],Cs,Acc);
        [C|Type] when C >= $A, C =< $G ->
	    parse_guitar_type_(Type,P++[C],[],Cs,Acc);
	_ ->
	    parse_b(Cs,[{annotate,any,P++String}|Acc])
    end.

parse_guitar_type_(Type,Note,Accidental,Cs,Acc) ->
    case Type of
	"min7"++Bass -> 
	    parse_guitar_bass(Bass,"min7",Note,Accidental,Cs,Acc);
	"maj7"++Bass -> 
	    parse_guitar_bass(Bass,"maj7",Note,Accidental,Cs,Acc);
	"m7"++Bass -> 
	    parse_guitar_bass(Bass,"min7",Note,Accidental,Cs,Acc);
	"min"++Bass -> 
	    parse_guitar_bass(Bass,"min",Note,Accidental,Cs,Acc);
	"maj"++Bass -> 
	    parse_guitar_bass(Bass,"maj",Note,Accidental,Cs,Acc);
	"m"++Bass -> 
	    parse_guitar_bass(Bass,"min",Note,Accidental,Cs,Acc);
	"sus"++Bass ->
	    parse_guitar_bass(Bass,"sus",Note,Accidental,Cs,Acc);
	"aug"++Bass -> 
	    parse_guitar_bass(Bass,"aug",Note,Accidental,Cs,Acc);
	"dim7"++Bass -> 
	    parse_guitar_bass(Bass,"dim7",Note,Accidental,Cs,Acc);
	"dim"++Bass ->
	    parse_guitar_bass(Bass,"dim",Note,Accidental,Cs,Acc);
	"6"++Bass ->
	    parse_guitar_bass(Bass,"6",Note,Accidental,Cs,Acc);
	"7"++Bass ->
	    parse_guitar_bass(Bass,"7",Note,Accidental,Cs,Acc);
	"9"++Bass ->
	    parse_guitar_bass(Bass,"9",Note,Accidental,Cs,Acc);
	"11"++Bass ->
	    parse_guitar_bass(Bass,"11",Note,Accidental,Cs,Acc);
	Bass ->
	    parse_guitar_bass(Bass,"maj",Note,Accidental,Cs,Acc)
    end.

parse_guitar_bass([$/|Bass],Type,Note,Accidental,Cs,Acc) ->
    parse_b(Cs, [{guitar,Note,Accidental,Type,Bass}|Acc]);
parse_guitar_bass([],Type,Note,Accidental,Cs,Acc) ->
    parse_b(Cs, [{guitar,Note,Accidental,Type,[]}|Acc]).

parse_index(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    Value = list_to_integer(string:trim(Line)),
    %% io:format("line:~w, index ~w\n", [get(line),Value]),
    {Cs1,[{index,Value}|Acc]}.

%% voice
%% V: <integer> [param=value ...]
parse_voice(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    case erl_scan:string(Line) of
	{ok,[I|Ps],_} ->
	    {Cs1,[{voice,voice_id(I),voice_params(Ps)}|Acc]}
    end.

voice_id({integer,_,I}) -> I;
voice_id({atom,_,A}) -> A;
voice_id({var,_,V}) -> V;
voice_id({string,_,S}) -> S.

%% name(nm) subname(snm) stem, clef
voice_params([{atom,_,Name},{'=',_},{string,_,X}|Ps]) ->
    [{Name,X}|voice_params(Ps)];
voice_params([{atom,_,Name},{'=',_},{atom,_,X}|Ps]) ->
    [{Name,X}|voice_params(Ps)];
voice_params([{atom,_,Name},{'=',_},{integer,_,X}|Ps]) ->
    [{Name,X}|voice_params(Ps)];
voice_params([]) ->
    [].

%% L: A/B | <name>
parse_note_length(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    parse_note_length(Line,Cs1,Acc).

parse_note_length(Line,Cs,Acc) ->
    Arg = string:to_lower(string:trim(Line)),
    case erl_scan:string(Arg) of
	{ok,[{integer,_,A},{'/',_},{integer,_,B}],_} ->
	    {Cs,[{unit_note_length,A,B}|Acc]};
	{ok,[{atom,_,Name}],_} ->
	    case Name of
		jig -> {Cs,[{unit_note_length,1,8}|Acc]};
		reel -> {Cs,[{unit_note_length,1,8}|Acc]};
		schottische -> {Cs,[{unit_note_length,1,8}|Acc]};
		waltz -> {Cs,[{unit_note_length,1,4}|Acc]};
		polka -> {Cs,[{unit_note_length,1,8}|Acc]};
		bourree -> {Cs,[{unit_note_length,1,8}|Acc]}
	    end
    end.

%% M: A/B | C
parse_meter(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    parse_meter(Line,Cs1,Acc).

parse_meter(Line,Cs,Acc) ->
    Arg = string:to_lower(string:trim(Line)),
    case erl_scan:string(Arg) of
	{ok,[{integer,_,A},{'/',_},{integer,_,B}],_} ->
	    {Cs,[{meter,A,B}|Acc]};
	{ok,[{atom,_,C}],_} ->
	    {Cs,[{meter,C}|Acc]};
	{ok,[{atom,_,C},{'|',_}],_} ->
	    {Cs,[{meter,[C,'|']}|Acc]}
    end.

%% Q: [A/B =] T  beats per default note note length
parse_tempo(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    parse_tempo(Line,Cs1,Acc).

parse_tempo(String,Cs,Acc) ->
    case erl_scan:string(String) of
	{ok,[{integer,_,A},{'/',_},{integer,_,B},{'=',_},{integer,_,T}],_} ->
	    {Cs, [{tempo,{A,B},T}|Acc]};
	{ok,[{integer,_,T}],_} ->
	    {Cs, [{tempo,T}|Acc]}
    end.

parse_key(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    Arg = remove_blanks(Line),
    case Arg of
	[] -> {Cs1,[{key,none,""}|Acc]};
	[$n,$o,$n,$e] -> {Cs1,[{key,none,""}|Acc]};

	[C,$b,$m,$a,$j|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"bmaj"],to_modal(Modal)}|Acc]};
	[C,$#,$m,$a,$j|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"#maj"],to_modal(Modal)}|Acc]};
	[C,$m,$a,$j|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"maj"],to_modal(Modal)}|Acc]};

	[C,$b,$m,$i,$n|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"bmin"],to_modal(Modal)}|Acc]};
	[C,$#,$m,$i,$n|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"#min"],to_modal(Modal)}|Acc]};
	[C,$m,$i,$n|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"min"],to_modal(Modal)}|Acc]};

	[C,$b,$m,$i,$x|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"bmix"],to_modal(Modal)}|Acc]};
	[C,$#,$m,$i,$x|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"#mix"],to_modal(Modal)}|Acc]};
	[C,$m,$i,$x|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"mix"],to_modal(Modal)}|Acc]};

	[C,$b,$m|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"bmin"],to_modal(Modal)}|Acc]};
	[C,$#,$m|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"#min"],to_modal(Modal)}|Acc]};
	[C,$m|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"min"],to_modal(Modal)}|Acc]};

	[C,$b|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"bmaj"],to_modal(Modal)}|Acc]};
	[C,$#|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"#maj"],to_modal(Modal)}|Acc]};
	[C|Modal] when C >= $A, C =< $G ->
	    {Cs1,[{key,[C|"maj"],to_modal(Modal)}|Acc]}
    end.

to_modal(String) ->
    case string:to_lower(String) of
	"" -> undefined;
	"lydian" -> lyd;
	"lyd" -> lyd;
	"ionian" -> ion;
	"ion" -> ion;
	"mixolydian" -> mix;
	"mix" -> mix;
	"dorian" -> dor; 
	"dor" -> dor;
	"aeolian" -> aeo;
	"aeo" -> aeo;
	"phrygian" -> phr;
	"phr" -> phr;
	"locrian" -> loc;
	"loc" -> loc
    end.

parse_arg(Cs,Key,Acc) ->
    {Line,Cs1} = get_line(Cs),
    Value = string:trim(Line),  %% FIXME trim trailing comment as well
    {Cs1,[{Key,Value}|Acc]}.

%% parse unsigned decimal integer
parse_unsigned([C|Cs]) when ?is_digit(C) ->
    parse_unsigned(Cs, (C-$0));
parse_unsigned(_) ->
    false.

parse_unsigned([C|Cs],N) when ?is_digit(C) ->
    parse_unsigned(Cs, N*10+(C-$0));
parse_unsigned(Cs, N) -> {N,Cs}.

new_line() ->
    case get(line) of
	undefined -> put(line, 2);
	I -> put(line,I+1)
    end.

get_line(Cs) ->
    get_line(Cs,[]).

get_line([$\r,$\n|Cs],Acc) -> new_line(), {lists:reverse(Acc), Cs};
get_line([$\n|Cs],Acc) -> new_line(), {lists:reverse(Acc), Cs};
get_line([$\r,$\v|Cs],Acc) -> {lists:reverse(Acc), Cs};
get_line([$\v|Cs],Acc) -> {lists:reverse(Acc), Cs};
get_line([C|Cs],Acc) -> get_line(Cs,[C|Acc]);
get_line([],Acc) -> {lists:reverse(Acc), []}.

%% not trim, but remove all blanks
remove_blanks([$\s|Cs]) ->remove_blanks(Cs);
remove_blanks([$\t|Cs]) ->remove_blanks(Cs);
remove_blanks([$\r|Cs]) ->remove_blanks(Cs);
remove_blanks([C|Cs]) -> [C|remove_blanks(Cs)];
remove_blanks([]) -> [].

%% translate char
tr([C|Cs], Map) ->
    case maps:find(C, Map) of
	error  -> [C|tr(Cs,Map)];
	{ok,delete} -> tr(Cs,Map);
	{ok,D} -> [D|tr(Cs,Map)]
    end;
tr([],_Map) -> [].


%% map notes in keysigntures (fixme: make this more compact)
map_key("none") -> #{};

map_key("Cmaj"++_) -> map_sharp([]);
map_key("Amin"++_) -> map_sharp([]);

map_key("Gmaj"++_)  -> map_sharp([?F]);
map_key("Emin"++_)  -> map_sharp([?F]);

map_key("Dmaj"++_)  -> map_sharp([?F,?C]);
map_key("Bmin"++_)  -> map_sharp([?F,?C]);

map_key("Amaj"++_)  -> map_sharp([?F,?C,?G]);
map_key("F#min"++_) -> map_sharp([?F,?C,?G]);

map_key("Emaj"++_)  -> map_sharp([?F,?C,?G,?D]);
map_key("C#min"++_) -> map_sharp([?F,?C,?G,?D]);

map_key("Bmaj"++_)  -> map_sharp([?F,?C,?G,?D,?A]);
map_key("G#mon"++_) -> map_sharp([?F,?C,?G,?D,?A]);

map_key("F#maj"++_) -> map_sharp([?F,?C,?G,?D,?A,?E]);
map_key("D#min"++_) -> map_sharp([?F,?C,?G,?D,?A,?E]);

map_key("C#maj"++_) -> map_sharp([?F,?C,?G,?D,?A,?E,?B]);
map_key("A#min"++_) -> map_sharp([?F,?C,?G,?D,?A,?E,?B]);

map_key("Fmaj"++_)  ->  map_flat([?B]);
map_key("Dmin"++_)  ->  map_flat([?B]);

map_key("Bbmaj"++_) -> map_flat([?B,?E]);
map_key("Gmin"++_)  -> map_flat([?B,?E]);

map_key("Ebmaj"++_) -> map_flat([?B,?E,?A]);
map_key("Cmin"++_)  -> map_flat([?B,?E,?A]);

map_key("Abmaj"++_) -> map_flat([?B,?E,?A,?D]);
map_key("Fmin"++_)  -> map_flat([?B,?E,?A,?D]);

map_key("Dbmaj"++_) -> map_flat([?B,?E,?A,?D,?G]);
map_key("Bbmin"++_) -> map_flat([?B,?E,?A,?D,?G]);

map_key("Gbmaj"++_) -> map_flat([?B,?E,?A,?D,?G,?C]);
map_key("Ebmin"++_) -> map_flat([?B,?E,?A,?D,?G,?C]);

map_key("Cbmaj"++_) -> map_flat([?B,?E,?A,?D,?G,?C,?F]);
map_key("Abmin"++_) -> map_flat([?B,?E,?A,?D,?G,?C,?F]).

mult({A,B},{C,D}) -> {A*C,B*D}.

map_sharp(Ns) ->
    map_sharp(Ns, #{}).

map_sharp([N|Ns],M0) ->
    M1 = lists:foldl(
	   fun(Ni,Mi) -> Mi#{ Ni => Ni+1} end, 
	   M0, lists:seq(N,126,12)),
    M2 = lists:foldl(
	   fun(Ni,Mi) -> Mi#{ Ni => Ni+1} end, 
	   M1, lists:seq(N-12,0,-12)),
    map_sharp(Ns, M2);
map_sharp([], M0) -> 
    M0.

map_flat(Ns) ->
    map_flat(Ns, #{}).

map_flat([N|Ns], M0) ->
    M1 = lists:foldl(
	   fun(Ni,Mi) -> Mi#{ Ni => Ni-1} end, 
	   M0, lists:seq(N,127,12)),
    M2 = lists:foldl(
	   fun(Ni,Mi) -> Mi#{ Ni => Ni-1} end, 
	   M1, lists:seq(N-12,1,-12)),
    map_flat(Ns, M2);
map_flat([], M0) -> 
    M0.
