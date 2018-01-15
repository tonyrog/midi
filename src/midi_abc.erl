%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    ABC format player
%%% @end
%%% Created :  7 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_abc).

-export([parse_file/1]).
-export([parse_string/1]).
-export([play_file/2, play_string/2, play/2]).
-export([play_file/3, play_string/3, play/3]).
-compile(export_all).

-include("midi.hrl").

-define(is_digit(C),   (((C) >= $0) andalso ((C) =< $9))).

%% Play ABC notation
%% 
%% LENGTH:
%%   default_note_length is the default length of a note L: A/B
%% 
%%   dotted notes i.e D>E 
%%   D is lengthened by with half note length and E is shortend by half note 
%%   if default note length is 1/8 then, D is 3/16 and E is 1/16
%%   the D<E is the way around D is 1/16 and E is 3/16,
%%
%% PITCH
%%   ^ sharpen - add 1 to the note ( prefix )
%%   _ flatten - subtract 1 from the note ( prefix )
%%   ? naturalise - temporarliy? clear the note map
%%   ' tick   - up one octave  ( suffix )
%%   , comma  - down one octave ( suffix )
%%
%%   Scale in G major: GABcde^fg
%%   Scale in G minor: GA_Bcd_efg
%%
%% use the signature field K to modify the note values
%%   Scale in G major using signtaure [K:G] GABcdefg
%%   and Scale in G minor using signature [K:Gmin] GABcdefg
%%   
%%   
%% 
play_file(Fd, File) ->
    play_file(Fd, File, first).

play_file(Fd, File, Index) ->
    case parse_file(File) of
	{error,Reason} -> {error,Reason};
	Ns -> play(Fd, Ns, Index)
    end.

parse_file(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    parse_string(binary_to_list(Bin));
	Error ->
	    Error
    end.

parse_string(String) ->
    put(line, 1),
    lists:reverse(parse(String,[])).

play_string(Fd, String) ->
    play_string(Fd, String,first).

play_string(Fd, String, Index) ->
    play(Fd, lists:reverse(parse(String,[])), Index).

play(Fd, Ns) ->
    play(Fd, Ns, first).

play(Fd, Ns, Index) ->
    midi:program_change(Fd, 0, ?GM_MIDI_Acoustic_Grand_Piano),
    midi:program_change(Fd, 1, ?GM_MIDI_Acoustic_Guitar_nylon),
    midi:program_change(Fd, 2, ?GM_MIDI_Acoustic_Bass),
    Ns1 = select_tune(Ns, Index),
    play_notes(Fd, Ns1, 120, {1,4}, #{}, [], []).

select_tune(Ns, Index) ->
    select_tune(Ns, Index, []).

select_tune([{index,I}|Ns], Index, Acc) when Index=:=I; Index=:=first ->
    select_take(Ns, Acc);
select_tune([{index,_}|Ns], Index, Acc) ->
    Ns1 = select_skip(Ns),
    select_tune(Ns1,Index,Acc);
select_tune([N|Ns], Index, Acc) ->
    select_tune(Ns, Index, [N|Acc]);
select_tune([], _Index, Acc) ->
    lists:reverse(Acc).

%% get all items until next {index,_}
select_take([{index,_}|_], Acc) -> lists:reverse(Acc);
select_take([N|Ns], Acc) -> select_take(Ns, [N|Acc]);
select_take([], Acc) -> lists:reverse(Acc).

%% skip all items until {index,_}
select_skip(Ns=[{index,_}|_]) -> Ns;
select_skip([_|Ns]) -> select_skip(Ns);
select_skip([]) -> [].
    

play_notes(Fd, [{repeat,N}|Ns],Tempo,DefaultNoteLen,Sig,SP,RP) ->
    play_notes(Fd, Ns, Tempo, DefaultNoteLen, Sig, [{repeat,N}|SP],[N|RP]);
play_notes(Fd, [repeat_end|Ns],Tempo,DefaultNoteLen,Sig,SP,RP) ->
    case RP of
	[] ->
	    play_notes(Fd,Ns,Tempo,DefaultNoteLen,Sig,[],[]);
	[0] ->
	    play_notes(Fd,Ns,Tempo,DefaultNoteLen,Sig,[],[]);
	[0|RP1] ->
	    play_notes(Fd,Ns,Tempo,DefaultNoteLen,Sig,[repeat_end|SP],RP1);
	[I|RP1] ->
	    {Ns1,SP1} = pop_notes([repeat_end|Ns],SP),
	    play_notes(Fd,Ns1,Tempo,DefaultNoteLen,Sig,SP1,[I-1|RP1])
    end;
play_notes(Fd,[],_Tempo,_DLen, _Sig,_SP,_RP) ->
    stop_guitar(Fd), %% stop old chord
    ok;
play_notes(Fd, Ns, Tempo, DefaultNoteLen, Sig, SP, RP) ->
    case get_note(Ns,Sig) of
	false ->
	    play_notes_(Fd,Ns,Tempo,DefaultNoteLen,Sig,SP,RP);
	{E={c,Notes},Ns1,Sig1} ->
	    play_chord(Fd,Notes,100,Tempo),
	    play_notes(Fd,Ns1,Tempo,DefaultNoteLen,Sig1,[E|SP],RP);
	{E={g,Notes,Bass},Ns1,Sig1} ->
	    play_guitar(Fd,Notes,Bass,100,Tempo),
	    play_notes(Fd,Ns1,Tempo,DefaultNoteLen,Sig1,[E|SP],RP);
	{E={n,Note,Len,As},Ns1,Sig1} ->
	    play_note(Fd,Note,As,100,Tempo,Len),
	    play_notes(Fd,Ns1,Tempo,DefaultNoteLen,Sig1,[E|SP],RP)
    end.

%% fixme: use modal?
play_notes_(Fd, [bar|Ns], Tempo, DLen, Sig, SP, RP) ->
    %% drop sharpend,flattened and neutralized notes in signature
    Sig1 = drop_note_sig(Sig),
    play_notes(Fd,Ns,Tempo,DLen,Sig1,SP,RP);
play_notes_(Fd, [{key,Key,_Modal}|Ns],Tempo,DLen,_Sig,SP,RP) ->
    io:format("key: ~s ~s\n", [Key,_Modal]),
    Sig1 = map_key(Key),
    io:format("signature: ~w\n", [Sig1]),
    play_notes(Fd,Ns,Tempo,DLen,Sig1,SP,RP);
play_notes_(Fd, [{title,T}|Ns],Tempo,DLen,Sig,SP,RP) ->
    io:format("title: ~s\n", [T]),
    play_notes(Fd,Ns,Tempo,DLen,Sig,SP,RP);
play_notes_(Fd, [{default_note_length,A,B}|Ns],Tempo,_DLen,Sig,SP,RP) ->
    play_notes(Fd,Ns,Tempo,{A,B},Sig,SP,RP);
play_notes_(Fd, [{tempo,T}|Ns],_Tempo,DLen,Sig,SP,RP) ->
    play_notes(Fd,Ns,T,DLen,Sig,SP,RP);
play_notes_(Fd, [{tempo,DLen,T}|Ns],_Tempo,_DLen,Sig,SP,RP) ->
    play_notes(Fd,Ns,T,DLen,Sig,SP,RP);
play_notes_(Fd,[_|Ns],Tempo,DLen,Sig,SP,RP) ->
    play_notes(Fd,Ns,Tempo,DLen,Sig,SP,RP).

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
get_note(Ns, Sig) ->
    case get_note_(Ns,Sig,[]) of
	false -> false;
	{E={c,_Notes},Ns1,Sig1} -> {E,Ns1,Sig1};
	{E={g,_Notes,_Bass},Ns1,Sig1} -> {E,Ns1,Sig1};
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
get_note_([E={c,_Notes}|Ns],Sig,_) -> {E,Ns,Sig};
get_note_([E={g,_Notes,_Bass}|Ns],Sig,_) -> {E,Ns,Sig};

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
get_note_([{chord,Notes}|Ns],Sig,_As) ->
    {{c,get_chord(Notes,Sig)},Ns,Sig};
get_note_([{guitar,Note,Accidental,Type,Bass}|Ns],Sig,_As) ->
    {get_guitar_chord(Note,Accidental,Type,Bass,Sig),Ns,Sig};
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
play_note(_Fd,rest,_As,_Velocity,Tempo,{D,E}) ->
    %% io:format("rest length=~w/~w\n", [D,E]),
    RestLen = trunc(((60*1000)/Tempo)*(D/E)),
    timer:sleep(RestLen);
play_note(Fd,Note,As,Velocity,Tempo,{D,E}) ->
    io:format("play note ~w [~s] length=~w/~w\n", [Note,fmt_note(As),D,E]),
    NoteLen = trunc(((60*1000)/Tempo)*(D/E)),
    midi:note_on(Fd,0,Note,Velocity),
    timer:sleep(NoteLen),
    midi:note_off(Fd,0,Note,Velocity).

%% Tempo is number of default note lengths per minute
play_chord(Fd,Ns,Velocity,Tempo) ->
    Notes = [N || {N,_}<-Ns],
    %% fixme: now we find max length, but we should check instead
    Lens = [{A/B,{A,B}} || {_,{A,B}} <- Ns],
    {_,{D,E}} = lists:max(Lens),
    %% io:format("play note ~w length=~w/~w\n", [Note,D,E]),
    NoteLen = trunc(((60*1000)/Tempo)*(D/E)),
    lists:foreach(fun(Note) -> midi:note_on(Fd,0,Note,Velocity) end, Notes),
    timer:sleep(NoteLen),
    lists:foreach(fun(Note) -> midi:note_off(Fd,0,Note,Velocity) end, Notes).

play_guitar(Fd,Ns0,Bs,Velocity,_Tempo) ->
    stop_guitar(Fd), %% stop old chord
    Ns = [N || {N,_}<-Ns0],
    %% fixme: now we find max length, but we should check instead
    %% Lens = [{A/B,{A,B}} || {_,{A,B}} <- Ns],
    %% {_,{D,E}} = lists:max(Lens),
    %% io:format("play note ~w length=~w/~w\n", [Note,D,E]),
    %% NoteLen = trunc(((60*1000)/Tempo)*(D/E)),
    io:format("Ns = ~w, Bs = ~w\n", [Ns, Bs]),
    lists:foreach(fun(N) -> midi:note_on(Fd,2,N-24,Velocity) end, Bs),
    lists:foreach(fun(N) -> midi:note_on(Fd,1,N-24,Velocity) end, Ns),
    put(g, {Ns,Bs}).

stop_guitar(Fd) ->
    case get(g) of
	undefined -> ok;
	{Ns,Bs} ->
	    lists:foreach(fun(N) -> midi:note_off(Fd,1,N-24,50) end, Ns),
	    lists:foreach(fun(N) -> midi:note_off(Fd,2,N-24,50) end, Bs),
	    erase(g)
    end.

fmt_note(As) ->
    fmt_note(As,[]).

fmt_note([],Acc) -> Acc;
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
	?b -> fmt_note(As,[$b|Acc])
    end.

%% 
%% Parse ABC format
%% FIXME: separate header / body
%% parse parts and voices:
%% [{part,"A",[{voice,V1,_},...,{voice,Vn,_}]},
%%  {part,"B",[{voice,V1,_},...,{voice,Vn,_}]},
%%  ..
%%  {part,"Z",[{voice,V1,_},...,{voice,Vn,_}]}]
%%
parse(Cs,Acc) ->
    case Cs of
	[] -> Acc;
	[$\s|Cs1]   -> parse(Cs1, Acc); %% fixme, groups
	[$\t|Cs1]   -> parse(Cs1, Acc); %% fixme, groups
	[$\\,$\n|Cs1] -> new_line(), parse(Cs1, Acc);  %% line continue
	[$\n|Cs1]   -> new_line(), parse(Cs1, Acc); %% fixme: delete?
	[$\v|Cs1]   -> parse(Cs1, Acc);
	[$\r|Cs1]   -> parse(Cs1, Acc);
	[$%|Cs1] -> {_Line,Cs2} = get_line(Cs1), parse(Cs2,Acc);

	[$A,$:|Cs1] -> parse_arg(Cs1,area,Acc);
	[$B,$:|Cs1] -> parse_arg(Cs1,book,Acc);
	[$C,$:|Cs1] -> parse_arg(Cs1,composer,Acc);
	[$D,$:|Cs1] -> parse_arg(Cs1,discography,Acc);
	[$F,$:|Cs1] -> parse_arg(Cs1,filename,Acc);
	[$G,$:|Cs1] -> parse_arg(Cs1,group,Acc);
	[$H,$:|Cs1] -> parse_arg(Cs1,history,Acc);
	[$I,$:|Cs1] -> parse_arg(Cs1,information,Acc);
	%% J?
	[$K,$:|Cs1] -> parse_key(Cs1, Acc);            %% mandatory (LAST)	
	[$L,$:|Cs1] -> parse_note_length(Cs1, Acc);    %% mandatory (4)
	[$M,$:|Cs1] -> parse_meter(Cs1, Acc);          %% mandatory (3)
	[$N,$:|Cs1] -> parse_arg(Cs1,notes, Acc);
	[$O,$:|Cs1] -> parse_arg(Cs1,origin, Acc);
	[$P,$:|Cs1] -> parse_arg(Cs1,parts,Acc);
	[$Q,$:|Cs1] -> parse_tempo(Cs1,Acc);
	[$R,$:|Cs1] -> parse_arg(Cs1,rhythm,Acc);
	[$S,$:|Cs1] -> parse_arg(Cs1,source, Acc);
	[$T,$:|Cs1] -> parse_arg(Cs1, title, Acc);     %% mandatory (2)
	[$U,$:|Cs1] -> parse_arg(Cs1, u, Acc);  %% what?
	[$V,$:|Cs1] -> parse_voice(Cs1, Acc);
	[$W,$:|Cs1] -> parse_arg(Cs1, words, Acc);
	[$w,$:|Cs1] -> parse_arg(Cs1, w, Acc);
	[$X,$:|Cs1] -> parse_index(Cs1,Acc);     %% mandatory (FIRST)
	%% Y?
	[$Z,$:|Cs1] -> parse_arg(Cs1,transcriber,Acc);
	%% Notes
	[$C|Cs1]    -> parse(Cs1, [{note,?C,{1,1}}|Acc]);
	[$D|Cs1]    -> parse(Cs1, [{note,?D,{1,1}}|Acc]);
	[$E|Cs1]    -> parse(Cs1, [{note,?E,{1,1}}|Acc]);
	[$F|Cs1]    -> parse(Cs1, [{note,?F,{1,1}}|Acc]);
	[$G|Cs1]    -> parse(Cs1, [{note,?G,{1,1}}|Acc]);
	[$A|Cs1]    -> parse(Cs1, [{note,?A,{1,1}}|Acc]);
	[$B|Cs1]    -> parse(Cs1, [{note,?B,{1,1}}|Acc]);
	[$c|Cs1]    -> parse(Cs1, [{note,?c,{1,1}}|Acc]);
	[$d|Cs1]    -> parse(Cs1, [{note,?d,{1,1}}|Acc]);
	[$e|Cs1]    -> parse(Cs1, [{note,?e,{1,1}}|Acc]);
	[$f|Cs1]    -> parse(Cs1, [{note,?f,{1,1}}|Acc]);
	[$g|Cs1]    -> parse(Cs1, [{note,?g,{1,1}}|Acc]);
	[$a|Cs1]    -> parse(Cs1, [{note,?a,{1,1}}|Acc]);
	[$b|Cs1]    -> parse(Cs1, [{note,?b,{1,1}}|Acc]);

	[$z|Cs1]    -> parse(Cs1,[{rest,{1,1}}|Acc]);
	[$x|Cs1]    -> parse(Cs1,[{space,{1,1}}|Acc]);
	[$'|Cs1]    -> parse(Cs1,[tick|Acc]);
	[$,|Cs1]    -> parse(Cs1,[comma|Acc]);
	[$|,$||Cs1]  -> parse(Cs1, [double_bar|Acc]);
	[$|,$]|Cs1]  -> parse(Cs1, [thin_thick|Acc]);
	[$[,$||Cs1]  -> parse(Cs1, [thick_thin|Acc]);
	[$|,$:|Cs1] ->  parse_iarg(Cs1,[repeat],1,Acc);
	[$:,$||Cs1=[C|_]] when ?is_digit(C) ->
	    parse_iarg(Cs1,[alt_end],1,[repeat_end|Acc]);
	[$:,$||Cs1]  -> 
	    parse(Cs1, [repeat_end|Acc]);
	[$:,$:|Cs1]  -> parse(Cs1, [repeat_end,{repeat,1}|Acc]);
	[$||Cs1=[C|_]] when ?is_digit(C) ->
	    parse_iarg(Cs1,[alt_end],1,[bar|Acc]);
	[$||Cs1]     -> parse(Cs1, [bar|Acc]);
	[$[|Cs1=[C|_]] when ?is_digit(C) ->
	    parse_iarg(Cs1,[repeat],1,Acc);
	[$(|Cs1=[C|_]] when ?is_digit(C) ->
	    parse_iarg(Cs1,[tuple],1,Acc);
	[$[ | Cs1]  -> parse_block(Cs1, [], Acc);
	[${ | Cs1]  -> parse_grace(Cs1, [], Acc);
	[$( | Cs1]  -> parse_slur(Cs1, Acc);
	[$" | Cs1]  -> parse_string(Cs1, [], Acc);
	[$! | Cs1]  -> parse_symbol(Cs1, [], Acc);
	[$>|Cs1]    -> parse(Cs1, [dot|Acc]);
	[$<|Cs1]    -> parse(Cs1, [dotm|Acc]);
	[$^|Cs1]    -> parse(Cs1, [sharpen|Acc]);     %% prefix
	[$=|Cs1]    -> parse(Cs1, [naturalise|Acc]);  %% prefix
	[$_|Cs1]    -> parse(Cs1, [flatten|Acc]);     %% prefix
	[$u|Cs1]    -> parse(Cs1, [up_bow|Acc]);     %% suffix
	[$v|Cs1]    -> parse(Cs1, [down_bow|Acc]);    %% suffix
	[$.|Cs1]    -> parse(Cs1, [staccato|Acc]);    %% prefix
	[$-|Cs1]    -> parse(Cs1, [tie|Acc]);         %% binary
	[$~|Cs1]    -> parse(Cs1, [ornament|Acc]);    %% prefix
	[$&|Cs1]    -> parse(Cs1, [short_part|Acc]);  %% binary (FIXME!)
	[$/|Cs1] -> parse_length_shorten(Cs1,Acc);
	Cs1=[C|_] when ?is_digit(C) -> parse_length_extend(Cs1,Acc)
    end.

%% parse length of note/rest and combine into a note with length
%% {note,MidiValue,{A,B}}

%% assume first character of Cs to be a digit
parse_length_extend(Cs,Acc) ->
    {L1,Cs1} = parse_unsigned(Cs),
    length_note(Cs1,{L1,1},Acc,[]).

%% we have seen a / character
parse_length_shorten(Cs, Acc) ->
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
    parse(Cs,lists:reverse([{note,N,mult(L,Len)}|Bs],Acc));
length_note(Cs, Len, [{rest,L}|Acc],[]) -> %% can not be prefixed
    parse(Cs,[{rest,mult(L,Len)}|Acc]);
length_note(Cs, Len, [{space,L}|Acc],[]) -> %% can not be prefixed
    parse(Cs,[{space,mult(L,Len)}|Acc]).



parse_iarg([C1,C2|Cs1],Ts,_D,Acc) when ?is_digit(C1), ?is_digit(C2) ->
    parse(Cs1,[build_tuple(Ts, (C1-$0)*10+(C2-$0))|Acc]);
parse_iarg([C1|Cs1],Ts,_D,Acc) when ?is_digit(C1) ->
    parse(Cs1,[build_tuple(Ts,(C1-$0))|Acc]);
parse_iarg(Cs1,Ts,D,Acc) ->
    parse(Cs1,[build_tuple(Ts,D)|Acc]).

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

parse_block([$]|Cs], Bs, Acc) ->
    Bs1 = tr(lists:reverse(Bs), #{ $\s => $\v }),
    RNs = parse(Bs1, []),
    chord(Cs, RNs, Acc);
parse_block([C|Cs], Bs, Acc) ->
    parse_block(Cs, [C|Bs], Acc);
parse_block([], Bs, Acc) ->
    Bs1 = tr(lists:reverse(Bs), #{ $\s => $\v }),
    RNs = parse(Bs1, []),
    chord([], RNs, Acc).

%% check if block is a chord
chord(Cs, RNs, Acc) ->
    case lists:keymember(note, 1, RNs) of
	true -> parse(Cs, [{chord,lists:reverse(RNs)}|Acc]);
	false -> parse(Cs, RNs++Acc)
    end.

parse_grace([$}|Cs], Bs, Acc) ->
    Bs1 = lists:reverse(Bs),
    Grace = lists:reverse(parse(Bs1,[])),
    parse(Cs, [{grace,Grace}|Acc]);
parse_grace([C|Cs], Bs, Acc) ->
    parse_grace(Cs, [C|Bs], Acc);
parse_grace([], Bs, Acc) ->
    Bs1 = lists:reverse(Bs),
    Grace = lists:reverse(parse(Bs1,[])),
    parse([], [{grace,Grace}|Acc]).

%% Parse slur, may be nested and contain annotation and symbols!!!
parse_slur(Cs1, Acc) ->
    parse_slur(Cs1,[],0,false,false,Acc).

parse_slur([$)|Cs],Bs,0,false,false,Acc) ->
    Bs1 = lists:reverse(Bs),
    Slur = lists:reverse(parse(Bs1,[])),
    parse(Cs, [{slur,Slur}|Acc]);
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
    Grace = lists:reverse(parse(Bs1,[])),
    parse([], [{slur,Grace}|Acc]).

parse_symbol([$!|Cs], Bs, Acc) ->
    parse(Cs,[{symbol,lists:reverse(Bs)}|Acc]);
parse_symbol([C|Cs], Bs, Acc) ->
    parse_symbol(Cs, [C|Bs], Acc);
parse_symbol([], Bs, Acc) ->
    parse([],[{symbol,lists:reverse(Bs)}|Acc]).

parse_string([$"|Cs], Bs, Acc) ->
    parse_string_(lists:reverse(Bs),Cs,Acc);
parse_string([C|Cs], Bs, Acc) ->
    parse_string(Cs, [C|Bs], Acc);
parse_string([], Bs, Acc) ->
    parse_string_(lists:reverse(Bs),[],Acc).

parse_string_([$^|String],Cs,Acc) ->
    parse(Cs, [{annotate,above,String}|Acc]);
parse_string_([$_|String],Cs,Acc) ->
    parse(Cs, [{annotate,below,String}|Acc]);
parse_string_([$<|String],Cs,Acc) ->
    parse(Cs, [{annotate,left,String}|Acc]);
parse_string_([$>|String],Cs,Acc) ->
    parse(Cs, [{annotate,right,String}|Acc]);
parse_string_([$@|String],Cs,Acc) ->
    parse(Cs, [{annotate,any,String}|Acc]);
parse_string_([$~|Chord],Cs,Acc) ->
    parse_guitar_chord_(Chord,"~",Cs,Acc);
parse_string_(Chord,Cs,Acc) ->
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
	    parse(Cs,[{annotate,any,P++String}|Acc])
    end.

%% fixme: type may be mixed "C#dim7"...
parse_guitar_type_(Type,Note,Accidental,Cs,Acc) ->
    case Type of
	"min7"++Bass -> parse_guitar_bass(Bass,"min7",Note,Accidental,Cs,Acc);
	"maj7"++Bass -> parse_guitar_bass(Bass,"maj7",Note,Accidental,Cs,Acc);
	"m7"++Bass -> parse_guitar_bass(Bass,"min7",Note,Accidental,Cs,Acc);
	"min"++Bass -> parse_guitar_bass(Bass,"min",Note,Accidental,Cs,Acc);
	"maj"++Bass -> parse_guitar_bass(Bass,"maj",Note,Accidental,Cs,Acc);
	"m"++Bass -> parse_guitar_bass(Bass,"min",Note,Accidental,Cs,Acc);
	"sus"++Bass -> parse_guitar_bass(Bass,"sus",Note,Accidental,Cs,Acc);
	"aug"++Bass -> parse_guitar_bass(Bass,"aug",Note,Accidental,Cs,Acc);
	"dim7"++Bass -> parse_guitar_bass(Bass,"dim7",Note,Accidental,Cs,Acc);
	"dim"++Bass -> parse_guitar_bass(Bass,"dim",Note,Accidental,Cs,Acc);
	"6"++Bass -> parse_guitar_bass(Bass,"6",Note,Accidental,Cs,Acc);
	"7"++Bass -> parse_guitar_bass(Bass,"7",Note,Accidental,Cs,Acc);
	"9"++Bass -> parse_guitar_bass(Bass,"9",Note,Accidental,Cs,Acc);
	"11"++Bass -> parse_guitar_bass(Bass,"11",Note,Accidental,Cs,Acc);
	Bass -> parse_guitar_bass(Bass,"maj",Note,Accidental,Cs,Acc)
    end.

parse_guitar_bass([$/|Bass],Type,Note,Accidental,Cs,Acc) ->
    parse(Cs, [{guitar,Note,Accidental,Type,Bass}|Acc]);
parse_guitar_bass([],Type,Note,Accidental,Cs,Acc) ->
    parse(Cs, [{guitar,Note,Accidental,Type,[]}|Acc]).

parse_arg(Cs, Key, Acc) ->
    {Line,Cs1} = get_line(Cs),
    Value = string:trim(Line),
    parse(Cs1, [{Key,Value}|Acc]).

parse_index(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    Value = list_to_integer(string:trim(Line)),
    %% io:format("line:~w, index ~w\n", [get(line),Value]),
    parse(Cs1, [{index,Value}|Acc]).

parse_voice(Cs, Acc) ->
    {Line,Cs1} = get_line(Cs),
    {Value,Params} = string:to_integer(string:trim(Line)),
    parse(Cs1, [{voice,Value,Params}|Acc]).

parse_note_length(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    parse_note_length(Line,Cs1,Acc).

parse_note_length(Line,Cs,Acc) ->
    Arg = string:to_lower(string:trim(Line)),
    case erl_scan:string(Arg) of
	{ok,[{integer,_,A},{'/',_},{integer,_,B}],_} ->
	    parse(Cs,[{default_note_length,A,B}|Acc]);
	{ok,[{atom,_,Name}],_} ->
	    case Name of
		jig -> parse(Cs,[{default_note_length,1,8}|Acc]);
		reel -> parse(Cs,[{default_note_length,1,8}|Acc]);
		schottische -> parse(Cs,[{default_note_length,1,8}|Acc]);
		waltz -> parse(Cs,[{default_note_length,1,4}|Acc]);
		polka -> parse(Cs,[{default_note_length,1,8}|Acc]);
		bourree -> parse(Cs,[{default_note_length,1,8}|Acc])
	    end
    end.

parse_meter(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    parse_meter(Line,Cs1,Acc).

parse_meter(Line,Cs,Acc) ->
    Arg = string:to_lower(string:trim(Line)),
    case erl_scan:string(Arg) of
	{ok,[{integer,_,A},{'/',_},{integer,_,B}],_} ->
	    parse(Cs,[{meter,A,B}|Acc]);
	{ok,[{atom,_,C}],_} ->
	    parse(Cs,[{meter,C}|Acc]);
	{ok,[{atom,_,C},{'|',_}],_} ->
	    parse(Cs,[{meter,[C,'|']}|Acc])
    end.

parse_tempo(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    parse_tempo(Line,Cs1,Acc).

parse_tempo(String,Cs,Acc) ->
    case erl_scan:string(String) of
	{ok,[{integer,_,A},{'/',_},{integer,_,B},{'=',_},{integer,_,T}],_} ->
	    parse(Cs, [{tempo,{A,B},T}|Acc]);
	{ok,[{integer,_,T}],_} ->
	    parse(Cs, [{tempo,T}|Acc])
    end.

parse_key(Cs,Acc) ->
    {Line,Cs1} = get_line(Cs),
    Arg = remove_blanks(Line),
    case Arg of
	[C,$b,$m,$a,$j|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"bmaj"],to_modal(Modal)}|Acc]);
	[C,$#,$m,$a,$j|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"#maj"],to_modal(Modal)}|Acc]);
	[C,$m,$a,$j|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"maj"],to_modal(Modal)}|Acc]);

	[C,$b,$m,$i,$n|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"bmin"],to_modal(Modal)}|Acc]);
	[C,$#,$m,$i,$n|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"#min"],to_modal(Modal)}|Acc]);
	[C,$m,$i,$n|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"min"],to_modal(Modal)}|Acc]);


	[C,$b,$m,$i,$x|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"bmix"],to_modal(Modal)}|Acc]);
	[C,$#,$m,$i,$x|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"#mix"],to_modal(Modal)}|Acc]);
	[C,$m,$i,$x|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"mix"],to_modal(Modal)}|Acc]);

	[C,$b,$m|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"bmin"],to_modal(Modal)}|Acc]);
	[C,$#,$m|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"#min"],to_modal(Modal)}|Acc]);
	[C,$m|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"min"],to_modal(Modal)}|Acc]);

	[C,$b|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"bmaj"],to_modal(Modal)}|Acc]);
	[C,$#|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"#maj"],to_modal(Modal)}|Acc]);
	[C|Modal] when C >= $A, C =< $G ->
	    parse(Cs1, [{key,[C|"maj"],to_modal(Modal)}|Acc])
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
