%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Midi editor
%%% @end
%%% Created : 24 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_editor).

-include("../include/midi.hrl").

-compile(export_all).

-define(DIVISION, 480).           %% PPQN = 480 pulses per quarter note
-define(TICK, (?DIVISION div 8)). %% PP32 pulses per 1/32 note
%%
%% Graphical(/text) layout
%%
%% 4/4
%%        
%%      | 12345678.12345678 12345678 12345678 |
%%      | C======= D======= C======= E======= | 
%%
%% 3/4
%%      | 12345678 12345678 12345678 |
%%      | C======= D======= C======= | 
%%
%%         
%%    | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
%% 0: |D==   c====                                            |
%%  : |E=    f====                                            |
%% 1: |===         ===                                        |
%% 2: |===                                                    |
%% 3: |===              ===        ==                         |
%% ...|                                                       |
%% 15:|===              ===        ==                         |
%%    | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
%%
load(File) ->
    case file:read_file(File) of
	{ok,Bin} ->
	    parse(Bin);
	Error ->
	    Error
    end.

play_file(File) ->
    play_file(synth,File).

play_file(Fd,File) ->
    {ok,Tracks} = load(File),
    Tracks1 = compile(Tracks),
    Tracks2 = [Ns || {_,Ns} <- Tracks1],
    midi_play:tracks(Fd,Tracks2,120,0,?DIVISION).
    
play_string(Data) ->
    play_string(synth,Data).

play_string(Fd,Data) ->
    {ok,Tracks} = parse(Data),
    Tracks1 = compile(Tracks),
    Tracks2 = [Ns || {_,Ns} <- Tracks1],
    midi_play:tracks(Fd,Tracks2,120,0,?DIVISION).

compile(Tracks) ->
    Tracks1 = lists:keysort(1,Tracks),
    Tracks2 = merge_channels(Tracks1),
    Tracks3 = delta_channels(Tracks2),
    Tracks3.

parse(Cs) when is_list(Cs) ->
    parse_(Cs);
parse(Bin) when is_binary(Bin) ->
    parse_(binary_to_list(Bin)).

parse_(Cs) -> parse_(Cs,-1,[]).

parse_("",_N,Acc) ->
    {ok,Acc};
parse_(Cs,N,Acc) ->
    {Line,Cs1} = get_line(Cs),
    Line1 = string:trim(Line),
    case Line1 of
	[C1,C0,$:|Line2] when C1 =:= $1, C0 >= $0, C0 =< $9 ->
	    N1 = 10+(C0-$0),
	    Ns = channel(Line2,N1,0,[]),
	    parse_(Cs1, N1, [{N1,Ns}|Acc]);
	[C0,$:|Line2] when C0 >= $0, C0 =< $9 ->
	    N1 = (C0-$0),
	    Ns = channel(Line2,N1,0,[]),
	    parse_(Cs1, N1, [{N1,Ns}|Acc]);
	[$:|Line2] ->
	    Ns = channel(Line2,N,0,[]),
	    parse_(Cs1, N, [{N,Ns}|Acc])
    end.

channel([C|Cs], Chan, Tick, Acc) when C >= $A, C =< $G ->
    Note = element((C-$A)+1,{?A,?B,?C,?D,?E,?F,?G}),
    continue(Cs,Chan,Tick+1,Note,[{note_on,Chan,Note,100},Tick|Acc]);
channel([C|Cs], Chan, Tick, Acc) when C >= $a, C =< $g ->
    Note = element((C-$a)+1,{?a,?b,?c,?d,?e,?f,?g}),
    continue(Cs,Chan,Tick+1,Note,[{note_on,Chan,Note,100},Tick|Acc]);
channel([$\s|Cs],Chan,Tick,Acc) ->
    channel(Cs,Chan,Tick+1,Acc);
channel([$||Cs],Chan,Tick,Acc) ->
    channel(Cs,Chan,Tick,Acc);
channel([],_Chan,_Tick,Acc) ->
    lists:reverse(Acc).

continue([$=|Cs],Chan,Tick,Note,Acc) ->
    continue(Cs,Chan,Tick+1,Note,Acc);
continue(Cs,Chan,Tick,Note,Acc) ->
    channel(Cs, Chan, Tick+1, [{note_off,Chan,Note,100},Tick|Acc]).

get_line(Cs) -> get_line(Cs,[]).
get_line([$\r,$\n|Cs],Acc) -> {lists:reverse(Acc), Cs};
get_line([$\n|Cs],Acc) -> {lists:reverse(Acc), Cs};
get_line([$\r,$\v|Cs],Acc) -> {lists:reverse(Acc), Cs};
get_line([$\v|Cs],Acc) -> {lists:reverse(Acc), Cs};
get_line([C|Cs],Acc) -> get_line(Cs,[C|Acc]);
get_line([],Acc) -> {lists:reverse(Acc), []}.


%% transform ticks into delta(*?TICK)
delta_channels([{Chan,Ns}|Tracks]) ->
    [{Chan,delta_channel(Ns,0)}|delta_channels(Tracks)];
delta_channels([]) ->
    [].

delta_channel([N,E|Ns],P) ->
    [(N-P)*?TICK,E|delta_channel(Ns,N)];
delta_channel([],_) ->
    [].

%% merge data on the same channel
merge_channels([{Chan,Ns1},{Chan,Ns2}|Tracks]) ->
    Ns = merge_events(Ns1,Ns2),
    merge_channels([{Chan,Ns}|Tracks]);
merge_channels([{Chan1,Ns1},{Chan2,Ns2}|Tracks]) ->
    [{Chan1,Ns1}|merge_channels([{Chan2,Ns2}|Tracks])];
merge_channels([{Chan1,Ns1}]) ->
    [{Chan1,Ns1}];
merge_channels([]) ->
    [].

%% merge events on the same channel by tick
merge_events([T1,E1|Es1],Es2=[T2,_E2|_]) when T1 =< T2 ->
    [T1,E1|merge_events(Es1,Es2)];
merge_events(Es1=[_T1,_E1|_],[T2,E2|Es2]) ->
    [T2,E2|merge_events(Es1,Es2)];
merge_events([], []) ->
    [];
merge_events(Es1,[]) -> Es1;
merge_events([],Es2) -> Es2.
