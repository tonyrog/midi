%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Handle .mid files
%%% @end
%%% Created :  4 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_file).

-export([new/1]).
-export([new_track/1, add_event/2]).
-export([new_note_on/4, new_note_off/4]).
-export([load/1, save/2]).
-export([text_expand/1,text_expand/2]).
-export([read_tune/1]).
-export([read_track/2]).
-export([read_chunk/1]).
-export([write_tune/2, write_tune/3, write_tune/4]).
-export([write_track/2]).
-export([write_chunk/3]).
-export([parse_track/2]).

-ifdef(OTP_RELEASE). %% this implies 21 or higher
-define(EXCEPTION(Class, Reason, Stacktrace), Class:Reason:Stacktrace).
-define(GET_STACK(Stacktrace), Stacktrace).
-else.
-define(EXCEPTION(Class, Reason, _), Class:Reason).
-define(GET_STACK(_), erlang:get_stacktrace()).
-endif.

-include("../include/midi.hrl").

new(Filename) ->
    {ok, Fd} = file:open(Filename, [write]),
    %% io:format("new: Fd = ~p\n", [Fd]),
    put(Fd, []),
    Fd.

new_track(Fd) ->
    TrackBuf = prim_buffer:new(),
    TRef = {Fd,TrackBuf},
    put(Fd, [TRef|get(Fd)]),
    %% io:format("new: track = ~p\n", [TRef]),
    TRef.

add_event(_TRef={_Fd,TrackBuf}, EventData) ->
    prim_buffer:write(TrackBuf, EventData).

new_note_on(Duration,Note,Velocity,Chan) ->
    Bin0 = midi_codec:length_encode(Duration),
    Bin1 = midi_codec:event_encode(#note_on{chan=Chan,note=Note,velocity=Velocity}),
    [Bin0, Bin1].

new_note_off(Duration,Note,Velocity,Chan) ->
    Bin0 = midi_codec:length_encode(Duration),
    Bin1 = midi_codec:event_encode(#note_off{chan=Chan,note=Note,velocity=Velocity}),
    [Bin0, Bin1].

save(Fd, _Filename) ->
    %% io:format("save: Fd = ~p\n", [Fd]),
    write_tune(Fd, 64),
    List = lists:reverse(get(Fd)),
    %% io:format("List = ~p\n", [List]),
    lists:foreach(
      fun({_Fd,TrackBuf}) ->
	      Size = prim_buffer:size(TrackBuf),
	      IOVec = prim_buffer:read_iovec(TrackBuf, Size),
	      write_track(Fd, IOVec)
      end, List),
    file:close(Fd),
    erase(Fd),
    ok.

load(File) ->
    case file:open(File,[read,raw,binary]) of
	{ok,Fd} ->
	    try read_tune(Fd) of
		R -> R
	    catch
		?EXCEPTION(error,E,_Trace) ->
		    {error,E,?GET_STACK(_Trace)}
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

read_tune(Fd) ->
    case read_chunk(Fd) of
	{ok,{<<"MThd">>,<<Format:16,NumTracks:16,Division:16/signed,
			  _/binary>>}} ->
	    Tracks =
		lists:foldl(
		  fun(I,Acc) ->
			  [read_track(Fd,I) | Acc]
		  end, [], lists:seq(0,NumTracks-1)),
	    {ok,{Format,NumTracks,Division},lists:reverse(Tracks)};
	{ok,Chunk={_ID,_Binary}} ->
	    io:format("chunk = ~p\n", [Chunk]),
	    {error,bad_chunk}
    end.

%% Format = 0 single track 
%%          1 multi track played simultaneously, 1 track is tempo map
%%          2 multi track independent sequence
%%
write_tune(Fd, Division) ->
    write_tune(Fd, 0, 1, Division).

write_tune(Fd, Format, Division) ->
    write_tune(Fd, Format, 1, Division).

write_tune(Fd, Format, NumTracks, Division) ->
    write_chunk(Fd, <<"MThd">>, <<Format:16,NumTracks:16,Division:16/signed>>).

read_track(Fd, I) ->
    case read_chunk(Fd) of
	{ok,_Chunk={<<"MTrk">>,Data}} ->
	    %% io:format("Track = ~p\n", [_Chunk]),
	    State = midi_codec:init(true, Data),
	    case parse_track(State, []) of
		{ok,Cmds} ->
		    {I,Cmds};
		Error -> Error
	    end;
	{ok,_Data} ->
	    {error, bad_track};
	Error -> Error
    end.

write_track(Fd, Data) ->
    End = midi_codec:event_encode(#meta{type=end_of_track}),
    write_chunk(Fd, <<"MTrk">>, [Data,End]).

parse_track(State, Acc) ->
    case midi_codec:scan_delta(State) of
	{{ok,Delta},State1} ->
	    case midi_codec:scan(State1) of
		{more, _State2} ->
		    io:format("warning: end of track with more to read\n", []),
		    {ok,lists:reverse([Delta|Acc])};
		{{Status,Params}, State2} ->
		    Event = midi_codec:event_decode(Status,Params),
		    io:format("Event = ~p\n", [Event]),
		    parse_track(State2, [Event,Delta|Acc])
	    end;
	{eot, _State2} ->
	    {ok,lists:reverse(Acc)};
	{Error,_State1} ->
	    Error
    end.
		
read_chunk(Fd) ->
    case file:read(Fd, 8) of
	{ok,<<ID:4/binary, Length:32>>} ->
	    case file:read(Fd, Length) of
		{ok,Data} when byte_size(Data) =:= Length -> {ok,{ID,Data}};
		{ok,_} -> {error, truncated};
		Error -> Error
	    end;
	{ok,_} -> {error, truncated};
	Error -> Error
    end.

write_chunk(Fd, ID, Data) ->
    Size = iolist_size(Data),
    file:write(Fd, [<<ID:4/binary,Size:32>>,Data]).
    
%%
%% Utility to exand environment "variables" in unicode text
%% variables are written as ${var} where var is a encoded atom
%% operating system enviroment is accessed through $(VAR)
%% and application library dir $/app/
%%
text_expand(Text) ->
    text_expand(Text,[]).
    
text_expand(Text, Env) when is_list(Text) ->
    %% assume unicode character list!
    text_expand_(Text, [], Env);
text_expand(Text, Env) when is_binary(Text) ->
    %% assume utf8 encoded data!
    text_expand_(unicode:characters_to_list(Text), [], Env).

text_expand_([$$,${|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [${,$$], env, Acc, Env);
text_expand_([$$,$(|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$(,$$], shell, Acc, Env);
text_expand_([$$,$/|Text], Acc, Env) ->
    text_expand_collect_(Text, [], [$/,$$], lib, Acc, Env);
text_expand_([$\\,C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([C|Text], Acc, Env) ->
    text_expand_(Text, [C|Acc], Env);
text_expand_([], Acc, _Env) ->
    lists:reverse(Acc).


text_expand_collect_([$)|Text], Var, _Pre, shell, Acc, Env) ->
    case os:getenv(rev_variable(Var)) of
	false ->
	    text_expand_(Text, Acc, Env);
	Value ->
	    Acc1 = lists:reverse(Value, Acc),
	    text_expand_(Text, Acc1, Env)
    end;
text_expand_collect_([$/|Text], Var, _Pre, lib, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	App ->
	    case code:lib_dir(App) of
		{error,_} ->
		    text_expand_(Text, Acc, Env);
		Value ->
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([$}|Text], Var, _Pre, env, Acc, Env) ->
    try erlang:list_to_existing_atom(rev_variable(Var)) of
	Key ->
	    case lists:keyfind(Key, 1, Env) of
		false ->
		    text_expand_(Text, Acc, Env);
		{_,Val} ->
		    Value = lists:flatten(io_lib:format("~w", [Val])),
		    Acc1 = lists:reverse(Value, Acc),
		    text_expand_(Text, Acc1, Env)
	    end
    catch
	error:_ ->
	    text_expand_(Text, Acc, Env)
    end;
text_expand_collect_([C|Text], Var, Pre, Shell, Acc, Env) ->
    if C >= $a, C =< $z;
       C >= $A, C =< $Z;
       C >= $0, C =< $9;
       C =:= $_; C =:= $@;
       C =:= $\s; C =:= $\t -> %% space and tab allowed in begining and end
	    text_expand_collect_(Text, [C|Var], Pre, Shell, Acc, Env);
       true ->
	    %% char not allowed in variable named
	    text_expand_(Text,  [C | Var ++ Pre ++ Acc], Env)
    end;
text_expand_collect_([], Var, Pre, _Shell, Acc, Env) ->
    text_expand_([],  Var ++ Pre ++ Acc, Env).

rev_variable(Var) ->
    trim_hd(lists:reverse(trim_hd(Var))).

trim_hd([$\s|Cs]) -> trim_hd(Cs);
trim_hd([$\t|Cs]) -> trim_hd(Cs);
trim_hd(Cs) -> Cs.
