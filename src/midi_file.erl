%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Handle .mid files
%%% @end
%%% Created :  4 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_file).

-export([load/1]).
-export([text_expand/1,text_expand/2]).

load(File) ->
    case file:open(File,[read,raw,binary]) of
	{ok,Fd} ->
	    try read_tune(Fd) of
		R -> R
	    catch
		error:E -> {error,E,erlang:get_stacktrace()}
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

read_track(Fd, I) ->
    case read_chunk(Fd) of
	{ok,_Chunk={<<"MTrk">>,Data}} ->
	    %% io:format("Track = ~p\n", [_Chunk]),
	    State = midi_codec:init(Data),
	    case parse_track(State, []) of
		{ok,Cmds} ->
		    {I,Cmds};
		Error -> Error
	    end;
	{ok,_Data} ->
	    {error, bad_track};
	Error -> Error
    end.

parse_track(State, Acc) ->
    case midi_codec:scan_delta(State) of
	{{ok,Delta},State1} ->
	    case midi_codec:scan(State1) of
		{more, _State2} ->
		    io:format("warning: end of track with more to read\n", []),
		    {ok,lists:reverse([Delta|Acc])};
		{{Status,Params}, State2} ->
		    Event = midi_codec:event_decode(Status,Params),
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
