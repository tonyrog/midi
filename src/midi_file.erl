%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Handle .mid files
%%% @end
%%% Created :  4 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_file).

-export([load/1]).

load(File) ->
    case file:open(File,[read,binary]) of
	{ok,Fd} ->
	    try read_tune(Fd) of
		R -> R
	    catch
		error:E -> {error,E}
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
	    State = midi_codec:init(binary_to_list(Data)),
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
