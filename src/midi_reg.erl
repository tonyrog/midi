%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    register / lookup registered midi devices
%%% @end
%%% Created : 16 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_reg).

-export([register/2]).
-export([unregister/1]).
-export([whereis/1]).

register(Name, Fd) when is_atom(Name), is_reference(Fd) ->
    ets:insert(midi_reg, {Name,Fd}).

unregister(Name) when is_atom(Name) ->
    ets:delete(midi_reg, Name).

whereis(Name) when is_atom(Name) ->
    try ets:lookup(midi_reg, Name) of
	[] -> undefined;
	[{_,Fd}] -> Fd
    catch
	error:_ -> undefined
    end.
