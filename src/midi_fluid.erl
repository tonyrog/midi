%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Wrapper to setup/start/stop fluid synth
%%% @end
%%% Created : 16 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_fluid).

-export([start/1, stop/1]).
-export([find_port/1]).

-define(SOUND_FONT, "/usr/share/sounds/sf2/FluidR3_GM.sf2").

%% find fluid synth port (device)
find_port([D=#{client_name := "FLUID"++_}|_Ds]) ->
    D;
find_port([_|Ds]) -> find_port(Ds);
find_port([]) -> false.

stop(Pid) ->
    Pid ! stop.

sound_fonts() ->
    case application:get_env(midi, sound_fonts) of
	undefined -> 
	    ?SOUND_FONT;  %% try "standard" location
	{ok,Fs} ->
	    Fs1 = [midi_file:text_expand(F)||F <- Fs],
	    string:join(Fs1, " ")
    end.

%% FIXME: match fluid with audio-driver and midi backend
%% pass backend module as argument?
start(Backend) ->
    Name = Backend:name(),
    Device  = Backend:audio_device(),
    Command = "fluidsynth --server "++
	"--audio-driver="++Name++" "++
	"-o audio."++Name++".device="++Device++" "++
	sound_fonts(),
    Pid = spawn(
	    fun() ->
		    Port = open_port({spawn,Command},
				     [eof,exit_status,stderr_to_stdout]),
		    port_loop(Port)
	    end),
    timer:sleep(3000), %% allow device to be created
    Pid.

port_loop(Port) ->
    receive
	{Port,eof} ->
	    io:format("fluidsynth closed\n", []),
	    ok;
	{Port,{data,Info}} -> %% print info from fluid synth
	    io:format("~s", [tr(Info,$\n,"\r\n")]),
	    port_loop(Port);
	{Port,What} ->
	    io:format("port_loop: got ~p\n", [What]),
	    port_loop(Port);
	stop ->
	    io:format("fluidsynth stopping\n", []),
	    erlang:port_close(Port),
	    ok
    end.

tr([C|Cs], C, Ds=[D]) -> [D|tr(Cs,C,Ds)];
tr([C|Cs], C, Ds=[D1,D2]) -> [D1,D2|tr(Cs,C,Ds)];
tr([C|Cs], C, Ds) -> Ds++tr(Cs,C,Ds);
tr([H|Cs], C, Ds) -> [H|tr(Cs,C,Ds)];
tr([],_C,_Ds) -> [].
