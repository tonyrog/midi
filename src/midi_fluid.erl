%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    Wrapper to setup/start/stop fluid synth
%%% @end
%%% Created : 16 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_fluid).

-export([start/0]).
-export([stop/1]).
-export([find_port/1]).

-define(DEFAULT_SOUND_FONTS,
	["/usr/share/sounds/sf2/FluidR3_GM.sf2",
	 "/usr/share/sounds/sf2/FluidR3_GS.sf2"]).
%% -p | --portname 
-define(DEFAULT_PORTNAME, "FLUID Synth").
%% -a | --audio-driver
-define(DEFAULT_AUDIO, "alsa").
%% -m | --midi-driver
-define(DEFAULT_MIDI,  "alsa_seq").
%% -g | --gain
-define(DEFAULT_GAIN,  0.2).
%% -K | --midi-channels
-define(DEFAULT_MIDI_CHANNELS, 16).
%% -L | --audio-channels
-define(DEFAULT_AUDIO_CHANNELS, 1).
%% -C | --chorus
-define(DEFAULT_CHORUS,  0).

%% Other
-define(DEFAULT_PERIOD_SIZE, 64).  %% to small without real time!


%% Other options
%% -o audio.<Audio>.device=<AudioDevice>
-define(DEFAULT_AUDIO_DEVICE, "default").

getenv(Name, Default) ->
    PropList = application:get_env(midi, midi_fluid, []),    
    Value = proplists:get_value(Name, PropList, Default),
    if is_integer(Value) ->
	    integer_to_list(Value);
       is_float(Value) ->
	    io_lib_format:fwrite_g(Value);
       is_list(Value) ->
	    case is_string(Value) andalso need_quote(Value) of
		true -> [$"|(Value++[$"])];
		false -> Value
	    end
    end.

is_string([C|Cs]) when is_integer(C), C >= 0, C =< 255 ->
    is_string(Cs);
is_string([]) ->
    true;
is_string(_) ->
    false.

need_quote([C|_]) when is_integer(C), C < 33 -> true;
need_quote([C|_]) when is_integer(C), C > 126 -> true;
need_quote([_|Cs]) -> need_quote(Cs);
need_quote([]) -> false.

%% find fluid synth port (device)
find_port(Devices) ->
    PortName = getenv(portname, ?DEFAULT_PORTNAME),
    find_port(PortName, Devices).
    
find_port(PortName, [D=#{client_name := ClientName}|Ds]) ->
    case lists:prefix(PortName, ClientName) of
	true ->
	    D;
	false ->
	    find_port(PortName, Ds)
    end;
find_port(PortName, [_|Ds]) -> find_port(PortName, Ds);
find_port(_PortName, []) -> false.

stop(Pid) ->
    Pid ! stop.

sound_fonts() ->
    Fs = getenv(sound_fonts, ?DEFAULT_SOUND_FONTS),
    Fs1 = [midi_file:text_expand(F)||F <- Fs],
    string:join(Fs1, " ").

%% FIXME: match fluid with audio-driver and midi backend
%% pass backend module as argument?
start() ->
    PortName = getenv(portname, ?DEFAULT_PORTNAME),
    Audio = getenv(audio, ?DEFAULT_AUDIO),
    Midi  = getenv(midi, ?DEFAULT_MIDI),
    MidiChannels = getenv(midi_channels, ?DEFAULT_MIDI_CHANNELS),
    AudioChannels = getenv(audio_channels, ?DEFAULT_AUDIO_CHANNELS),
    AudioDevice = getenv(audio_device, ?DEFAULT_AUDIO_DEVICE),
    Gain = getenv(gain, ?DEFAULT_GAIN),
    Chorus = getenv(chorus, ?DEFAULT_CHORUS),
    PeriodSize = getenv(period_size, ?DEFAULT_PERIOD_SIZE),

    %% --server?
    Command = lists:flatten(
		["fluidsynth", " ",
		 " -o midi.portname=", PortName,
		 " -o synth.midi-channels=",MidiChannels, 
		 " -o synth.audio-channels=",AudioChannels,
		 " -o midi.driver=",Midi,
		 " -o audio.driver=",Audio,
		 " -o synth.chorus.active=",Chorus,
		 " -o synth.gain=",Gain,
		 " -o audio.",Audio,".device=",AudioDevice,
		 " -o audio.period-size=",PeriodSize,
		 " ", sound_fonts()]),
    io:format("command: ~s\n", [Command]),
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
	    io:format("port_loop: fluidsynth closed\n", []),
	    ok;
	{Port,{data,Info}} -> %% print info from fluid synth
	    io:format("port_loop: ~s", [tr(Info,$\n,"\r\n")]),
	    port_loop(Port);
	{Port,What} ->
	    io:format("port_loop: got ~p\n", [What]),
	    port_loop(Port);
	stop ->
	    io:format("port_loop: fluidsynth stopping\n", []),
	    erlang:port_close(Port),
	    ok
    end.

tr([C|Cs], C, Ds=[D]) -> [D|tr(Cs,C,Ds)];
tr([C|Cs], C, Ds=[D1,D2]) -> [D1,D2|tr(Cs,C,Ds)];
tr([C|Cs], C, Ds) -> Ds++tr(Cs,C,Ds);
tr([H|Cs], C, Ds) -> [H|tr(Cs,C,Ds)];
tr([],_C,_Ds) -> [].
