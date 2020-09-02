%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    INTEGRA-7 functions
%%% @end
%%% Created : 24 Jun 2020 by Tony Rogvall <tony@rogvall.se>

-module(midi_integra7).

-export([surround_LR/3, surround_LR2/3]).
-export([surround_FB/3, surround_FB2/3]).
-export([surround_ambience/3, surround_ambience2/3]).

-include("../include/midi_integra7.hrl").

surround_LR(Synth, Chan, Value) ->
    midi:control7(Synth, Chan,
		  ?MIDI_CTRL_INTEGRA_7_LEFT_RIGHT, 
		  midi:signed7(Value)).

surround_LR2(Synth, Chan, Value) ->
    midi:control14(Synth, Chan,
		   ?MIDI_CTRL_INTEGRA_7_LEFT_RIGHT,
		   ?MIDI_CTRL_INTEGRA_7_LEFT_RIGHT_FINE, 
		   midi:signed14(Value)).

surround_FB(Synth, Chan, Value) ->
    midi:control7(Synth, Chan,
		  ?MIDI_CTRL_INTEGRA_7_FRONT_BACK, 
		  midi:signed7(Value)).

surround_FB2(Synth, Chan, Value) ->
    midi:control14(Synth, Chan,
		   ?MIDI_CTRL_INTEGRA_7_FRONT_BACK,
		   ?MIDI_CTRL_INTEGRA_7_FRONT_BACK_FINE,
		   midi:signed14(Value)).

surround_ambience(Synth, Chan, Value) ->
    midi:control7(Synth, Chan,
		  ?MIDI_CTRL_INTEGRA_7_AMBIENCE, 
		  midi:signed7(Value)).

surround_ambience2(Synth, Chan, Value) ->
    midi:control14(Synth, Chan,
		   ?MIDI_CTRL_INTEGRA_7_AMBIENCE,
		   ?MIDI_CTRL_INTEGRA_7_AMBIENCE_FINE,
		   midi:signed14(Value)).
