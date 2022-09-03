%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Prophet 6 paramter settings
%%% @end
%%% Created :  3 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(midi_prophet6).

-include("../include/midi_prophet6.hrl").

-export([osc_1_freq/3, osc_1_sync/3, osc_1_level/3, 
	 osc_1_shape/3, osc_1_pulse_width/3]).
-export([osc_2_freq/3, osc_2_freq_finem/3, 
	 osc_2_level/3, osc_2_shape/3, osc_2_pulse_width/3,
	 osc_2_low_freq/3, osc_2_key_on_off/3]).
-export([osc_1_sub_level/3]).
-export([glide_mode/3, glide_on_off/3, glide_rate/3]).
-export([pbend_range/3, noise_level/3, slop/3]).
-export([low_pass_freq/3]).
-export([vca_env_decay/3,vca_env_sustain/3,vca_env_release/3]).
-export([vca_env_vel_on_off/3]).
-export([low_pass_env_amt/3]).
-export([filter_env_attack/3, filter_env_decay/3,
	 filter_env_sustain/3, filter_env_release/3]).
-export([high_pass_env_amt/3]).
-export([lfo_freq/3, lfo_initial_amt/3, lfo_shape/3, lfo_sync/3]).
-export([lfo_freq_1_dest_on_off/3]).
-export([lfo_freq_2_dest_on_off/3]).
-export([lfo_pw_1_2_dest_on_off/3]).
-export([lfo_amp_dest_on_off/3]).

osc_1_freq(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_1_FREQ, Value).

osc_1_sync(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_1_SYNC, Value).

osc_1_level(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_1_LEVEL, Value).

osc_1_shape(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_1_SHAPE, Value).

osc_1_pulse_width(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_1_PULSE_WIDTH, Value).

osc_2_freq(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_2_FREQ, Value).

osc_2_freq_finem(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_2_FREQ_FINEM, Value).

osc_2_level(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_2_LEVEL, Value).

osc_2_shape(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_2_SHAPE, Value).

osc_2_pulse_width(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_2_PULSE_WIDTH, Value).

osc_2_low_freq(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_2_LOW_FREQ, Value).

osc_2_key_on_off(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_2_KEY_ON_OFF, Value).

osc_1_sub_level(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?OSC_1_SUB_LEVEL, Value).

glide_mode(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?GLIDE_MODE, Value).

glide_on_off(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?GLIDE_ON_OFF, Value).

glide_rate(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?GLIDE_RATE, Value).

pbend_range(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?PBEND_RANGE, Value).

noise_level(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?NOISE_LEVEL, Value).

slop(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?SLOP, Value).

low_pass_freq(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LOW_PASS_FREQ, Value).

vca_env_decay(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?VCA_ENV_DECAY, Value).

vca_env_sustain(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?VCA_ENV_SUSTAIN, Value).

vca_env_release(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?VCA_ENV_RELEASE, Value).

vca_env_vel_on_off(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?VCA_ENV_VEL_ON_OFF, Value).

low_pass_env_amt(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LOW_PASS_ENV_AMT, Value).

filter_env_attack(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?FILTER_ENV_ATTACK, Value).

filter_env_decay(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?FILTER_ENV_DECAY, Value).

filter_env_sustain(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?FILTER_ENV_SUSTAIN, Value).

filter_env_release(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?FILTER_ENV_RELEASE, Value).

high_pass_env_amt(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?HIGH_PASS_ENV_AMT, Value).

lfo_freq(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LFO_FREQ, Value).

lfo_initial_amt(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LFO_INITIAL_AMT, Value).

lfo_shape(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LFO_SHAPE, Value).

lfo_sync(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LFO_SYNC, Value).

lfo_freq_1_dest_on_off(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LFO_FREQ_1_DEST_ON_OFF, Value).

lfo_freq_2_dest_on_off(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LFO_FREQ_2_DEST_ON_OFF, Value).

lfo_pw_1_2_dest_on_off(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LFO_PW_1_2_DEST_ON_OFF, Value).

lfo_amp_dest_on_off(Synth, Chan, Value) ->
    midi:nrp(Synth, Chan, ?LFO_AMP_DEST_ON_OFF, Value).

