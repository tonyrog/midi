%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2024 by Tony Rogvall <tony@rogvall.se>

-module(midi_evolver).

-compile(export_all).

-export([open/0, open/1, close/1]).

-export([device_inquiry/1, device_inquiry/2]).
-export([program_dump/3, request_program_dump/3]).
-export([request_waveshape_dump/2, waveshape_data_dump/2]).
-export([program_edit_buffer_dump/1, request_program_edit_buffer/1]).
-export([global_parameter_dump/1, request_global_parameter_dump/1]).
-export([transmit_program_parameter/3]).
-export([transmit_sequencer_tables/5]).
-export([transmit_sequencer_parameter/4]).
-export([transmit_global_parameter/3]).
-export([transmit_waveshape_data/3]).
-export([transmit_waveshape/3]).

-export([cc/0]).

-export([pack_ms_bit/1, unpack_ms_bit/1]).
-export([unpack_global_params/1]).
-export([decode_program/1, decode_program/2, decode_program/4]).
%% reverse engineer
-export([get_edit_buffer/1, diff_edit_buffer/2, diff_bytes/4]).
-export([dump_edit_buffer/1, decode_edit_buffer/1]).
-export([verify_dump/4]).


-include("../include/midi.hrl").

-define(DSI_ID_HEADER, 1).
-define(FILE_VERSION, 1).

-define(EVOLVER_ID,        16#20).
-define(PROPHET_8_ID,      16#23).
-define(PROPHET_6_ID,      16#2d).
-define(PROPHET_REV2_ID,   16#2f).
-define(PROPHET_X_ID,      16#30).
-define(PROPHET_5_ID,      16#33).  %% 16#31 or 16#33 ? (manual is ambigous)
-define(PROPHET_PRO_3_ID,  16#31).
-define(TRIGON_6_ID,       16#2D).  %% 0010 1101 (same as Prophet-6)???
%% -define(EVOLVER_DESKTOP_ID, 16#21). ?
-define(EVOLVER_KEYBOARD_ID, 16#22).

-define(SYS_REQUEST_PROGRAM_TRANSMIT, 5).
-define(SYS_REQUEST_PROGRAM_EDIT_BUFFER_TRANSMIT, 6).
-define(SYS_REQUEST_WAVESHAPE_TRANSMIT, 11).
-define(SYS_REQUEST_GLOBAL_PARAMETER_TRANSMIT, 14).

-define(PROGRAM_PARAMETER,   2#000_0001).  %% 1
-define(PROGRAM_DATA,        2#000_0010).  %% 2
-define(EDIT_BUFFER_DATA,    2#000_0011).  %% 3
-define(SEQUENCE_PARAMETER,  2#000_1000).  %% 8
-define(GLOBAL_PARAMETER,    2#000_1000).  %% 9
-define(WAVESHAPE_DATA,      2#000_1010).  %% 10
-define(MAIN_PARAMETER_DATA, 2#000_1111).  %% 15
-define(PROGRAM_NAME_DATA,   2#001_0001).  %% 17

-define(PARM(Param, Name),
	(Param) => {Name, {0,127,#{}}},
	(Name) => (Param)).

-define(PARM(Param, Name, Min, Max),
	(Param) => {(Name), {(Min),(Max),#{}}},
	(Name) => (Param)).

-define(PARM(Param, Name, Min, Max, Enum),
	(Param) => {(Name), {(Min),(Max),(Enum)}},
	(Name) => (Param)).

%% enum
-define(ONOFF, #{off=>0, on=>1}). %% 0-1

%% enum
-define(DESTINATION,
	#{
	  off => 0,
	  osc_1_freq => 1,
	  osc_2_freq => 2,
	  osc_3_freq => 3,
	  osc_4_freq => 4,
	  osc_all_freq  => 5,
	  osc_1_level => 6,
	  osc_2_level => 7,
	  osc_3_level => 8,
	  osc_4_level => 9,
	  osc_all_Level => 10,
	  noise_level => 11,
	  ext_in_level => 12,
	  osc_1_pulsw => 13,
	  osc_2_pulsw => 14,
	  osc_all_pulsw => 15,
	  fm_osc_4_3 => 16,
	  fm_osc_3_4 => 17,
	  rm_osc_4_3 => 18,
	  rm_osc_3_4 => 19,
	  filter_freq => 20,
	  filter_split => 21,
	  resonance => 22,
	  highpass_freq => 23,
	  vca_amt => 24,
	  pan => 25,
	  feedback_freq => 26,
	  feedback_amt => 27,
	  delay_1_time => 28,
	  delay_2_time => 29,
	  delay_3_time => 30,
	  delay_time_all => 31,
	  delay_1_amt => 32,
	  delay_2_amt => 33,
	  delay_3_amt => 34,
	  delay_amt_all => 35,
	  delay_feedback_1 => 36,
	  delay_feedback_2 => 37,
	  lfo_1_freq => 38,
	  lfo_2_freq => 39,
	  lfo_3_freq => 40,
	  lfo_4_freq => 41,
	  lfo_all_freq => 42,
	  lfo_1_amt => 43,
	  lfo_2_amt => 44,
	  lfo_3_amt => 45,
	  lfo_4_amt => 46,
	  lfo_all_amt => 47,
	  env_1_amt => 48,
	  env_2_amt => 49,
	  env_3_amt => 50,
	  env_all_amt => 51,
	  env_1_attack => 52,
	  env_2_attack => 53,
	  env_3_attack => 54,
	  env_all_attack => 55,
	  env_1_decay => 56,
	  env_2_decay => 57,
	  env_3_decay => 58,
	  env_all_decay => 59,
	  env_1_release => 60,
	  env_2_release => 61,
	  env_3_release => 62,
	  env_all_release => 63,
	  filter_1_left_cutoff_freq => 64,
	  filter_2_right_cutoff_freq => 65,
	  filter_1_left_resonance => 66,
	  filter_2_right_resonance => 67,
	  distortion => 68,
	  %% sequencer-only destinations
	  tempo_clock_multiplier => 69,
	  midi_note_out => 70,
	  midi_velocity_out => 71,
	  midi_mod wheel_out => 72,
	  midi_pressure_out => 73,
	  midi_breath_controller_out => 74,
	  midi_foot_controller_out => 75
	 }).


-define(SOURCES,
	#{
	  off => 0,
	  seq_1 => 1,
	  seq_2 => 2,
	  seq_3 => 3,
	  seq_4 => 4,
	  lfo_1 => 5,
	  lfo_2 => 6,
	  lfo_3 => 7,
	  lfo_4 => 8,
	  filter_env => 9,
	  amp_vca_env => 10,
	  env3 => 11,
	  external_audio input peak => 12,
	  external_audio_env_follower => 13,
	  pitch_bend => 14,
	  mod_wheel => 15,
	  pressure => 16,
	  midi_breath_controller => 17,
	  midi_foot_controller => 18,
	  key_velocity => 19,
	  key_note_number => 20,
	  midi_Expression => 21,
	  noise => 22,
	  osc_3 => 23,
	  osc_4 => 24,
	 }).


%% FIXME!!!
cc() ->
    #{
      
     }.


global_parameter_data() ->
    #{
      ?PARM(0, program_number, 0, 127),   %% Program Number 1 – 128
      ?PARM(1, bank_number, 0, 3),        %% Bank Number 1 – 4
      ?PARM(2, master_volume, 0, 100),    %% Master Volume 0 – 100
      ?PARM(3, master_transpose, 0, 72),  %% Master Transpose; 0 = -36 semitones (- 3 octaves), 36 = 0 (no transpose), and 72 = +36 semitones.
      ?PARM(4, bpm, 30, 250),             %% BPM
      ?PARM(5, clock_divide, 0, 12),      %% Clock Divide
      %% 0   Half Note
      %% 1   Quarter Note
      %% 2   Eighth Note
      %% 3   Eighth Note half swing
      %% 4   Eighth Note full swing
      %% 5   Eighth Note triplets
      %% 6   Sixteenth Note
      %% 7   Sixteenth Note half swing
      %% 8   Sixteenth Note full swing
      %% 9   Sixteenth Note triplets
      %% 10 Thirty-second Notes
      %% 11 Thirty-second Notes triplets
      %% 12 Sixty-Fourth Notes triplets
      ?PARM(6, use_program_tempo, 0, 1),  %% Use Program tempo
      ?PARM(7, midi_clock_select, 0, 6),  %% MIDI clock select
      %% 0    Use Internal clock, don’t send MIDI clock
      %% 1    Use Internal clock, send MIDI clock
      %% 2    Use MIDI clock In
      %% 3    Use MIDI clock In, and retransmit MIDI clock out
      %% 4    No change
      %% 5    No change
      %% 6    Use MIDI clock In, but ignore MIDI Start/Stop
      ?PARM(8, not_used_8, 0, 0),           %% Not used
      ?PARM(9, poly_chain, 0, 19),        %% Poly Chain
      %% 0    No Chaining
      %% 1    2 voices total
      %% 2    3 voices total
      %% .
      %% 19 20 voices total
      ?PARM(10, input_gain, 0, 8),        %% Input Gain
      %% 0    No gain
      %% 1    + 3 db
      %% 2    + 6 db
      %% 3    + 9 db
      %% 4    + 12 db
      %% 5    + 15 db
      %% 6    + 18 db
      %% 7    + 21 db
      %% 8    + 24 db      
      ?PARM(11, master_fine_tune, 0, 100),%% Master Fine Tune
      %%% 0 = -50 cents, 50 = 0 (centered), 100 = + 50 cents
      ?PARM(12, not_used_12, 0, 0),          %% Not used
      ?PARM(13, not_used_13, 0, 0),          %% Not used
      ?PARM(14, midi_channel, 0, 16),     %% MIDI Channel
      %% if = 0, data received on all MIDI channels.
      %% Otherwise = channel number 1 – 16.
      ?PARM(15, not_used_15, 0, 0),          %% Not used
      ?PARM(16, not_used_16, 0, 0),          %% Not used
      ?PARM(17, not_used_17, 0, 0),          %% Not used
      ?PARM(18, midi_program_change_enable, 0, 1), %% MIDI Program Change enable
      ?PARM(19, midi_pressure_enable, 0, 1),       %% MIDI Pressure enable
      ?PARM(20, midi_controller_enable, 0, 1),     %% MIDI Controller enable
      ?PARM(21, midi_sysex_enable, 0, 1),          %% MIDI SysEx enable
      ?PARM(22, pedal_cv1_dst, 0, 5),      %% Pedal/CV 1 destination
      ?PARM(23, pedal_cv2_dst, 0, 5),      %% Pedal/CV 2 destination
      ?PARM(24, velocity_curve, 0, 3),             %% Velocity Curve
      ?PARM(25, pressure_curve, 0, 3),             %% Pressure Curve
      ?PARM(26, local_control, 0, 1),        %% Local Control Off/On
      ?PARM(27, damper_polatity, 0, 1)       %% Damper Polarity
      %% 0 = normally open, 1 = normally closed
     }.

%% Note: Some parameters are not used in order to maintain the closest match
%%  with other Evolver models.

program_parameter_data() ->
    #{
      ?PARM(0, osc_1_freq, 0, 120),          %% Oscillator 1 Frequency
      ?PARM(1, osc_1_fine_tune, 0, 100),     %% Oscillator 1 Fine Tune; 0 = -50 cents, 50 = 0 (centered), 100
      ?PARM(2, osc_1_shape, 0, 102),         %% Oscillator 1 Shape
      %% 0    Sawtooth
      %% 1    Triangle
      %% 2    Sawtooth/triangle mix
      %% 3 – 102 Pulse Wave, Pulse width 0 – 99
      ?PARM(3, osc_1_level, 0, 100),         %% Oscillator 1 Level
      ?PARM(4, osc_2_freq, 0, 120),          %% Oscillator 2 Frequency
      ?PARM(5, osc_2_fine_tune, 0, 100),     %% Oscillator 2 Fine Tune; 0 = -50 cents, 50 = 0 (centered), 100
      ?PARM(6, osc_2_shape, 0, 102),         %% Oscillator 2 Shape
      %% 0    Sawtooth
      %% 1    Triangle
      %% 2    Sawtooth/triangle mix
      %% 3 – 102 Pulse Wave, Pulse width 0 – 99
      ?PARM(7, osc_2_level, 0, 100),         %% Oscillator 2 Level
      ?PARM(8, osc_3_freq, 0, 120),          %% Oscillator 3 Frequency 0 – 120 in semitones (10 octave range)
      ?PARM(9, osc_3_fine_tune, 0, 100),     %% Oscillator 3 Fine Tune; 0 = -50 cents, 50 = 0 (centered), 100
      ?PARM(10, osc_3_shape, 0, 127),        %% Oscillator 3 Shape 1 – 128
      ?PARM(11, osc_3_level, 0, 100),        %% Oscillator 3 Level
      ?PARM(12, osc_4_freq, 0, 120),         %% Oscillator 4 Frequency, 0 – 120 in semitones (10 octave range)
      ?PARM(13, osc_4_fine_tune, 0, 100),    %% Oscillator 4 Fine Tune; 0 = -50 cents, 50 = 0 (centered), 100 = + 50 cents
      ?PARM(14, osc_4_shape, 0, 127),        %% Oscillator 4 Shape 1 – 128
      ?PARM(15, osc_4_level, 0, 100),        %% Oscillator 4 Level
      ?PARM(16, filter_freq, 0, 164),       %% Filter Frequency, steps in semitones
      ?PARM(17, filter_env_amt, 0, 198),    %% Filter Envelope Amount; -99 to +99
      ?PARM(18, filter_env_attack, 0, 110), %% Filter Envelope Attack
      ?PARM(19, filter_env_decay, 0, 110),  %% Filter Envelope Decay
      ?PARM(20, filter_env_sustain, 0, 100),%% Filter Envelope Sustain
      ?PARM(21, filter_env_release, 0, 110),%% Filter Envelope Release
      ?PARM(22, resonance, 0, 100),         %% Resonance
      ?PARM(23, filter_keyboard_amt, 0, 100),%% Filter Keyboard Amount
      ?PARM(24, vca_level, 0, 100),         %% VCA Level
      ?PARM(25, vca_env_amt, 0, 100),       %% VCA Envelope Amount
      ?PARM(26, vca_env_attack, 0, 110),    %% VCA Envelope Attack
      ?PARM(27, vca_env_decay, 0, 110),     %% VCA Envelope Decay
      ?PARM(28, vca_env_sustain, 0, 100),   %% VCA Envelope Sustain
      ?PARM(29, vca_env_release, 0, 110),   %% VCA Envelope Release
      ?PARM(30, output_pan, 0, 6),          %% Output Pan
      %% 0   Left channel panned fully left, Right fully to the right
      %% 1   Left channel panned mostly left, Right mostly to the right
      %% 2   Left channel panned somewhat left, Right somewhat to the right
      %% 3   Mono
      %% 4   Right channel panned somewhat left, Left somewhat to the right
      %% 5   Right channel panned mostly left, Left mostly to the right
      %% 6   Right channel panned fully left, Left fully to the right
      ?PARM(31, program_volume, 0, 100),   %% Program Volume
      ?PARM(32, feedback_freq, 0, 48),     %% Feedback Frequency; steps in semitones
      ?PARM(33, feedback_amt, 0, 100),     %% Feedback Amount
      ?PARM(34, grunge, 0, 1),             %% Grunge; 0 = off, 1 = on
      ?PARM(35, delay1_time, 0, 166),      %% Delay 1 Time
      ?PARM(36, delay1_level, 0, 100),     %% Delay 1 Level
      ?PARM(37, delay_sum_fb_to_delay_in, 0, 100), %% Delay sum feedback to Delay input
      ?PARM(38, delay_sum_fb_to_filter_in, 0, 100), %% Delay sum feedback to Filter input
      ?PARM(39, output_hack_amt, 0, 14),   %% Output Hack Amount
      ?PARM(40, lfo_1_freq, 0, 160),        %% LFO 1 Frequency; 
      %% 150 unsynced frequency
      %% 151 Sequence speed divided by 32
      %% 152 Sequence speed divided by 16
      %% 153 Sequence speed divided by 8
      %% 154 Sequence speed divided by 4
      %% 155 Sequence speed divided by 2
      %% 156 One cycle per step
      %% 157 Two cycles per step
      %% 158 Four cycles per step
      %% 159 Eight cycles per step
      %% 160 Sixteen cycles per step
      ?PARM(41, lfo_1_shape, 0, 4),         %% LFO 1 Shape
      %% 0   Triangle
      %% 1   Reverse Sawtooth
      %% 2   Sawtooth
      %% 3   Pulse (square)
      %% 4   Random
      ?PARM(42, lfo_1_amt, 0,200), %%    LFO 1 Amount (over 100 repeats with Key sync on)
      ?PARM(43, lfo_1_dst, 0,68), %%    LFO 1 Destination (see destination table on page 53)
      ?PARM(44, lfo_2_freq, 0,160), %%   LFO 2 Frequency (same as LFO 1)
      ?PARM(45, lfo_2_shape, 0,4),  %%   LFO 2 Shape (same as LFO 1)
      ?PARM(46, lfo_2_amt, 0,200), %%   LFO 2 Amount (over 100 repeats with Key sync on)
      ?PARM(47, lfo_2_dst,  0,68), %%    LFO 2 Destination (see destination table on page 53)
      ?PARM(48, env_3_amt, 0,198), %%   Envelope 3 Amount; -99 to +99
      ?PARM(49, env_3_dst,  0,68), %%    Envelope 3 Destination (see destination table on page 53)
      ?PARM(50, env_3_attack, 0,110), %%   Envelope 3 Envelope Attack
      ?PARM(51, env_3_decay, 0,110),  %% Envelope 3 Envelope Decay
      ?PARM(52, env_3_sustain,0,100), %%    Envelope 3 Envelope Sustain
      ?PARM(53, env_3_release, 0,110), %%  Envelope 3 Envelope Release
      ?PARM(54, trigger_select, 0,13), %%    Trigger Select
      %% 0    All – The envelopes will be triggered by either the
      %%      sequencer or the keyboard
      %% 1    Seq – The envelopes will be triggered by the sequencer
      %%      only.
      %% 2    The envelopes will be triggered by the keyboard only.
      %% 3    Same, with sequencer reset on Note On
      %% 4    Combo – Envelopes will only be triggered by both the
      %%      keyboard and the sequencer is running
      %% 5    Combo Reset – same, with sequencer reset on Note On
      %% 6    External Input gates the envelopes
      %% 7    External Input gates the envelopes and resets the
      %%      sequencer
      %% 8    External Input gates the sequencer
      %% 9    External Input gates the sequencer and resets the
      %%      sequencer
      %% 10 Keyboard plays sequence once
      %% 11 Keyboard plays sequence once, resetting on multiple
      %%      notes
      %% 12 External Trigger – the sequence plays once on an
      %%      external signal
      %% 13 The sequence plays once when a key is hit
      ?PARM(55, key_off_transpose,  0,73),
      %%   Key Off / Transpose –
      %%   0 = Key pitch ignored. 
      %%   1-36 = semitones keyboard transpose, 
      %%   37 = no transposing, 
      %%   73 = +36 semitones
      ?PARM(56,  seq_1_dst, 0,75), %%    Sequencer 1 Destination (see destination table on page 53)
      ?PARM(57,  seq_2_dst, 0,75), %% Sequencer 2 Destination (see destination table on page 53)
      ?PARM(58,  seq_3_dst, 0, 75), %%    Sequencer 3 Destination (see destination table on page 53)
      ?PARM(59,  seq_4_dst, 0, 75), %%   Sequencer 4 Destination (see destination table on page 53)
      ?PARM(60,  noice_volume,   0, 100), %%   Noise Volume
      ?PARM(61,  ext_input_volume, 0, 100), %%   External Input Volume
      ?PARM(62,  ext_input_mode,  0,2, 
	    #{ %% External Input Mode
	       stereo =>  0, %%    Stereo
	       left => 1,    %% Left Input channel goes to both channels
	       right => 2, %% Right Input channel goes to both channels
	       both => 3 %% Left channel audio, Right channel control
	     }), %% Right channel audio, Left channel control
      ?PARM(63, input_hack_amt, 0, 14),  %%  Input Hack Amount
      ?PARM(64, osc1_glide, 0, 200), 
      %%   Glide, Oscillator 1; 101 – 199 = Fingered; 200 = osc midi off
      ?PARM(65, sync, 0, 1),       %% Sync; 0 = off, 1 = on
      ?PARM(66, program_tempo, 30, 250), %%      Program tempo
      ?PARM(67, program_clock_device, 0, 12), %% Program Clock Divide (see Master Clock Divide for selections)
      ?PARM(68, osc_2_glide, 0,200), %%    Glide, Oscillator 2; 101 – 199 = Fingered; 200 = osc midi off
      ?PARM(69, osc_slope, 0,5), %%      Oscillator Slop

      ?PARM(70, pitch_bend_range, 0, 12), %%   Pitch Bend Range, in semitones
      ?PARM(71, key_mode,  0,23), %%    Key Mode
      %% 0    Low note priority
      %% 1    Low note priority with re-trigger
      %% 2    High note priority
      %% 3    High note priority with re-trigger
      %% 4    Last note hit priority
      %% 5    Last note hit priority with re-trigger      
      %% Add 0 for Poly, 6 for Mono, 12 for Unison 1, and 18 for Unison
      %% 2 to the above
      ?PARM(72, osc_3_glide,      0, 200), %%   Glide, Oscillator 3; 101 – 199 = Fingered; 200 = osc midi off
      ?PARM(73, fm_osc_3_osc_3,      0 , 100  ), %%  FM, Oscillator 4 to Oscillator 3
      ?PARM(74, osc_3_shape_mod,      0,4   ), %%      Shape Mod Oscillator 3; 0 = Off, 1 = Sequence 1, etc.
      ?PARM(75, osc_4_osc_3_ring_mod,      0 , 100 ), %%    Ring Mod, Oscillator 4 to Oscillator 3
      ?PARM(76, osc_4_glide,      0 , 200 ), %%    Glide, Oscillator 4; 101 – 199 = Fingered; 200 = osc midi off
      ?PARM(77, fm_osc_3_to_osc_4,   0 , 100  ), %%   FM, Oscillator 3 to Oscillator 4
      ?PARM(78, osc_4_shape_mod,    0, 4     ), %%    Shape Mod Oscillator 4; 0 = Off, 1 = Sequence 1, etc
      ?PARM(79, osc_3_to_osc_4_ring_mod,      0 , 100  ), %%   Ring Mod, Oscillator 3 to Oscillator 4
      ?PARM(80, pole_select, 0, 1     ), %%    2/4 Pole Select; 0 = 2 Pole, 1 = 4 Pole
      ?PARM(81, filter_env_velocity,      0 , 100  ), %%   Filter Envelope Velocity
      ?PARM(82, filter_audio_modulation,     0 , 100  ), %%   Filter Audio Modulation
      ?PARM(83, filter_splt,      0 , 100 ), %%   Filter Split
      ?PARM(84, highpass_filter_cutoff,      0 , 199 ), %%   Highpass Filter cutoff. 0-99 for filter on output; 100 – 199 for levels 0-99 with filter on input
      ?PARM(85, mod_1_src,   0 , 24  ), %%   Modulation 1 Source (see Source Table on page 55)
      ?PARM(86, mod_1_amt,      0 , 198 ), %%   Modulation 1 Amount; -99 to +99
      ?PARM(87, mod_1_dst,      0 , 68  ), %%   Modulation 1 Destination (see destination table on page 53)
      ?PARM(88, lin_exp_env,      0, 1    ), %%    Linear/Exponential envelopes 0 = Exponential, 1 = Linear
      ?PARM(89, vca_env_velocity,      0, 100  ), %%  VCA Envelope Velocity
      ?PARM(90, mod_2_src,      0 , 24  ), %%   Modulation 2 Source (see Source Table on page 55)
      ?PARM(91, mod_2_amt,     0 , 198 ), %%   Modulation 2 Amount; -99 to +99
      ?PARM(92, mod_2_dst,      0 , 68  ), %%   Modulation 2 Destination (see destination table on page 53)
      ?PARM(93, mod_3_src,      0 , 24  ), %%   Modulation 3 Source (see Source Table on page 55)
      ?PARM(94, mod_3_amt,      0 , 198 ), %%   Modulation 3 Amount; -99 to +99
      ?PARM(95, mod_3_dst,      0 , 68  ), %%   Modulation 3 Destination (see destination table on page 53)
      ?PARM(96, mod_4_src,     0 , 24   ), %%  Modulation 4 Source (see Source Table on page 55)
      ?PARM(97, mod_4_amt,  0 , 198  ), %%  Modulation 4 Amount; -99 to +99
      ?PARM(98, mod_4_dst,     0 , 68   ), %%  Modulation 4 Destination (see destination table on page 53)
      ?PARM(99, delay_2_time,     0 , 166  ), %%  Delay 2 Time
      ?PARM(100, delay_2_level,     0 , 100 ), %%   Delay 2 Level
      ?PARM(101, delay_3_time,      0 , 166 ), %%   Delay 3 Time
      ?PARM(102, delay_3_level,     0 , 100 ), %%   Delay 3 Level
      ?PARM(103, distortion,     0 , 199 ), %%   Distortion; 0-99 for distortion on output; 100 – 199 for levels  0-99 with distortion on input
      ?PARM(104, lfo_3_freq,     0 , 160 ), %%   LFO 3 Frequency (same as LFO 1)
      ?PARM(105, lfo_3_shape,     0,4     ), %%   LFO 3 Shape (same as LFO 1)
      ?PARM(106, lfo_3_amt,  0 , 200 ), %%   LFO 3 Amount (over 100 repeats with Key sync on)
      ?PARM(107, lfo_3_dst,  0 , 68  ), %%   LFO 3 Destination (see destination table on page 53)
      ?PARM(108, lfo_4_freq, 0 , 160 ), %%   LFO 4 Frequency (same as LFO 1)
      ?PARM(109, lfo_4_shape, 0 , 4   ), %%     LFO 4 Shape (same as LFO 1)
      ?PARM(110, lfo_4_amt,   0 , 200 ), %%   LFO 4 Amount (over 100 repeats with Key sync on)
      ?PARM(111, lfo_4_dst,  0 , 68  ), %%   LFO 4 Destination (see destination table on page 53)
      ?PARM(112, env_3_delay,     0 , 100 ), %%   Envelope 3 Delay
      ?PARM(113, env_3_velocity,     0 , 100 ), %%   Envelope 3 Velocity
      ?PARM(114, ext_input_peak_amt,     0 , 198 ), %%   External Input Peak Amount; -99 to +99
      ?PARM(115, ext_input_peak_dst,  0 , 68  ), %%   External Input Peak Destination (see destination table on  page 53)
      ?PARM(116, ext_input_env_follower_amt,     0 , 198 ), %%   External Input Envelope Follower Amount; -99 to +99
      ?PARM(117, ext_input_env_follower_dst,      0 , 68 ), %%    External Input Envelope Follower Destination (see destination table on page 53)
      ?PARM(118, velocity_amt,      0 , 198 ), %%   Velocity Amount; -99 to +99
      ?PARM(119, velocity_dst,      0 , 68 ), %%    Velocity Destination (see destination table on page 53)
      ?PARM(120, mod_wheel_amt,     0 , 198 ), %%   Mod Wheel Amount; -99 to +99
      ?PARM(121, mod_wheel_dst,     0 , 68  ), %%   Mod Wheel Destination (see destination table on page 53)
      ?PARM(122, pressure_amt,     0 , 198 ), %%   Pressure Amount; -99 to +99
      ?PARM(123, pressure_dst,     0 , 68  ), %%   Pressure Destination (see destination table on page 53)
      ?PARM(124, breath_controller_amt,     0 , 198 ), %%   Breath Controller Amount; -99 to +99
      ?PARM(125, breath_controller_dst,  0 , 68 ), %%    Breath Controller Destination (see destination table on page 53)
      ?PARM(126, foot_controller_amt,      0 , 198 ), %%   Foot Controller Amount; -99 to +99
      ?PARM(127, foot_controller_dst,     0 , 68 ) %%   Foot Controller Destination (see destination table on page 53)
     }.



%% verify a nrmn parameter againt the dump,
verify_dump(S, Bin0, Param, Value) ->
    {ok,_} = transmit_program_parameter(S, Param, Value),
    case diff_edit_buffer(S, Bin0) of
	[] -> %% set same value as in dump or non existing parameter
	    not_found_in_dump;
	[{DumpNo, OldValue, Value}] ->
	    io:format("dump index ~p\n", [DumpNo]),
	    io:format("new value = ~p\n", [Value]),
	    io:format("old value = ~p\n", [OldValue]),
	    DumpValue = maps:get(DumpNo, program_parameter_data(), undefined),
	    {ok,_} = transmit_program_parameter(S, Param, OldValue),  %% restore
	    case DumpValue of
		undefined -> %% not found in dump
		    {error, {Param, not_found_in_dump}};
		{Name, {Min, Max, _}} when Value >= Min, Value =< Max ->
		    if Param =:= DumpNo ->
			    {ok, DumpNo};
		       Param =:= Name ->
			    {ok, DumpNo};
		       true ->
			    {error, {name_mismatch, Name}}
		    end
	    end
    end.
    
      

%% require -config prophet6.config
open() ->
    case midi:setup_synth() of
	{ok,#{ device:=Device} } -> 
	    open(Device);
	Error -> Error
    end.

open(Device) ->
    midi:open(Device,[event,binary,running]).

close(Synth) ->
    midi:close(Synth).


device_inquiry(Synth) ->
    midi:device_inquiry(Synth).

device_inquiry(Synth, Chan) ->
    case midi:device_inquiry(Synth, Chan) of
	{ok, Info=#{ id := 1, family := Family }} ->
	    Name = case Family band 16#7f of
		       ?PROPHET_8_ID    -> prophet_8;
		       ?PROPHET_6_ID    -> prophet_6;
		       ?PROPHET_REV2_ID -> prophet_rev2;
		       ?PROPHET_X_ID    -> prophet_x;
		       ?PROPHET_5_ID    -> prophet_5;
		       ?EVOLVER_ID      -> evolver;
		       ?EVOLVER_KEYBOARD_ID -> evolver_keyboard;
		       _ -> {unknown, Family}
		   end,
	    {ok, Info#{model_name=>Name}};
	Error ->
	    Error
	end.

program_dump(Synth, Bank, Program) when Bank >= 1, Bank =< 4,
					Program >= 1, Program =< 128 ->
    midi:flush(Synth),
    request_program_dump(Synth, Bank, Program),
    timer:sleep(100),
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?EVOLVER_ID:7,
	       0:1, ?FILE_VERSION:7, %% 1
	       0:1, ?PROGRAM_DATA:7, %% Program Data
	       0:1, Bank0:7,
	       0:1, Program0:7,
	       Bin/binary>>} ->
	    %% Bin should be 220 bytes packed,
	    %% 128 bytes of program parameters, 64 bytes of sequence data
	    io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    Bin1 = unpack_ms_bit(Bin),
	    %% Data = decode_program(Bin1, program_parameter_data()),
	    %% the second packet may already be in message queue, use recv_sys
	    case midi:recv_sys(Synth) of
		{ok, <<0:1, ?DSI_ID_HEADER:7,
		       0:1, ?EVOLVER_ID:7,
		       0:1, ?FILE_VERSION:7, %% 1
		       0:1, ?PROGRAM_NAME_DATA:7, %% Program Data
		       0:1, _Bank1:7,
		       0:1, _Program1:7,
		       Name/binary>>} ->
		    io:format("|Name|=~w, Name=~p\n", [byte_size(Name),Name]),
		    {ok,#{bank=>Bank0+1,
			  program=>Program0+1,
			  name=>Name,
			  data=>Bin1}};
		{ok, Bin} ->
		    {error, {unexpeced, Bin}};
		Error ->
		    Error
	    end;
	{ok, Bin} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    {error, {unexpeced, Bin}};
	Error ->
	    Error
    end.

%% 96-127 may be user writable
%% ROM waveshapes are 12 bits
%% User waveshapes can use upto 16 bits (little endian)
waveshape_data_dump(Synth, Num) when Num >= 1, Num =< 128 ->
    midi:flush(Synth),
    request_waveshape_dump(Synth, Num),
    timer:sleep(100),
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?EVOLVER_ID:7,
	       0:1, ?FILE_VERSION:7, %% 1
	       0:1, ?WAVESHAPE_DATA:7,  %% Waveshape DATA
	       0:1, Num0:7,
	       Bin/binary>>} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    UnpackedBin = unpack_ms_bit(Bin),
	    {ok,#{shape=>Num0+1,data=>UnpackedBin}};
	{ok, Bin} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    {error, {unexpeced, Bin}};
	Error ->
	    Error
	end.

transmit_program_parameter(Synth, ParamName, Value) when is_atom(ParamName) ->
    Param = maps:get(ParamName, program_parameter_data()),
    transmit_program_parameter(Synth, Param, Value);
transmit_program_parameter(Synth, Param, Value) ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?EVOLVER_ID:7,
		 0:1, ?FILE_VERSION:7, %% 1
		 0:1, ?PROGRAM_PARAMETER:7, %% Program Parameter
		 0:1, Param:7,
		 0:1, (Value band 16#f):7,
		 0:1, ((Value bsr 4) band 16#f):7,
		 ?MIDI_EVENT_SYS:4, 7:4>>).    

%% Step value v = 0-100
%% Reset = 255?  Rest = 102?

transmit_sequencer_tables(Synth, Seq1, Seq2, Seq3, Seq4) when
      is_list(Seq1), is_list(Seq2), is_list(Seq3), is_list(Seq4) ->
    transmit_sequencer_list(Synth, 1, 1, Seq1),
    transmit_sequencer_list(Synth, 2, 1, Seq2),
    transmit_sequencer_list(Synth, 3, 1, Seq3),
    transmit_sequencer_list(Synth, 4, 1, Seq4).

transmit_sequencer_list(Synth, Seq, Step, [Value|Vs]) when Step =< 16 ->
    transmit_sequencer_parameter(Synth, Seq, Step, Value),
    transmit_sequencer_list(Synth, Seq, Step+1, Vs);
transmit_sequencer_list(_Synth, _Seq, _, []) ->
    ok.

transmit_sequencer_parameter(Synth, Seq, Step, Value) when
      Seq >= 1, Seq =< 4, Step >= 1, Step =< 16, Value >= 0, Value =< 255 ->
    transmit_sequencer_parameter_(Synth, (Seq-1)*16 + (Step-1), Value).
     
transmit_sequencer_parameter_(Synth, Step, Value) when
      Step >= 0, Step =< 64, Value >= 0, Value =< 255 ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?EVOLVER_ID:7,
		 0:1, ?FILE_VERSION:7, %% 1
		 0:1, ?SEQUENCE_PARAMETER:7, %% Program Parameter
		 0:1, Step:7,  %% 1:0-15, 2:16-31, 3:32-47, 4:48-63
		 0:1, (Value band 16#f):7,
		 0:1, ((Value bsr 4) band 16#f):7,
		 ?MIDI_EVENT_SYS:4, 7:4>>).    

transmit_global_parameter(Synth, Param, Value) ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?EVOLVER_ID:7,
		 0:1, ?FILE_VERSION:7, %% 1
		 0:1, ?GLOBAL_PARAMETER:7, %% Global Parameter
		 0:1, Param:7,             %% 0-27
		 0:1, (Value band 16#f):7,
		 0:1, ((Value bsr 4) band 16#f):7,
		 ?MIDI_EVENT_SYS:4, 7:4>>).

int16(X) when X < -1.0 ->  -32767;
int16(X) when X >  1.0 ->   32767;
int16(X) -> trunc(X * 32767).

make_samples(N, Function) ->
    make_samples(N, 0.0, (2.0*math:pi())/N, Function).

make_samples(0, _X, _Step, _Function) ->
    [];
make_samples(N, X, Step, Function) ->
    [ int16(Function(X)) | make_samples(N-1, X+Step, Step, Function) ].

transmit_wav(Synth, Num, File) ->
    transmit_wav(Synth, Num, 1.0, 0, File).

transmit_wav(Synth, Num, Scale, File) ->
    transmit_wav(Synth, Num, Scale, 0, File).

transmit_wav(Synth, Num, Scale, Offset, File) ->
    case alsa_util:read_file(File) of
	{ok, {#{ format := SrcFormat, channels := SrcChannels }, Data}} ->
	    <<_:Offset/binary, Data1/binary>> = Data,
	    Data2 = alsa_samples:reformat(SrcFormat, s16_le,
					  SrcChannels, 1, Data1),
	    SrcRate = 16000,
	    DstRate = trunc(16000*Scale),
	    Data3 = alsa_samples:resample(SrcRate, DstRate, s16_le, 1, Data2),
	    <<Data4:256/binary, _/binary>> = Data3,
	    transmit_waveshape_data(Synth, Num, Data4);
	Error ->
	    Error
    end.
	    

-spec transmit_waveshape(Synth::pid(), Num::integer(),
			 Function::function()) -> {ok, 301}.
transmit_waveshape(Synth, Num, Function) when is_function(Function) ->
    Samples = make_samples(128, Function),
    transmit_waveshape_data(Synth, Num, Samples).

%% Bin is 128 16-bit little endian data
transmit_waveshape_data(Synth, Num, Samples) when is_list(Samples) ->
    Bin = << <<X:16/signed-little>> || X <- Samples >>,
    transmit_waveshape_data(Synth, Num, Bin);
transmit_waveshape_data(Synth, Num, Bin) when Num >= 97, Num =< 128,
					      byte_size(Bin) =:= 256 ->
    PackedBin = pack_ms_bit(Bin),
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?EVOLVER_ID:7,
		 0:1, ?FILE_VERSION:7, %% 1
		 0:1, ?WAVESHAPE_DATA:7,  %% Waveshape DATA
		 0:1, (Num-1):7,          %% 96-127
		 PackedBin/binary,
		 ?MIDI_EVENT_SYS:4, 7:4>>).

get_edit_buffer(Synth) ->
    midi:flush(Synth),
    request_program_edit_buffer(Synth),
    timer:sleep(100),
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?EVOLVER_ID:7,
	       0:1, ?FILE_VERSION:7, %% 1
	       0:1, ?EDIT_BUFFER_DATA:7, %% Edit Buffer Data
	       Bin/binary>>} ->
	    %% Bin should be 220 bytes packed,
	    %% 128 bytes of program parameters, 64 bytes of sequence data
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    %% 128 bytes of program parameters, 64 bytes of sequence data
	    {ok, unpack_ms_bit(Bin)};
	{ok, Bin} ->
	    {error, {unexpeced, Bin}};
	Error ->
	    Error
	end.

decode_edit_buffer(Synth) ->
    {ok, Bin} = get_edit_buffer(Synth),
    decode_program(Bin, program_parameter_data()).

dump_edit_buffer(Synth) ->
    {ok, Bin} = get_edit_buffer(Synth),
    [ io:format("~s : ~p\n", [binary:encode_hex(Bin16), Bin16]) || 
	<<Bin16:16/binary>> <= Bin ].

diff_edit_buffer(Synth, Bin0) ->
    {ok, Bin1} = get_edit_buffer(Synth),    
    diff_bytes(0, Bin0, Bin1, []).

diff_bytes(Addr, <<B0,Bin0/binary>>, <<B1,Bin1/binary>>, Diff) ->
    if B0 =:= B1 ->
	    diff_bytes(Addr+1, Bin0, Bin1, Diff);
       true ->
	    diff_bytes(Addr+1, Bin0, Bin1, [{Addr,B0,B1}|Diff])
    end;
diff_bytes(_Addr, _Bin0, _Bin1, Diff) ->
    lists:reverse(Diff).


program_edit_buffer_dump(Synth) ->
    midi:flush(Synth),
    request_program_edit_buffer(Synth),
    timer:sleep(100),
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?EVOLVER_ID:7,
	       0:1, ?FILE_VERSION:7, %% 1
	       0:1, ?EDIT_BUFFER_DATA:7, %% Edit Buffer Data
	       Bin/binary>>} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    Bin1 = unpack_ms_bit(Bin),
	    Data = decode_program(Bin1, program_parameter_data()),
	    {ok,#{data=>Data}};
	{ok, Bin} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    {error, {unexpeced, Bin}};
	Error ->
	    Error
	end.

request_program_dump(Synth, Bank, Program) when
      is_integer(Bank), Bank >= 1, Bank =< 4,
      is_integer(Program), Program >= 1, Program =< 128 ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?EVOLVER_ID:7,
		 0:1, ?FILE_VERSION:7, %% 1
		 0:1, ?SYS_REQUEST_PROGRAM_TRANSMIT:7,
		 0:1, (Bank-1):7, %% Bank 0-3
		 0:1, (Program-1):7, %%  Program 0-127
		 ?MIDI_EVENT_SYS:4, 7:4>>).

request_program_edit_buffer(Synth) ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?EVOLVER_ID:7,
		 0:1, ?FILE_VERSION:7, %% 1
		 0:1, ?SYS_REQUEST_PROGRAM_EDIT_BUFFER_TRANSMIT:7,
		 ?MIDI_EVENT_SYS:4, 7:4>>).

global_parameter_dump(Synth) ->
    request_global_parameter_dump(Synth),    
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?EVOLVER_ID:7,
	       0:1, ?FILE_VERSION:7, %% 1
	       0:1, 15:7,          %% Main Parameter Data
	       Bin/binary>>} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    Params = unpack_global_params(Bin),
	    {ok,Params};
	{ok, Bin} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    {error, {unexpeced, Bin}};
	Error ->
	    Error
    end.

request_waveshape_dump(Synth, Num) when Num >= 1, Num =< 128 ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?EVOLVER_ID:7,
		 0:1, ?FILE_VERSION:7, %% 1
		 0:1, ?SYS_REQUEST_WAVESHAPE_TRANSMIT:7,
		 0:1, (Num-1):7,
		 ?MIDI_EVENT_SYS:4, 7:4>>).

request_global_parameter_dump(Synth) ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?EVOLVER_ID:7,
		 0:1, ?FILE_VERSION:7, %% 1
		 0:1, ?SYS_REQUEST_GLOBAL_PARAMETER_TRANSMIT:7,
		 ?MIDI_EVENT_SYS:4, 7:4>>).

%% 7 bytes is packed in 8 bytes

%% example: packed_size(128+64=192) = 220) = 192 = 128 + 64!
packed_size(NBytes) when is_integer(NBytes), NBytes>=0 -> 
    (NBytes*8+7) div 7;
packed_size(Bin) when is_binary(Bin) ->
    packed_size(byte_size(Bin)).


%% 8 bytes is unpacked in 7 bytes
%% example: unpacked_size(220) = 192 = 128 + 64!
unpacked_size(NBytes) when is_integer(NBytes), NBytes>=0 ->
    (NBytes*7+7) div 8;
unpacked_size(Bin) ->
    unpacked_size(byte_size(Bin)).

%% byte_size(Bin) rem 7 = 0
pack_ms_bit(Bin) when is_binary(Bin) ->
    list_to_binary(pack_ms_bit_(Bin)).

pack_ms_bit_(<<A7:1,A:7, B7:1,B:7, C7:1,C:7, D7:1,D:7,
	       E7:1,E:7, F7:1,F:7, G7:1,G:7, Data/binary>>) ->
    [<<0:1,G7:1,F7:1,E7:1,D7:1,C7:1,B7:1,A7:1,A,B,C,D,E,F,G>> |
     pack_ms_bit(Data)];
pack_ms_bit_(<<A7:1,A:7, B7:1,B:7, C7:1,C:7, D7:1,D:7, E7:1,E:7, F7:1,F:7>>) ->
    [<<0:1,0:1,F7:1,E7:1,D7:1,C7:1,B7:1,A7:1,A,B,C,D,E,F>>];
pack_ms_bit_(<<A7:1, A:7, B7:1, B:7, C7:1, C:7, D7:1, D:7, E7:1, E:7>>) ->
    [<<0:1,0:1,0:1,E7:1,D7:1,C7:1,B7:1,A7:1,A,B,C,D,E>>];
pack_ms_bit_(<<A7:1, A:7, B7:1, B:7, C7:1, C:7, D7:1, D:7>>) ->
    [<<0:1,0:1,0:1,0:1,D7:1,C7:1,B7:1,A7:1,A,B,C,D>>];
pack_ms_bit_(<<A7:1, A:7, B7:1, B:7, C7:1, C:7>>) ->
    [<<0:1,0:1,0:1,0:1,0:1,C7:1,B7:1,A7:1,A,B,C>>];
pack_ms_bit_(<<A7:1, A:7, B7:1, B:7>>) ->
    [<<0:1,0:1,0:1,0:1,0:1,0:1,B7:1,A7:1,A,B>>];
pack_ms_bit_(<<A7:1, A:7>>) ->
    [<<0:1,0:1,0:1,0:1,0:1,0:1,0:1,A7:1,A>>];
pack_ms_bit_(<<>>) ->
    [].


unpack_ms_bit(<<0:1,G7:1,F7:1,E7:1,D7:1,C7:1,B7:1,A7:1,
		0:1,A:7,0:1,B:7,0:1,C:7,0:1,D:7,0:1,E:7,0:1,F:7,0:1,G:7, 
		Data/binary>>) ->
    Data1 = unpack_ms_bit(Data),
    <<A7:1, A:7, B7:1, B:7, C7:1, C:7, D7:1, D:7,
      E7:1, E:7, F7:1, F:7, G7:1, G:7, Data1/binary>>;
unpack_ms_bit(<<0:1,G7:1,F7:1,E7:1,D7:1,C7:1,B7:1,A7:1,Bin/binary>>) ->
    unpack_bytes([A7,B7,C7,D7,E7,F7,G7], Bin);
unpack_ms_bit(<<>>) ->
    <<>>.

unpack_bytes([A7|Bs], <<0:1,A:7,Bin/binary>>) ->
    Bin1 = unpack_bytes(Bs, Bin),
    <<A7:1,A:7, Bin1/binary>>;
unpack_bytes(_, <<>>) ->
    <<>>.

unpack_global_params(Data) ->
    [try global_to_name(Param) of
	 {Name,_Range} ->
	     {Name, Value}
     catch
	 error:_ ->
	     {Param, Value}
     end || {Param, Value} <- midi_sysex:unpack_nibbles(0, Data)].

decode_program(Bin) when is_binary(Bin) ->
    decode_program(Bin, program_parameter_data()).

decode_program(Data, Map) ->
    decode_program(0, Data, Map, []).

decode_program(128, SeqData, _Map, Acc) ->
    Seq = decode_seq_data(SeqData),
    {lists:reverse(Acc), Seq};
decode_program(Param, <<C,Data/binary>>, Map, Acc) ->
    try maps:get(Param, Map) of
	{ParamName,_Range} ->
	    io:format("~s: ~w\n", [ParamName, C]),
	    decode_program(Param+1, Data, Map, [{ParamName,C}|Acc])
    catch
	error:_ ->
	    decode_program(Param+1, Data, Map, [{Param,C}|Acc])
    end;
decode_program(_, <<>>, _Map, Acc) ->
    lists:reverse(Acc).

%% Data for 4 sequencers of 16 steps 8 bit each
%% Example:
%%  seq_1_dst = midi_note_out             ( 70 )
%%  seq_2_dst = midi_velocity_out         ( 71 )
%%  seq_3_dst = filter_1_left_cutoff_freq ( 64 )
%%  seq_4_dst = lfo_1_freq                ( 38 )
%%  set seq 1 step 1 to 60
%%  set seq 2 step 1 to 100
%%  set seq 3 step 1 to 50
%%  set seq 4 step 1 to 50

decode_seq_data(SeqData) ->
    <<Seq1:16/binary,Seq2:16/binary,Seq3:16/binary,Seq4:16/binary>> = SeqData,
    {Seq1, Seq2, Seq3, Seq4}.
    

global_to_name(Param) when is_integer(Param), Param >= 0, Param =< 27 ->
    maps:get(Param, global_parameter_data());
global_to_name(Param) when is_atom(Param) ->
    Param.

check_range({Min,Max,_Enum}, Value) when is_integer(Value), 
					 Value >= Min, Value =< Max ->
    Value;
check_range({Min,Max,Enum}, Value) when is_atom(Value) ->
    Value1 = maps:get(Value, Enum),
    if Value1 >= Min, Value1 =< Max -> Value1; 
       true -> error(out_of_range)
    end.
