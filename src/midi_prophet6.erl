%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Prophet 6 parameter settings
%%% @end
%%% Created :  3 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(midi_prophet6).

-export([open/0, open/1, close/1]).
-export([transmit_param/3, transmit_param/4]).
-export([transmit_name/3]).
-export([transmit_seq/3]).
-export([device_inquiry/1, device_inquiry/2]).
-export([program_dump/3, request_program_dump/3]).
-export([program_edit_buffer_dump/1, request_program_program_edit_buffer/1]).
-export([global_parameter_dump/1, request_global_parameter_dump/1]).

-export([name_to_nrpn/1, nrpn_to_name/1]).
-export([nrpn_names/0]).
-export([nrpn_list/0]).
-export([nrpn/0]).
-export([cc/0]).
-export([dump/0]).

-export([pack_ms_bit/1, unpack_ms_bit/1]).
-export([unpack_global_params/1]).
-export([decode_program/2, decode_program/4]).
%% reverse engineer
-export([get_edit_buffer/1, diff_edit_buffer/2, diff_bytes/4]).
-export([dump_edit_buffer/1, decode_edit_buffer/1]).
-export([verify_nrpn_dump/4]).

-include("../include/midi.hrl").

-define(DSI_ID_HEADER, 1).

-define(PROPHET_8_ID,      16#23).
-define(PROPHET_6_ID,      16#2d).
-define(PROPHET_REV2_ID,   16#2f).
-define(PROPHET_X_ID,      16#30).
-define(PROPHET_5_ID,      16#33).  %% 16#31 or 16#33 ? (manual is ambigous)
-define(PROPHET_PRO_3_ID,  16#31).
-define(TRIGON_6_ID,       16#2D).  %% 0010 1101 (same as Prophet-6)???

-define(SYS_REQUEST_PROGRAM_DUMP, 5).
-define(SYS_REQUEST_PROGRAM_EDIT_BUFFER, 6).
-define(SYS_REQUEST_GLOBAL_PARAMETER_DUMP, 14).
-define(SYS_REQUEST_GLOBAL_PROGRAM_DUMP, 5).

-define(PARM(Param, Name),
	(Param) => {Name, {0,127,#{}}},
	(Name) => Param).

-define(PARM(Param, Name, Min, Max),
	(Param) => {Name, {Min,Max,#{}}},
	(Name) => Param).

-define(PARM(Param, Name, Min, Max, Enum),
	(Param) => {Name, {Min,Max,Enum}},
	(Name) => Param).

-define(ONOFF, #{off=>0, on=>1}). %% 0-1

cc() ->
    #{
      ?PARM(0, bank_select_msb),
      ?PARM(1, pulse_width),
      ?PARM(3, bpm),
      ?PARM(4, foot_controller),
      ?PARM(5, glide_mode),
      ?PARM(6, data_entry_msb),
      ?PARM(7, midi_volume),
      ?PARM(8, sub_osc_level),
      ?PARM(9, distortion_amount),
      
      ?PARM(38, data_entry_lsb),
      ?PARM(39, volume_lsb),
      ?PARM(40, vca_env_amt),
      ?PARM(41, vca_env_vel_amt),
      ?PARM(43, vca_env_attack),
      ?PARM(44, vca_env_decay),
      ?PARM(45, vca_env_sustain),
      ?PARM(46, vca_env_release),
      ?PARM(47, low_pass_env_amt),
      ?PARM(50, filter_env_attack),
      ?PARM(51, filter_env_decay),
      ?PARM(52, filter_env_sustain),
      ?PARM(53, filter_env_release),
      ?PARM(54, high_pass_env_amt),

      ?PARM(58, arp_on_off),
      ?PARM(59, arp_mode),
      ?PARM(60, arp_range),
      ?PARM(62, arp_time_signature),
      ?PARM(64, damper_pedal),
      ?PARM(65, glide_on_off),
      ?PARM(67, osc_1_freq),
      ?PARM(69, osc_1_level),
      ?PARM(70, osc_1_shape),
      ?PARM(71, osc_1_mod_wheel),
      ?PARM(74, brightness),
      ?PARM(75, osc_2_freq),
      ?PARM(76, osc_2_freq_fine),
      ?PARM(77, osc_2_level),
      ?PARM(78, osc_2_shape),
      ?PARM(79, osc_2_pulse_width),
      ?PARM(96, data_increment),
      ?PARM(97, data_decrement),
      ?PARM(98, nrpn_param_lsb),
      ?PARM(99, nrpn_param_msb),
      ?PARM(100, rpn_param_lsb),
      ?PARM(101, rpn_param_msb),
      ?PARM(102, low_pass_freq),
      ?PARM(103, low_pass_resonance),
      ?PARM(104, low_pass_key_amt),
      ?PARM(105, low_pass_vel_on_off),
      ?PARM(106, high_pass_freq),
      ?PARM(107, high_pass_resonance),
      ?PARM(108, high_pass_key_amt),
      ?PARM(109, high_pass_vel_on_off),
      ?PARM(120, all_sound_off),
      ?PARM(121, reset_controllers),
      ?PARM(122, local_control_on_off),
      ?PARM(123, all_notes_off),
      ?PARM(124, omni_mode_off),
      ?PARM(125, omni_mode_on),
      ?PARM(126, mono_mode_on),
      ?PARM(127, poly_mode_on)
}.


nrpn() ->
    #{ 
       %% PROGRAM PARAMETER DATA
       ?PARM(0, osc_1_freq, 0, 60),
       ?PARM(1, osc_1_sync, 0, 1),
       ?PARM(2, osc_1_level, 0, 127),
       ?PARM(3, osc_1_shape, 0, 254),
       ?PARM(4, osc_1_pulse_width, 0, 255),
       ?PARM(5, osc_2_freq, 0, 60),
       ?PARM(6, osc_2_freq_fine, 0, 254),
       ?PARM(7, osc_2_level, 0, 127),
       ?PARM(8, osc_2_shape, 0, 254),
       ?PARM(9, osc_2_pulse_width, 0, 255),
       ?PARM(10, osc_2_low_freq, 0, 1, ?ONOFF),
       ?PARM(11, osc_2_key_on_off, 0, 1, ?ONOFF),
       ?PARM(27, osc_1_sub_level, 0, 127),
       ?PARM(28, glide_mode, 0, 3),
       ?PARM(29, glide_on_off, 0, 1, ?ONOFF),
       ?PARM(30, glide_rate, 0, 127),
       ?PARM(31, pbend_range, 0, 24),
       ?PARM(32, noise_level, 0, 127),
       ?PARM(33, slop, 0, 127),
       ?PARM(45, low_pass_freq, 0, 164),
       ?PARM(46, low_pass_res, 0, 255),
       ?PARM(47, low_pass_key_amt, 0, 2),
       ?PARM(48, low_pass_vel_on, 0, 1, ?ONOFF),
       ?PARM(49, high_pass_freq, 0, 164),
       ?PARM(50, high_pass_res, 0, 255),
       ?PARM(51, high_pass_key_amt, 0, 2),
       ?PARM(52, high_pass_vel_on, 0, 1, ?ONOFF),
       ?PARM(62, voice_volume, 0, 127),
       ?PARM(63, pan_spread, 0, 127),
       ?PARM(64, distortion_amt, 0, 127),
       ?PARM(66, vca_env_amt, 0, 127),
       ?PARM(67, vca_env_attack, 0, 127),
       ?PARM(68, vca_env_decay, 0, 127),
       ?PARM(69, vca_env_sustain, 0, 127),
       ?PARM(70, vca_env_release, 0, 127),
       ?PARM(71, vca_env_vel_on_off, 0, 1, ?ONOFF),
       ?PARM(77, low_pass_env_amt, 0, 254),
       ?PARM(78, filter_env_attack, 0, 127),
       ?PARM(79, filter_env_decay, 0, 127),
       ?PARM(80, filter_env_sustain, 0, 127),
       ?PARM(81, filter_env_release, 0, 127),
       ?PARM(82, high_pass_env_amt, 0, 254),
       ?PARM(88, lfo_freq, 0, 254),
       ?PARM(89, lfo_initial_amt, 0, 255),
       ?PARM(90, lfo_shape, 0, 4),
       ?PARM(91, lfo_sync, 0, 1, ?ONOFF),
       ?PARM(93, lfo_freq_1_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(94, lfo_freq_2_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(95, lfo_pw_1_2_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(96, lfo_amp_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(97, lfo_low_pass_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(98, lfo_high_pass_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(109, pressure_amt, 0, 254),
       ?PARM(110, pressure_freq_1_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(111, pressure_freq_2_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(112, pressure_low_pass_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(113, pressure_high_pass_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(114, pressure_vca_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(115, pressure_lfo_amt_dest_on_off, 0, 1, ?ONOFF),
       ?PARM(119, fx_1_select, 0, 9),
       ?PARM(120, fx_1_mix, 0, 127),
       ?PARM(121, fx_1_param_1, 0, 255),
       ?PARM(122, fx_1_param_2, 0, 127),
       ?PARM(123, fx_1_sync, 0, 1, ?ONOFF),

       ?PARM(127, fx_2_select, 0, 13),
       ?PARM(128, fx_2_mix, 0, 127),
       ?PARM(129, fx_2_param_1, 0, 255),
       ?PARM(130, fx_2_param_2, 0, 127),
       ?PARM(131, fx_2_sync, 0, 1, ?ONOFF),

       ?PARM(135, fx_on_off, 0, 1, ?ONOFF),

       ?PARM(143, polymod_filter_env_amt, 0, 254),
       ?PARM(144, polymod_osc_2_amt, 0, 254),
       ?PARM(145, polymod_freq_1_dest_on_off, 0,1,?ONOFF),
       ?PARM(146, polymod_shape_1_dest_on_off, 0,1,?ONOFF),
       ?PARM(147, polymod_pw_1_dest_on_off, 0,1,?ONOFF),
       ?PARM(148, polymod_low_pass_dest_on_off, 0,1,?ONOFF),
       ?PARM(149, polymod_high_pass_dest_on_off,0,1,?ONOFF),
       ?PARM(156, unison_on_off,0,1,?ONOFF),
       ?PARM(157, unison_mode, 0,6),
       ?PARM(158, key_mode, 0, 5),
       ?PARM(160, arp_on_off, 0,1,?ONOFF),
       ?PARM(161, arp_mode, 0, 4),
       ?PARM(162, arp_range, 0, 2),
       ?PARM(163, arp_tim_sig, 0, 9),
       ?PARM(167, bpm, 30, 250),
       ?PARM(168, seq_on_off, 0, 1, ?ONOFF),
       ?PARM(169, seq_length, 0, 63),
       ?PARM(170, seq_mode, 0, 1),
       ?PARM(171, seq_play_mode, 0, 1),
       ?PARM(236, name, 32, 125), %% note! ascii char in 236-255 (name_last)
       ?PARM(255, name_last, 32, 125),
       ?PARM(256, seq_note1, 12, 108),  %% 64 params
       ?PARM(320, seq_vel1,  0, 127),   %% 64 params
       ?PARM(384, seq_note2, 12, 108),
       ?PARM(448, seq_vel2,  0, 127),   %% 64 params
       ?PARM(512, seq_note3, 12, 108),
       ?PARM(576, seq_vel3,  0, 127),   %% 64 params
       ?PARM(640, seq_note4, 12, 108),
       ?PARM(704, seq_vel4,  0, 127),   %% 64 params
       ?PARM(768, seq_note5, 12, 108),
       ?PARM(832, seq_vel5,  0, 127),   %% 64 params
       ?PARM(896, seq_note6, 12, 108),
       ?PARM(960, seq_vel6,  0, 127),   %% 960-1023

       %% GLOBAL PARAMETER DATA
       ?PARM(1024, master_fine_tune, 0, 100),
       ?PARM(1025, master_coarse_tune, 0, 24),
       ?PARM(1026, midi_channel, 0, 16, #{all=>0}),
       ?PARM(1027, midi_clock_mode, 0, 3, 
	      #{off=>0, master=>1, slave=>2, slave_thru=>3}),
       ?PARM(1028, midi_clock_port, 0, 1, 
	      #{midi=>0, usb=>1}),
       ?PARM(1029, midi_param_send, 0, 2,
	      #{nrp=>0,cc=>1,off=>2}),
       ?PARM(1030, midi_param_receive, 0, 2,
	      #{nrp=>0,cc=>1,off=>2}),
       ?PARM(1031, midi_control_enable, 0, 1, ?ONOFF),
       ?PARM(1032, midi_sysex_enable, 0, 1, ?ONOFF),
       ?PARM(1033, midi_out_select, 0, 3,
	      #{off=>0, midi=>1, usb=>2, midi_usb=>3}),
       ?PARM(1035, midi_local_control, 0, 1, ?ONOFF),
       ?PARM(1037, pot_mode, 0, 2,
	      #{relative=>0, passthru=>1, jump=>2}),
       ?PARM(1039, seq_jack, 0, 3,
	      #{normal=>0, trigger=>1, gate=>2, trigger_gate=>3}),
       ?PARM(1040, sustain_polarity, 0, 3,
	      #{normally_open=>0, normally_closed=>1, sustain_normally_open=>2, sustain_normally_closed=>3}),

       ?PARM(1041, velocity_reponse, 0, 3),
       ?PARM(1042, aftertouch_response, 0, 3),
       ?PARM(1043, mono_stereo, 0, 1, #{stereo=>0, mono=>1}),
       ?PARM(1044, alt_tuning, 0, 16)
     }.

%% dump id's found usomg reverse engineering, 
%% using get_edit_buffer and diff_edit_buffer
dump() ->
    #{
      ?PARM(0,  osc_1_freq, 0,60),
      ?PARM(1,  osc_2_freq,  0,60),
      ?PARM(2,  osc_2_freq_fine, 0,254),
      ?PARM(3,  osc_1_shape, 0,254),
      ?PARM(4,  osc_2_shape, 0,254),
      ?PARM(5,  osc_1_pulse_width, 0,255),
      ?PARM(6,  osc_2_pulse_width, 0,255),
      ?PARM(7,  osc_1_level, 0,127),
      ?PARM(8,  osc_2_level, 0,127),
      ?PARM(9,  osc_1_sub_level, 0,127), %% Osc 1 Sub Octave
      ?PARM(10, noise_level, 0,127),
      
      ?PARM(11, osc_1_sync, 0,1),
      ?PARM(12, osc_2_key_on_off, 0,1),
      ?PARM(13, osc_2_low_freq, 0,1),
      ?PARM(14, glide_rate, 0,127),
      ?PARM(15, glide_mode, 0,3),     %%% glide + (dec/inc)
      ?PARM(16, glide_on_off, 0,1),
      ?PARM(17, pbend_range, 0, 24),

      ?PARM(18, slope_amt, 0,127),
      ?PARM(19, low_pass_freq, 0, 164),
      ?PARM(20, low_pass_res, 0, 255),
      ?PARM(21, low_pass_key_amt, 0, 2),
      ?PARM(22, low_pass_vel_on, 0, 1),
      ?PARM(23, high_pass_freq, 0,164),
      ?PARM(24, high_pass_res, 0, 255),
      ?PARM(25, high_pass_key_amt, 0, 2),
      ?PARM(26, high_pass_vel_on, 0, 1),
      ?PARM(27, voice_volume, 0, 127),  %% prgm vol
      ?PARM(28, pan_spread, 0, 127),
      ?PARM(29, low_pass_filter_env_amt, 0, 254),
      ?PARM(30, high_pass_filter_env_amt, 0, 254),
      ?PARM(31, vca_env_amt, 0, 127),

      ?PARM(35, filter_env_attack, 0, 127),
      ?PARM(36, vca_env_attack, 0, 127),
      ?PARM(37, filter_env_decay,   0, 127),
      ?PARM(38, vca_env_decay, 0, 127),

      ?PARM(39, filter_env_sustain, 0, 127),
      ?PARM(40, vca_env_sustain, 0, 127),
      ?PARM(41, filter_env_release, 0, 127),
      ?PARM(42, vca_env_release, 0, 127),
      ?PARM(43, vca_env_vel_on, 0, 1),
      ?PARM(44, fx_1_select, 0, 9),
      ?PARM(45, fx_2_select, 0, 13),
      ?PARM(46, fx_on_off, 0, 1),
      ?PARM(48, fx_1_mix, 0, 127),
      ?PARM(49, fx_2_mix, 0, 127),
      ?PARM(50, fx_1_param_1, 0, 255),
      ?PARM(51, fx_2_param_1, 0, 255),
      ?PARM(52, fx_1_param_2, 0, 127),
      ?PARM(53, fx_2_param_2, 0, 127),
      ?PARM(55, fx_1_sync, 0, 1),
      %% 56
      %% 57?
      ?PARM(58, distortion_amt, 0, 127),
      %% LFO
      ?PARM(59, lfo_freq, 0, 254),
      ?PARM(62, lfo_shape, 0, 4),
      ?PARM(63, lfo_initial_amt, 0, 255),
      ?PARM(64, lfo_freq_1_dest_on_off, 0, 1),
      ?PARM(65, lfo_freq_2_dest_on_off, 0, 1),
      ?PARM(66, lfo_pw_1_2_dest_on_off, 0, 1),
      ?PARM(67, lfo_low_pass_dest_on_off, 0, 1, ?ONOFF),
      ?PARM(68, lfo_high_pass_dest_on_off, 0, 1, ?ONOFF),      
      ?PARM(69, lfo_amp_dest_on_off, 0, 1),
      
      %% Aftertouch
      ?PARM(70, pressure_amt, 0, 254),
      ?PARM(71, pressure_freq_1_dest_on_off, 0, 1, ?ONOFF),
      ?PARM(72, pressure_freq_2_dest_on_off, 0, 1, ?ONOFF),
      ?PARM(73, pressure_low_pass_dest_on_off, 0, 1, ?ONOFF),
      ?PARM(74, pressure_high_pass_dest_on_off, 0, 1, ?ONOFF),
      ?PARM(75, pressure_vca_dest_on_off, 0, 1, ?ONOFF),
      ?PARM(76, pressure_lfo_amt_dest_on_off, 0, 1, ?ONOFF),      

      ?PARM(77, polymod_filter_env_amt, 0, 254),
      ?PARM(78, polymod_osc_2_amt, 0, 254),      
      ?PARM(79, polymod_freq_1_dest_on_off, 0,1,?ONOFF),
      ?PARM(80, polymod_shape_1_dest_on_off, 0,1,?ONOFF),
      ?PARM(81, polymod_pw_1_dest_on_off, 0,1,?ONOFF),
      ?PARM(82, polymod_low_pass_dest_on_off, 0,1,?ONOFF),
      ?PARM(83, polymod_high_pass_dest_on_off,0,1,?ONOFF),
      ?PARM(84, unison_on_off,0,1,?ONOFF),
      ?PARM(85, unison_mode, 0,6),  %% push unison + (dec/inc)
      ?PARM(86, key_mode, 0, 5),

      ?PARM(87, bpm, 30, 250),
      ?PARM(89, arp_mode, 0, 4),
      ?PARM(90, arp_range, 0, 2),
      ?PARM(91, arp_on_off, 0,1,?ONOFF),
      ?PARM(92, arp_tim_sig, 0, 9), %%% ???

      ?PARM(95, seq_mode, 0, 1),
      ?PARM(96, seq_play_mode, 0, 1)
      
     }.

%% verify a nrmn parameter againt the dump,
verify_nrpn_dump(S, Bin0, Param, Value) ->
    {ok,_} = transmit_param(S, Param, Value),
    case diff_edit_buffer(S, Bin0) of
	[] -> %% set same value as in dump or non existing parameter
	    not_found_in_dump;
	[{DumpNo, OldValue, Value}] ->
	    io:format("dump index ~p\n", [DumpNo]),
	    io:format("new value = ~p\n", [Value]),
	    io:format("old value = ~p\n", [OldValue]),
	    DumpValue = maps:get(DumpNo, dump(), undefined),
	    {ok,_} = transmit_param(S, Param, OldValue),  %% restore
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

transmit_param(Synth, Param, Value) ->
    transmit_param(Synth, 0, Param, Value).

transmit_param(Synth, Chan, ParamName, Value) when is_atom(ParamName) ->
    Param = maps:get(ParamName, nrpn()),
    {_, Range} = maps:get(Param, nrpn()),
    Value1 = check_range(Range, Value),
    midi:nrpn(Synth, Chan, Param, Value1);
transmit_param(Synth, Chan, Param, Value) when
      is_integer(Param), Param >= 0, Param =< 16#3fff ->
    {_ParamName, Range} = maps:get(Param, nrpn()),
    Value1 = check_range(Range, Value),
    midi:nrpn(Synth, Chan, Param, Value1).

transmit_name(Synth, Chan, Name) when is_list(Name) ->
    Param = name_to_nrpn(name),
    transmit_name_(Synth, Chan, Param, Name).

transmit_name_(Synth, Chan, Param, [C|Cs]) ->
    transmit_param(Synth, Chan, Param, C),
    transmit_name_(Synth, Chan, Param+1, Cs);
transmit_name_(_Synth, _Chan, _Param, []) ->
    ok.

-type seqnote() :: #{ voice => 1..6, note => 12..108, vel => 0..127 }.
-type seqdata() :: [seqnote()].

-spec transmit_seq(Synth::reference(), Chan::0..16, Seq::[seqdata()]) ->
	  ok.

transmit_seq(Synth, Chan, Seq) ->
    transmit_seq_(Synth, Chan, 0, Seq).

transmit_seq_(Synth, Chan, I, [Data|Seq]) ->
    transmit_seq_data(Synth, Chan, I, Data),
    transmit_seq_(Synth, Chan, I+1, Seq);
transmit_seq_(_Synth, _Chan, _I, []) ->
    ok.

transmit_seq_data(Synth, Chan, I, [D|Data]) ->
    J = maps:get(voice, D),
    case maps:get(note, D, undefined) of
	undefined -> ok;
	Note ->
	    Note0 = seq_note(J),
	    transmit_param(Synth, Chan, Note0+I, Note)		
    end,
    case maps:get(vel, D, undefined) of
	undefined -> ok;
	Vel -> 
	    Vel0 = seq_vel(J),
	    transmit_param(Synth, Chan, Vel0+I, Vel)
    end,
    transmit_seq_data(Synth, Chan, I+1, Data);
transmit_seq_data(_Synth, _Chan, _I, []) ->
    ok.

%% get note and vel parameter base index from note number
seq_note(I) when I>=1, I=<6  -> 256+(I-1)*128.
seq_vel(I) when I>=1, I=<6 -> 320+(I-1)*128.


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
		       _ -> {unknown, Family}
		   end,
	    {ok, Info#{model_name=>Name}};
	Error ->
	    Error
	end.

program_dump(Synth, Bank, Program) ->
    request_program_dump(Synth, Bank, Program),
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?PROPHET_6_ID:7,
	       0:1, 16#02:7, %% Program DATA
	       0:1, Bank:7,
	       0:1, Program:7,
	       Bin/binary>>} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    Bin1 = unpack_ms_bit(Bin),
	    Data = decode_program(Bin1, dump()),
	    {ok,#{bank=>Bank,program=>Program,data=>Data}};
	{ok, Bin} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    {error, {unexpeced, Bin}};
	Error ->
	    Error
    end.

get_edit_buffer(Synth) ->
    request_program_program_edit_buffer(Synth),    
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?PROPHET_6_ID:7,
	       0:1, 16#03:7, %% Edit Buffer Data
	       Bin/binary>>} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    {ok, unpack_ms_bit(Bin)};
	{ok, Bin} ->
	    {erro, {unexpeced, Bin}};
	Error ->
	    Error
	end.

decode_edit_buffer(Synth) ->
    {ok, Bin} = get_edit_buffer(Synth),
    decode_program(Bin, dump()).

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
    request_program_program_edit_buffer(Synth),
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?PROPHET_6_ID:7,
	       0:1, 16#03:7, %% Edit Buffer Data
	       Bin/binary>>} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    Bin1 = unpack_ms_bit(Bin),
	    Data = decode_program(Bin1, dump()),
	    {ok,#{data=>Data}};
	{ok, Bin} ->
	    %% io:format("|Bin|=~w, Bin=~p\n", [byte_size(Bin), Bin]),
	    {error, {unexpeced, Bin}};
	Error ->
	    Error
	end.

request_program_dump(Synth, Bank, Program) when
      is_integer(Bank), Bank >= 0, Bank =< 9,
      is_integer(Program), Program >= 0, Program =< 99 ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?PROPHET_6_ID:7,
		 0:1, ?SYS_REQUEST_PROGRAM_DUMP:7,
		 0:1, Bank:7, %% Bank 0-9 ? 0-4?
		 0:1, Program:7, %%  Program 0-99
		 ?MIDI_EVENT_SYS:4, 7:4>>).

request_program_program_edit_buffer(Synth) ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?PROPHET_6_ID:7,
		 0:1, ?SYS_REQUEST_PROGRAM_EDIT_BUFFER:7,
		 ?MIDI_EVENT_SYS:4, 7:4>>).

global_parameter_dump(Synth) ->
    request_global_parameter_dump(Synth),    
    case midi:read_sys(Synth) of
	{ok, <<0:1, ?DSI_ID_HEADER:7,
	       0:1, ?PROPHET_6_ID:7,
	       0:1, 15:7,
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

request_global_parameter_dump(Synth) ->
    midi:write(Synth,
	       <<?MIDI_EVENT_SYS:4, 0:4,
		 0:1, ?DSI_ID_HEADER:7,
		 0:1, ?PROPHET_6_ID:7,
		 0:1, ?SYS_REQUEST_GLOBAL_PARAMETER_DUMP:7,
		 ?MIDI_EVENT_SYS:4, 7:4>>).


%% byte_size(Bin) rem 7 = 0
pack_ms_bit(<<A7:1, A:7, B7:1, B:7, C7:1, C:7, D7:1, D:7,
	      E7:1, E:7, F7:1, F:7, G7:1, G:7, Data/binary>>) ->
    Data1 = pack_ms_bit(Data),
    <<0:1,G7:1,F7:1,E7:1,D7:1,C7:1,B7:1,A7:1,
      A,B,C,D,E,F,G,Data1/binary>>;
pack_ms_bit(<<>>) ->
    <<>>.

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

%% unpack global parameters
unpack_global_params(Data) ->
    unpack_global_params(1024, Data, []).

unpack_global_params(Param, <<Value,Data/binary>>, Acc) ->
    try nrpn_to_name(Param) of
	{ParamName,_Range} -> %% fixme: use range? warnings?
	    io:format("~s: ~w\n", [ParamName, Value]),
	    unpack_global_params(Param+1, Data, [{ParamName, Value}|Acc])
    catch
	error:_ ->
	    unpack_global_params(Param+1, Data, [{Param, Value}|Acc])
    end;
unpack_global_params(_, <<>>, Acc) ->
    lists:reverse(Acc).    

decode_program(Data, Map) ->
    decode_program(0, Data, Map, []).

decode_program(Param, <<C,Data/binary>>, Map, Acc) ->
    try maps:get(Param, Map) of
	{name, _Range} -> %% sequence of data (until last)
	    decode_program_seq(Param+1,Param+20-1,name,Data,[C],Map,Acc);
	{seq_note1, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_note1,Data,[C],Map,Acc);
	{seq_vel1, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_vel1,Data,[C],Map,Acc);
	{seq_note2, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_note2,Data,[C],Map,Acc);
	{seq_vel2, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_vel2,Data,[C],Map,Acc);
	{seq_note3, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_note3,Data,[C],Map,Acc);
	{seq_vel3, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_vel3,Data,[C],Map,Acc);
	{seq_note4, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_node4,Data,[C],Map,Acc);
	{seq_vel4, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_vel4,Data,[C],Map,Acc);
	{seq_note5, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_note5,Data,[C],Map,Acc);
	{seq_vel5, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_val5,Data,[C],Map,Acc);
	{seq_note6, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_note6,Data,[C],Map,Acc);
	{seq_vel6, _Range} ->
	    decode_program_seq(Param+1,Param+64-1,seq_vel6,Data,[C],Map,Acc);
	{ParamName,_Range} ->
	    io:format("~s: ~w\n", [ParamName, C]),
	    decode_program(Param+1, Data, Map, [{ParamName,C}|Acc])
    catch
	error:_ ->
	    decode_program(Param+1, Data, Map, [{Param,C}|Acc])
    end;
decode_program(_, <<>>, _Map, Acc) ->
    lists:reverse(Acc).

%% collect bytes for param sequnces
decode_program_seq(Param, Param, ParamName, <<C,Data/binary>>, Cs, Map, Acc) ->
    List = lists:reverse([C|Cs]),
    io:format("~s: ~p\n", [ParamName, List]),
    decode_program(Param+1, Data, Map, [{ParamName,List}|Acc]);
decode_program_seq(Param,ParamLast,ParamName,<<C,Data/binary>>,Cs,Map, Acc)
  when Param < ParamLast ->
    decode_program_seq(Param+1,ParamLast,ParamName,Data,[C|Cs],Map, Acc).


name_to_nrpn(ParamName) when is_atom(ParamName) ->
    maps:get(ParamName, nrpn());
name_to_nrpn(Param) when is_integer(Param), Param >= 0, Param =< 16#3fff ->
    Param.

nrpn_to_name(Param) when is_integer(Param), Param >= 0, Param =< 16#3fff ->
    maps:get(Param, nrpn());
nrpn_to_name(Param) when is_atom(Param) ->
    Param.

check_range({Min,Max,_Enum}, Value) when is_integer(Value), 
					 Value >= Min, Value =< Max ->
    Value;
check_range({Min,Max,Enum}, Value) when is_atom(Value) ->
    Value1 = maps:get(Value, Enum),
    if Value1 >= Min, Value1 =< Max -> Value1; 
       true -> error(out_of_range)
    end.

nrpn_names() ->
    lists:foldl(fun({Name,{_Min,_Max,_Enum}}, Acc) ->
			[Name|Acc];
		   (_, Acc) -> Acc
		end, [], maps:values(nrpn())).

nrpn_list() ->
    lists:foldl(fun({Name,{Min,Max,Enum}}, Acc) ->
			[{Name,Min,Max,Enum}|Acc];
		   (_, Acc) -> Acc
		end, [], maps:values(nrpn())).
