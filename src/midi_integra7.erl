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

-export([dt/4]).
-export([checksum/1]).

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

-define(MIDI_SYSEX_ID_ROLAND, 16#41).
-define(ROLAND_INTEGRA_7,  16#00,16#00,16#64).
-define(COMMAND_DT1, 16#12).
%% base addresses
-define(Setup,  16#01000000).
-define(System, 16#02000000).
-define(Temporary_Studio_Set, 16#18000000).
%% Tone N = 1..16
-define(Temporary_Tone(N), 
	(((((N-1) div 4)+16#19) bsl 24)+(((N-1)*2 rem 8) bsl 20))).

%% System
-define(System_Common, ?System+16#00000000).

%% Temporary Tone 
-define(Temporary_PCM_Synth_Tone(T),             ((T)+16#000000)).
-define(Temporary_SuperNATURAL_Synth_Tone(T),    ((T)+16#010000)).
-define(Temporary_SuperNATURAL_Acoustic_Tone(T), ((T)+16#020000)).
-define(Temporary_SuperNATURAL_Drum_Kit(T),      ((T)+16#030000)).
-define(Temporary_PCM_Drum_Kit(T),               ((T)+16#100000)).

%% Studio Set
-define(Studio_Set_Common, (?Temporary_Studio_Set+16#000000)).
-define(Studio_Set_Common_Chorus, (?Temporary_Studio_Set+16#000400)).
-define(Studio_Set_Common_Reverb, (?Temporary_Studio_Set+16#000600).
-define(Studio_Set_Common_Motional_Surround, (?Temporary_Studio_Set+16#000800).
-define(Studio_Set_Master_EQ, (?Temporary_Studio_Set+16#000900)).
-define(Studio_Set_MIDI(Channel),  %% channel 1..16
	(?Temporary_Studio_Set+16#001000+(((Channel)-1) bsl 8))).
-define(Studio_Set_Part(Part),  %% Part 1..16
	(?Temporary_Studio_Set+16#002000+(((Part)-1) bsl 8))).
-define(Studio_Set_Part_EQ(Part),  %% Part 1..16
	(?Temporary_Studio_Set+16#005000+(((Part)-1) bsl 8))).


dt(Handle, Dev, Address, Data) ->
    %% <<A3,A2,A1,A0>> = <<Address:32>>,
    Packet = <<Address:32,Data/binary>>,
    Checksum = checksum(Packet),
    midi:sysex(Handle, 
	       <<?MIDI_SYSEX_ID_ROLAND, Dev,
		 ?ROLAND_INTEGRA_7, ?COMMAND_DT1,
		 Packet/binary, Checksum>>).

%% checksum for integra7
checksum(Data) ->
   16#80 - (data_sum(Data) band 16#7F).

data_sum([]) -> 0;
data_sum([H|T]) -> H + data_sum(T);
data_sum(Bin) when is_binary(Bin) -> data_sum_bin(Bin).

data_sum_bin(<<A,B,C,D,Tail/binary>>) -> A + B + C + D + data_sum_bin(Tail);
data_sum_bin(<<A,B,C>>) -> A + B + C;
data_sum_bin(<<A,B>>) -> A + B;
data_sum_bin(<<A>>) -> A;
data_sum_bin(<<>>) -> 0.
