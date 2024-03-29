
-ifndef(_MIDI_H_).
-define(_MIDI_H_, true).

%% midi mapping
-define(C, 60).  %% C4
-define(D, 62).
-define(E, 64).
-define(F, 65).
-define(G, 67).
-define(A, 69).
-define(B, 71).
-define(c, 72).
-define(d, 74).
-define(e, 76).
-define(f, 77).
-define(g, 79).
-define(a, 81).
-define(b, 83).

-record(note_on,
	{
	 chan :: 0..15,
	 note :: 0..127,
	 velocity :: 0..127  %% 0 = off!
	}).

-record(note_off,
	{
	 chan :: 0..15,
	 note :: 0..127,
	 velocity :: 0..127
	}).

-record(after_touch,
	{
	 chan :: 0..15,
	 note :: 0..127,
	 value :: 0..127
	}).

-record(control_change,
	{
	 chan :: 0..15,
	 control :: 0..127,
	 param :: 0..127
	}).

-record(pitch_bend,
	{
	 chan :: 0..15,
	 bend :: -8192 .. 8191 %% signed 14-bit
	}).

-record(program_change,
	{
	 chan :: 0..15,
	 prog :: 0..127
	}).

-record(pressure,
	{
	 chan :: 0..15,
	 pressure :: 0..127
	}).

-record(meta,
	{
	 type :: atom(),
	 params :: [byte()] | binary() | term()
	}).

-record(sys,
	{
	 type   :: atom(),
	 params :: [byte()] | binary()
	}).

-define(USEC_PER_MINUTE, 60000000).
-define(DEFAULT_MPQN,    500000).  %% 120 BPM

-record(tparam,
	{
	  ppqn = 0.0 :: float(),  %% pulses per quarter note
	  mpqn = 0   :: number(), %% micro seconds per quarter note
	  bpm  = 0.0 :: float(),  %% beats per minute (USEC_PER_MINUTE/MPQN)
	  uspp = 1.0 :: float(),  %% Micro seconds per pulse (MPQN/PPQN)
	  sig  = {4,4} :: {integer(),integer()}, %% time signature
	  cc   = 0     :: integer(), %% # MIDI clocks in a metronome click
	  bb   = 0     :: integer()  %% # 32nd-notes in a MIDI quarter-note
	}).


%% MIDI message types
-define(MIDI_EVENT_NOTEOFF, 	  16#8).  %% channel 0-15 in lower nibble
-define(MIDI_EVENT_NOTEON, 	  16#9).  %% channel 0-15 in lower nibble
-define(MIDI_EVENT_AFTERTOUCH,    16#A).
-define(MIDI_EVENT_CONTROLCHANGE, 16#B).  %% channel 0-15 in lower nibble
-define(MIDI_EVENT_PROGRAMCHANGE, 16#C).  %% channel 0-15 in lower nibble
-define(MIDI_EVENT_PRESSURE,      16#D).
-define(MIDI_EVENT_PITCHBEND,     16#E).
-define(MIDI_EVENT_SYS,           16#F).

-define(MIDI_META_TEXT,           16#01).  %% text
-define(MIDI_META_COPYRIGHT,      16#02).  %% text
-define(MIDI_META_TRACKNAME,      16#03).  %% text
-define(MIDI_META_INSTRUMENT,     16#04).  %% text
-define(MIDI_META_LYRIC,          16#05).  %% text
-define(MIDI_META_MARKER,         16#06).  %% text
-define(MIDI_META_CUE_POINT,      16#07).  %% text
-define(MIDI_META_PROGRAM_NAME,   16#08).  %% text
-define(MIDI_META_DEVICE_NAME,    16#09).  %% text
-define(MIDI_META_MIDI_CHANNEL,   16#20).  %% uint8
-define(MIDI_META_MIDI_PORT,      16#21).  %% uint8
-define(MIDI_META_END_OF_TRACK,   16#2f).  %% -
-define(MIDI_META_TEMPO,          16#51).  %% uint24/big
-define(MIDI_META_SMPTE_OFFSET,   16#54).  %% <<HR:8,MN:8,SE:8,FR:8,FF:8>>
-define(MIDI_META_TIME_SIGNATURE, 16#58).  %% <<NN:8,DD:8,CC:8,BB:8>>
-define(MIDI_META_KEY_SIGNATURE,  16#59).  %% <<SF:8,MI:8>>
-define(MIDI_META_PROPRIETARY,    16#7f).   %% text

-define(MIDI_CTRL_BANK_SELECT,       0).
-define(MIDI_CTRL_MODULATION_WHEEL,  1).
-define(MIDI_CTRL_BREATH_CONTROLLER, 2).
-define(MIDI_CTRL_FOOT_PEDAL,        4).
-define(MIDI_CTRL_PORTAMENTO_TIME,   5).
-define(MIDI_CTRL_DATA_ENTRY,        6).
-define(MIDI_CTRL_VOLUME,            7).
-define(MIDI_CTRL_BALANCE,           8).
-define(MIDI_CTRL_PAN_POSITION,      10).   %% MSB
-define(MIDI_CTRL_EXPRESSION,        11).
-define(MIDI_CTRL_EFFECT_CONTROL_1,  12).
-define(MIDI_CTRL_EFFECT_CONTROL_2,  13).
-define(MIDI_CTRL_GENERAL_PURPOSE_SLIDER_1, 16).
-define(MIDI_CTRL_GENERAL_PURPOSE_SLIDER_2, 17).
-define(MIDI_CTRL_GENERAL_PURPOSE_SLIDER_3, 18).
-define(MIDI_CTRL_GENERAL_PURPOSE_SLIDER_4, 19).
-define(MIDI_CTRL_BANK_SELECT_FINE,         (32+0)).
-define(MIDI_CTRL_MODULATION_WHEEL_FINE,    (32+1)).
-define(MIDI_CTRL_BREATH_CONTROLLER_FINE,   (32+2)).
-define(MIDI_CTRL_FOOT_PEDAL_FINE,          (32+4)).
-define(MIDI_CTRL_PORTAMENTO_TIME_FINE,     (32+5)).
-define(MIDI_CTRL_DATA_ENTRY_FINE,          (32+6)).
-define(MIDI_CTRL_VOLUME_FINE,              (32+7)).
-define(MIDI_CTRL_BALANCE_FINE,             (32+8) ).
-define(MIDI_CTRL_PAN_POSITION_FINE,        (32+10)).  %% LSB
-define(MIDI_CTRL_EXPRESSION_FINE,          (32+11)).
-define(MIDI_CTRL_EFFECT_CONTROL_1_FINE,    (32+12)).
-define(MIDI_CTRL_EFFECT_CONTROL_2_FINE,    (32+13)).
-define(MIDI_CTRL_HOLD_PEDAL,               64).
-define(MIDI_CTRL_PORTAMENTO,               65).
-define(MIDI_CTRL_SUSTENUTO_PEDAL,          66).
-define(MIDI_CTRL_SOFT_PEDAL,               67).
-define(MIDI_CTRL_LEGATO_PEDAL,             68).
-define(MIDI_CTRL_HOLD_2_PEDAL,             69).
-define(MIDI_CTRL_SOUND_VARIATION,          70).
-define(MIDI_CTRL_SOUND_TIMBRE,             71).
-define(MIDI_CTRL_SOUND_RELEASE_TIME,       72).
-define(MIDI_CTRL_SOUND_ATTACK_TIME,        73).
-define(MIDI_CTRL_SOUND_BRIGHTNESS,         74).
-define(MIDI_CTRL_SOUND_CONTROL_6,          75).
-define(MIDI_CTRL_SOUND_CONTROL_7,          76).
-define(MIDI_CTRL_SOUND_CONTROL_8,          77).
-define(MIDI_CTRL_SOUND_CONTROL_9,          78).
-define(MIDI_CTRL_SOUND_CONTROL_10,         79).
-define(MIDI_CTRL_GENERAL_PURPOSE_BUTTON_1, 80).
-define(MIDI_CTRL_GENERAL_PURPOSE_BUTTON_2, 81).
-define(MIDI_CTRL_GENERAL_PURPOSE_BUTTON_3, 82).
-define(MIDI_CTRL_GENERAL_PURPOSE_BUTTON_4, 83).
-define(MIDI_CTRL_EFFECTS_LEVEL,            91).
-define(MIDI_CTRL_TREMULO_LEVEL,            92).
-define(MIDI_CTRL_CHORUS_LEVEL,             93).
-define(MIDI_CTRL_CELESTE_LEVEL,            94).
-define(MIDI_CTRL_PHASER_LEVEL,             95).
-define(MIDI_CTRL_DATA_BUTTON_INCREMENT,    96).
-define(MIDI_CTRL_DATA_BUTTON_DECREMENT,    97).
-define(MIDI_CTRL_NON_REGISTERED_PARAMETER_FINE, 98). %% LSB
-define(MIDI_CTRL_NON_REGISTERED_PARAMETER,  99).     %% MSB
-define(MIDI_CTRL_REGISTERED_PARAMETER_FINE, 100).    %% LSB
-define(MIDI_CTRL_REGISTERED_PARAMETER,     101).     %% MSB
-define(MIDI_CTRL_ALL_SOUND_OFF,            120).
-define(MIDI_CTRL_ALL_CONTROLLERS_OFF,      121).
-define(MIDI_CTRL_LOCAL_KEYBOARD,           122).
-define(MIDI_CTRL_ALL_NOTES_OFF,            123).
-define(MIDI_CTRL_OMNI_MODE_OFF,            124).
-define(MIDI_CTRL_OMNI_MODE_ON,             125).
-define(MIDI_CTRL_MONO_OPERATION,           126).
-define(MIDI_CTRL_POLY_OPERATION,           127).

%% General midi instruments Piano:
-define(GM_MIDI_Acoustic_Grand_Piano,  0).
-define(GM_MIDI_Bright_Acoustic_Piano, 1).
-define(GM_MIDI_Electric_Grand_Piano,  2 ).
-define(GM_MIDI_Honky_tonk_Piano,      3 ).
-define(GM_MIDI_Electric_Piano_1, 4).
-define(GM_MIDI_Electric_Piano_2, 5).
-define(GM_MIDI_Harpsichord, 6).
-define(GM_MIDI_Clavinet, 7).
%% Chromatic Percussion:
-define(GM_MIDI_Celesta, 8).
-define(GM_MIDI_Glockenspiel, 9).
-define(GM_MIDI_Music_Box, 10).
-define(GM_MIDI_Vibraphone, 11).
-define(GM_MIDI_Marimba, 12).
-define(GM_MIDI_Xylophone, 13).
-define(GM_MIDI_Tubular_Bells, 14).
-define(GM_MIDI_Dulcimer, 15).
%% Organ:
-define(GM_MIDI_Drawbar_Organ, 16).
-define(GM_MIDI_Percussive_Organ, 17).
-define(GM_MIDI_Rock_Organ, 18).
-define(GM_MIDI_Church_Organ, 19).
-define(GM_MIDI_Reed_Organ, 20).
-define(GM_MIDI_Accordion, 21).
-define(GM_MIDI_Harmonica, 22 ).
-define(GM_MIDI_Tango_Accordion, 23).
%% Guitar:
-define(GM_MIDI_Acoustic_Guitar_nylon, 24).
-define(GM_MIDI_Acoustic_Guitar_steel, 25 ).
-define(GM_MIDI_Electric_Guitar_jazz, 26).
-define(GM_MIDI_Electric_Guitar_clean, 27).
-define(GM_MIDI_Electric_Guitar_muted, 28 ).
-define(GM_MIDI_Overdriven_Guitar, 29).
-define(GM_MIDI_Distortion_Guitar, 30 ).
-define(GM_MIDI_Guitar_harmonics, 31).
%% Bass
-define(GM_MIDI_Acoustic_Bass, 32).
-define(GM_MIDI_Electric_Bass_finger, 33).
-define(GM_MIDI_Electric_Bass_pick, 34).
-define(GM_MIDI_Fretless_Bass, 35).
-define(GM_MIDI_Slap_Bass_1, 36).
-define(GM_MIDI_Slap_Bass_2, 37 ).
-define(GM_MIDI_Synth_Bass_1, 38).
-define(GM_MIDI_Synth_Bass_2, 39).
%% Strings:
-define(GM_MIDI_Violin, 40).
-define(GM_MIDI_Viola, 41).
-define(GM_MIDI_Cello, 42 ).
-define(GM_MIDI_Contrabass, 43).
-define(GM_MIDI_Tremolo_Strings, 44).
-define(GM_MIDI_Pizzicato_Strings, 45).
-define(GM_MIDI_Orchestral_Harp, 46).
-define(GM_MIDI_Timpani, 47).
%% Strings (continued):
-define(GM_MIDI_String_Ensemble_1, 48).
-define(GM_MIDI_String_Ensemble_2, 49 ).
-define(GM_MIDI_Synth_Strings_1, 50).
-define(GM_MIDI_Synth_Strings_2, 51 ).
-define(GM_MIDI_Choir_Aahs, 52).
-define(GM_MIDI_Voice_Oohs, 53 ).
-define(GM_MIDI_Synth_Voice, 54).
-define(GM_MIDI_Orchestra_Hit, 55).
%% Brass:
-define(GM_MIDI_Trumpet, 56).
-define(GM_MIDI_Trombone, 57).
-define(GM_MIDI_Tuba, 58).
-define(GM_MIDI_Muted_Trumpet, 59).
-define(GM_MIDI_French_Horn, 60).
-define(GM_MIDI_Brass_Section, 61).
-define(GM_MIDI_Synth_Brass_1, 62 ).
-define(GM_MIDI_Synth_Brass_2, 63 ).
%% Reed:
-define(GM_MIDI_Soprano_Sax, 64).
-define(GM_MIDI_Alto_Sax, 65).
-define(GM_MIDI_Tenor_Sax, 66).
-define(GM_MIDI_Baritone_Sax, 67).
-define(GM_MIDI_Oboe, 68).
-define(GM_MIDI_English_Horn, 69).
-define(GM_MIDI_Bassoon, 70).
-define(GM_MIDI_Clarinet, 71 ).
%% Pipe:
-define(GM_MIDI_Piccolo, 72).
-define(GM_MIDI_Flute, 73).
-define(GM_MIDI_Recorder, 74).
-define(GM_MIDI_Pan_Flute, 75).
-define(GM_MIDI_Blown_Bottle, 76).
-define(GM_MIDI_Shakuhachi, 77).
-define(GM_MIDI_Whistle, 78).
-define(GM_MIDI_Ocarina, 79 ).
%% Synth Lead:
-define(GM_MIDI_Lead_1_square, 80).
-define(GM_MIDI_Lead_2_sawtooth, 81).
-define(GM_MIDI_Lead_3_calliope, 82 ).
-define(GM_MIDI_Lead_4_chiff, 83).
-define(GM_MIDI_Lead_5_charang, 84).
-define(GM_MIDI_Lead_6_voice, 85).
-define(GM_MIDI_Lead_7_fifths, 86).
-define(GM_MIDI_Lead_8_bass_plus_lead, 87).
%% Synth Pad:
-define(GM_MIDI_Pad_1_new_age, 88).
-define(GM_MIDI_Pad_2_warm, 89).
-define(GM_MIDI_Pad_3_polysynth, 90).
-define(GM_MIDI_Pad_4_choir, 91).
-define(GM_MIDI_Pad_5_bowed, 92 ).
-define(GM_MIDI_Pad_6_metallic, 93).
-define(GM_MIDI_Pad_7_halo, 94).
-define(GM_MIDI_Pad_8_sweep, 95).
%% Synth Effects:
-define(GM_MIDI_FX_1_rain, 96).
-define(GM_MIDI_FX_2_soundtrack, 97).
-define(GM_MIDI_FX_3_crystal, 98).
-define(GM_MIDI_FX_4_atmosphere, 99).
-define(GM_MIDI_FX_5_brightness, 100).
-define(GM_MIDI_FX_6_goblins, 101).
-define(GM_MIDI_FX_7_echoes, 102).
-define(GM_MIDI_FX_8_sci_fi, 103 ).
%% Ethnic:
-define(GM_MIDI_Sitar, 104).
-define(GM_MIDI_Banjo, 105 ).
-define(GM_MIDI_Shamisen, 106).
-define(GM_MIDI_Koto, 107).
-define(GM_MIDI_Kalimba, 108).
-define(GM_MIDI_Bag_pipe, 109).
-define(GM_MIDI_Fiddle, 110).
-define(GM_MIDI_Shanai, 111 ).
%% Percussive:
-define(GM_MIDI_Tinkle_Bell, 112).
-define(GM_MIDI_Agogo, 113).
-define(GM_MIDI_Steel_Drums, 114).
-define(GM_MIDI_Woodblock, 115).
-define(GM_MIDI_Taiko_Drum, 116).
-define(GM_MIDI_Melodic_Tom, 117).
-define(GM_MIDI_Synth_Drum, 118).
%% Sound effects:
-define(GM_MIDI_Reverse_Cymbal, 119).
-define(GM_MIDI_Guitar_Fret_Noise, 120).
-define(GM_MIDI_Breath_Noise, 121).
-define(GM_MIDI_Seashore, 122).
-define(GM_MIDI_Bird_Tweet, 123).
-define(GM_MIDI_Telephone_Ring, 124).
-define(GM_MIDI_Helicopter, 125).
-define(GM_MIDI_Applause, 126).
-define(GM_MIDI_Gunshot, 127).

%% GM Midi channel 9
-define(GM_DRUM_Bass_Drum_2, 35).
-define(GM_DRUM_Bass_Drum_1, 36).
-define(GM_DRUM_Side_Stick, 37).
-define(GM_DRUM_Snare_Drum_1, 38).
-define(GM_DRUM_Hand_Clap, 39).
-define(GM_DRUM_Snare_Drum_2, 40).
-define(GM_DRUM_Low_Tom_2, 41).
-define(GM_DRUM_Closed_Hi_hat, 42).
-define(GM_DRUM_Low_Tom_1, 43).
-define(GM_DRUM_Pedal_Hi_hat, 44).
-define(GM_DRUM_Mid_Tom_2, 45).
-define(GM_DRUM_Open_Hi_hat, 46).
-define(GM_DRUM_Mid_Tom_1, 47).
-define(GM_DRUM_High_Tom_2, 48).
-define(GM_DRUM_Crash_Cymbal_1, 49).
-define(GM_DRUM_High_Tom_1, 50).
-define(GM_DRUM_Ride_Cymbal_1, 51).
-define(GM_DRUM_Chinese_Cymbal, 52).
-define(GM_DRUM_Ride_Bell, 53).
-define(GM_DRUM_Tambourine, 54).
-define(GM_DRUM_Splash_Cymbal, 55).
-define(GM_DRUM_Cowbell, 56).
-define(GM_DRUM_Crash_Cymbal_2, 57).
-define(GM_DRUM_Vibra_Slap, 58).
-define(GM_DRUM_Ride_Cymbal_2, 59).
-define(GM_DRUM_High_Bongo, 60).
-define(GM_DRUM_Low_Bongo, 61).
-define(GM_DRUM_Mute_High_Conga, 62).
-define(GM_DRUM_Open_High_Conga, 63).
-define(GM_DRUM_Low_Conga, 64).
-define(GM_DRUM_High_Timbale, 65).
-define(GM_DRUM_Low_Timbale, 66).
-define(GM_DRUM_High_Agogo, 67).
-define(GM_DRUM_Low_Agogo, 68).
-define(GM_DRUM_Cabasa, 69).
-define(GM_DRUM_Maracas, 70).
-define(GM_DRUM_Short_Whistle, 71).
-define(GM_DRUM_Long_Whistle, 72).
-define(GM_DRUM_Short_Guiro, 73).
-define(GM_DRUM_Long_Guiro, 74).
-define(GM_DRUM_Claves, 75).
-define(GM_DRUM_High_Wood_Block, 76).
-define(GM_DRUM_Low_Wood_Block, 77).
-define(GM_DRUM_Mute_Cuica, 78).
-define(GM_DRUM_Open_Cuica, 79).
-define(GM_DRUM_Mute_Triangle, 80).
-define(GM_DRUM_Open_Triangle, 81).

%% sysex header
-define(SYSEX_NON_REALTIME, 16#7E).
-define(SYSEX_REALTIME,     16#7F).

%% sysex messages (non-realtime)
-define(SYSEX_DUMP_HEADER, 16#01).
-define(SYSEX_DATA_PACKET, 16#02).
-define(SYSEX_DUMP_REQUEST, 16#03).

-define(SYSEX_SAMPLE_DUMP_EXTENSIONS, 16#05).
-define(SYSEX_DEVICE_INQUIRY, 16#06).
-define(DEVICE_INQUIRY_REQUEST, 1).
-define(DEVICE_INQUIRY_REPLY,   2).

-define(SYSEX_FILE_DUMP, 16#07).
-define(FILE_DUMP_HEADER, 1).
-define(FILE_DUMP_DATA_PACKET, 2).
-define(FILE_DUMP_REQUEST, 3).

-define(SYSEX_MIDI_TUNING_STANDARD, 16#08).
-define(SYSEX_GENERAL_MIDI, 16#09).
-define(GERNERAL_MIDI_ON, 1).
-define(GERNERAL_MIDI_OFF, 2).

-define(SYSEX_DEVICE_EOF, 16#7B).
-define(SYSEX_DEVICE_WAIT, 16#7C).
-define(SYSEX_DEVICE_CANCEL, 16#7D).
-define(SYSEX_DEVICE_NAK, 16#7E).
-define(SYSEX_DEVICE_ACK, 16#7F).




-endif.

