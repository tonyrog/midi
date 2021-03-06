/*
 *  midi devices & functions
 */
#ifndef _MIDI_H_
#define  _MIDI_H_

// MIDI message types
enum {
    MIDI_EVENT_NOTEOFF 	     = 0x80,  // channel 0-15 in lower nibble
    MIDI_EVENT_NOTEON 	     = 0x90,  // channel 0-15 in lower nibble
    MIDI_EVENT_AFTERTOUCH    = 0xA0,
    MIDI_EVENT_CONTROLCHANGE = 0xB0,  // channel 0-15 in lower nibble
    MIDI_EVENT_PROGRAMCHANGE = 0xC0,  // channel 0-15 in lower nibble
    MIDI_EVENT_PRESSURE      = 0xD0,
    MIDI_EVENT_PITCHBEND     = 0xE0,
    MIDI_EVENT_SYS           = 0xF0
};

enum {
    MIDI_META_TEXT           = 0x01,  // text
    MIDI_META_COPYRIGHT      = 0x02,  // text
    MIDI_META_TRACKNAME      = 0x03,  // text
    MIDI_META_INSTRUMENT     = 0x04,  // text
    MIDI_META_LYRIC          = 0x05,  // text
    MIDI_META_MARKER         = 0x06,  // text
    MIDI_META_CUE_POINT      = 0x07,  // text
    MIDI_META_PROGRAM_NAME   = 0x08,  // text
    MIDI_META_DEVICE_NAME    = 0x09,  // text
    MIDI_META_MIDI_CHANNEL   = 0x20,  // uint8
    MIDI_META_MIDI_PORT      = 0x21,  // uint8
    MIDI_META_END_OF_TRACK   = 0x2f,  // -
    MIDI_META_TEMPO          = 0x51,  // uint24/big
    MIDI_META_SMPTE_OFFSET   = 0x54,  // <<HR:8,MN:8,SE:8,FR:8,FF:8>>
    MIDI_META_TIME_SIGNATURE = 0x58,  // <<NN:8,DD:8,CC:8,BB:8>>
    MIDI_META_KEY_SIGNATURE  = 0x59,  // <<SF:8,MI:8>>
    MIDI_META_PROPRIETARY    = 0x7f   // text
};

#define MIDI_CONTROL_CHANGE(chan) (MIDI_EVENT_CONTROLCHANGE|((chan)&0xF))
#define MIDI_PROGRAM_CHANGE(chan) (MIDI_EVENT_PROGRAMCHANGE|((chan)&0xF))
#define MIDI_NOTE_ON(chan)        (MIDI_EVENT_NOTEON|((chan)&0xF))
#define MIDI_NOTE_OFF(chan)       (MIDI_EVENT_NOTEOFF|((chan)&0xF))


#define MIDI_CTRL_BANK_SELECT       0
#define MIDI_CTRL_MODULATION_WHEEL  1
#define MIDI_CTRL_BREATH_CONTROLLER 2
#define MIDI_CTRL_FOOT_PEDAL        4
#define MIDI_CTRL_PORTAMENTO_TIME   5
#define MIDI_CTRL_DATA_ENTRY        6
#define MIDI_CTRL_VOLUME            7
#define MIDI_CTRL_BALANCE           8
#define MIDI_CTRL_PAN_POSITION      10
#define MIDI_CTRL_EXPRESSION        11
#define MIDI_CTRL_EFFECT_CONTROL_1  12
#define MIDI_CTRL_EFFECT_CONTROL_2  13
#define MIDI_CTRL_GENERAL_PURPOSE_SLIDER_1 16
#define MIDI_CTRL_GENERAL_PURPOSE_SLIDER_2 17
#define MIDI_CTRL_GENERAL_PURPOSE_SLIDER_3 18
#define MIDI_CTRL_GENERAL_PURPOSE_SLIDER_4 19
#define MIDI_CTRL_BANK_SELECT_FINE         (32+0)
#define MIDI_CTRL_MODULATION_WHEEL_FINE    (32+1)
#define MIDI_CTRL_BREATH_CONTROLLER_FINE   (32+2)
#define MIDI_CTRL_FOOT_PEDAL_FINE          (32+4)
#define MIDI_CTRL_PORTAMENTO_TIME_FINE     (32+5)
#define MIDI_CTRL_DATA_ENTRY_FINE          (32+6)
#define MIDI_CTRL_VOLUME_FINE              (32+7)
#define MIDI_CTRL_BALANCE_FINE             (32+8) 
#define MIDI_CTRL_PAN_POSITION_FINE        (32+10)
#define MIDI_CTRL_EXPRESSION_FINE          (32+11)
#define MIDI_CTRL_EFFECT_CONTROL_1_FINE    (32+12)
#define MIDI_CTRL_EFFECT_CONTROL_2_FINE    (32+13)
#define MIDI_CTRL_HOLD_PEDAL               64
#define MIDI_CTRL_PORTAMENTO               65
#define MIDI_CTRL_SUSTENUTO_PEDAL          66
#define MIDI_CTRL_SOFT_PEDAL               67
#define MIDI_CTRL_LEGATO_PEDAL             68
#define MIDI_CTRL_HOLD_2_PEDAL             69
#define MIDI_CTRL_SOUND_VARIATION          70
#define MIDI_CTRL_SOUND_TIMBRE             71
#define MIDI_CTRL_SOUND_RELEASE_TIME       72
#define MIDI_CTRL_SOUND_ATTACK_TIME        73
#define MIDI_CTRL_SOUND_BRIGHTNESS         74
#define MIDI_CTRL_SOUND_CONTROL_6          75
#define MIDI_CTRL_SOUND_CONTROL_7          76
#define MIDI_CTRL_SOUND_CONTROL_8          77
#define MIDI_CTRL_SOUND_CONTROL_9          78
#define MIDI_CTRL_SOUND_CONTROL_10         79
#define MIDI_CTRL_GENERAL_PURPOSE_BUTTON_1 80
#define MIDI_CTRL_GENERAL_PURPOSE_BUTTON_2 81
#define MIDI_CTRL_GENERAL_PURPOSE_BUTTON_3 82
#define MIDI_CTRL_GENERAL_PURPOSE_BUTTON_4 83
#define MIDI_CTRL_EFFECTS_LEVEL            91
#define MIDI_CTRL_TREMULO_LEVEL            92
#define MIDI_CTRL_CHORUS_LEVEL             93
#define MIDI_CTRL_CELESTE_LEVEL            94
#define MIDI_CTRL_PHASER_LEVEL             95
#define MIDI_CTRL_DATA_BUTTON_INCREMENT    96
#define MIDI_CTRL_DATA_BUTTON_DECREMENT    97
#define MIDI_CTRL_NON_REGISTERED_PARAMETER_FINE 98
#define MIDI_CTRL_NON_REGISTERED_PARAMETER  99
#define MIDI_CTRL_REGISTERED_PARAMETER_FINE 100
#define MIDI_CTRL_REGISTERED_PARAMETER     101
#define MIDI_CTRL_ALL_SOUND_OFF            120
#define MIDI_CTRL_ALL_CONTROLLERS_OFF      121
#define MIDI_CTRL_LOCAL_KEYBOARD           122
#define MIDI_CTRL_ALL_NOTES_OFF            123
#define MIDI_CTRL_OMNI_MODE_OFF            124
#define MIDI_CTRL_OMNI_MODE_ON             125
#define MIDI_CTRL_MONO_OPERATION           126
#define MIDI_CTRL_POLY_OPERATION           127

// MIDI control 
#if 0
enum {
    MIDI_CONTROL_BankMSB         = 0x00,
    MIDI_CONTROL_BankLSB	 = 0x20,
    MIDI_CONTROL_Volume          = 0x07,
    MIDI_CONTROL_Modulation      = 0x01,
    MIDI_CONTROL_DataEntryMSB    = 0x06,
    MIDI_CONTROL_Pan             = 0x0A,
    MIDI_CONTROL_Expression      = 0x0B,
    MIDI_CONTROL_DataEntryLSB    = 0x26,
    MIDI_CONTROL_Sustain         = 0x40,
    MIDI_CONTROL_RPNLSB          = 0x64,
    MIDI_CONTROL_RPNMSB          = 0x65,
    MIDI_CONTROL_ResetAll        = 0x79,
    MIDI_CONTROL_AllNotesOff     = 0x7B
};
#endif

// General midi instruments Piano:
#define GM_MIDI_Acoustic_Grand_Piano  0
#define GM_MIDI_Bright_Acoustic_Piano 1
#define GM_MIDI_Electric_Grand_Piano  2 
#define GM_MIDI_Honky_tonk_Piano      3 
#define GM_MIDI_Electric_Piano_1 4
#define GM_MIDI_Electric_Piano_2 5
#define GM_MIDI_Harpsichord 6
#define GM_MIDI_Clavinet 7
// Chromatic Percussion:
#define GM_MIDI_Celesta 8
#define GM_MIDI_Glockenspiel 9
#define GM_MIDI_Music_Box 10
#define GM_MIDI_Vibraphone 11
#define GM_MIDI_Marimba 12
#define GM_MIDI_Xylophone 13
#define GM_MIDI_Tubular_Bells 14
#define GM_MIDI_Dulcimer 15
// Organ:
#define GM_MIDI_Drawbar_Organ 16
#define GM_MIDI_Percussive_Organ 17
#define GM_MIDI_Rock_Organ 18
#define GM_MIDI_Church_Organ 19
#define GM_MIDI_Reed_Organ 20
#define GM_MIDI_Accordion 21
#define GM_MIDI_Harmonica 22 
#define GM_MIDI_Tango_Accordion 23
// Guitar:
#define GM_MIDI_Acoustic_Guitar_nylon 24
#define GM_MIDI_Acoustic_Guitar_steel 25 
#define GM_MIDI_Electric_Guitar_jazz 26
#define GM_MIDI_Electric_Guitar_clean 27
#define GM_MIDI_Electric_Guitar_muted 28 
#define GM_MIDI_Overdriven_Guitar 29
#define GM_MIDI_Distortion_Guitar 30 
#define GM_MIDI_Guitar_harmonics 31
// Bass
#define GM_MIDI_Acoustic_Bass 32
#define GM_MIDI_Electric_Bass_finger 33
#define GM_MIDI_Electric_Bass_pick 34
#define GM_MIDI_Fretless_Bass 35
#define GM_MIDI_Slap_Bass_1 36
#define GM_MIDI_Slap_Bass_2 37 
#define GM_MIDI_Synth_Bass_1 38
#define GM_MIDI_Synth_Bass_2 39
// Strings:
#define GM_MIDI_Violin 40
#define GM_MIDI_Viola 41
#define GM_MIDI_Cello 42 
#define GM_MIDI_Contrabass 43
#define GM_MIDI_Tremolo_Strings 44
#define GM_MIDI_Pizzicato_Strings 45
#define GM_MIDI_Orchestral_Harp 46
#define GM_MIDI_Timpani 47
// Strings (continued):
#define GM_MIDI_String_Ensemble_1 48
#define GM_MIDI_String_Ensemble_2 49 
#define GM_MIDI_Synth_Strings_1 50
#define GM_MIDI_Synth_Strings_2 51 
#define GM_MIDI_Choir_Aahs 52
#define GM_MIDI_Voice_Oohs 53 
#define GM_MIDI_Synth_Voice 54
#define GM_MIDI_Orchestra_Hit 55
// Brass:
#define GM_MIDI_Trumpet 56
#define GM_MIDI_Trombone 57
#define GM_MIDI_Tuba 58
#define GM_MIDI_Muted_Trumpet 59
#define GM_MIDI_French_Horn 60
#define GM_MIDI_Brass_Section 61
#define GM_MIDI_Synth_Brass_1 62 
#define GM_MIDI_Synth_Brass_2 63 
// Reed:
#define GM_MIDI_Soprano_Sax 64
#define GM_MIDI_Alto_Sax 65
#define GM_MIDI_Tenor_Sax 66
#define GM_MIDI_Baritone_Sax 67
#define GM_MIDI_Oboe 68
#define GM_MIDI_English_Horn 69
#define GM_MIDI_Bassoon 70
#define GM_MIDI_Clarinet 71 
// Pipe:
#define GM_MIDI_Piccolo 72
#define GM_MIDI_Flute 73
#define GM_MIDI_Recorder 74
#define GM_MIDI_Pan_Flute 75
#define GM_MIDI_Blown_Bottle 76
#define GM_MIDI_Shakuhachi 77
#define GM_MIDI_Whistle 78
#define GM_MIDI_Ocarina 79 
// Synth Lead:
#define GM_MIDI_Lead_1_square 80
#define GM_MIDI_Lead_2_sawtooth 81
#define GM_MIDI_Lead_3_calliope 82 
#define GM_MIDI_Lead_4_chiff 83
#define GM_MIDI_Lead_5_charang 84
#define GM_MIDI_Lead_6_voice 85
#define GM_MIDI_Lead_7_fifths 86
#define GM_MIDI_Lead_8_bass_plus_lead 87
// Synth Pad:
#define GM_MIDI_Pad_1_new_age 88
#define GM_MIDI_Pad_2_warm 89
#define GM_MIDI_Pad_3_polysynth 90
#define GM_MIDI_Pad_4_choir 91
#define GM_MIDI_Pad_5_bowed 92 
#define GM_MIDI_Pad_6_metallic 93
#define GM_MIDI_Pad_7_halo 94
#define GM_MIDI_Pad_8_sweep 95
// Synth Effects:
#define GM_MIDI_FX_1_rain 96
#define GM_MIDI_FX_2_soundtrack 97
#define GM_MIDI_FX_3_crystal 98
#define GM_MIDI_FX_4_atmosphere 99
#define GM_MIDI_FX_5_brightness 100
#define GM_MIDI_FX_6_goblins 101
#define GM_MIDI_FX_7_echoes 102
#define GM_MIDI_FX_8_sci_fi 103 
// Ethnic:
#define GM_MIDI_Sitar 104
#define GM_MIDI_Banjo 105 
#define GM_MIDI_Shamisen 106
#define GM_MIDI_Koto 107
#define GM_MIDI_Kalimba 108
#define GM_MIDI_Bag_pipe 109
#define GM_MIDI_Fiddle 110
#define GM_MIDI_Shanai 111 
// Percussive:
#define GM_MIDI_Tinkle_Bell 112
#define GM_MIDI_Agogo 113
#define GM_MIDI_Steel_Drums 114
#define GM_MIDI_Woodblock 115
#define GM_MIDI_Taiko_Drum 116
#define GM_MIDI_Melodic_Tom 117
#define GM_MIDI_Synth_Drum 118
// Sound effects:
#define GM_MIDI_Reverse_Cymbal 119
#define GM_MIDI_Guitar_Fret_Noise 120
#define GM_MIDI_Breath_Noise 121
#define GM_MIDI_Seashore 122
#define GM_MIDI_Bird_Tweet 123
#define GM_MIDI_Telephone_Ring 124
#define GM_MIDI_Helicopter 125
#define GM_MIDI_Applause 126
#define GM_MIDI_Gunshot 127

// GM Midi channel 10 (hmm +1 ? test)
#define GM_DRUM_Bass_Drum_2 34
#define GM_DRUM_Bass_Drum_1 35 
#define GM_DRUM_Side_Stick 36
#define GM_DRUM_Snare_Drum_1 37
#define GM_DRUM_Hand_Clap 38
#define GM_DRUM_Snare_Drum_2 39
#define GM_DRUM_Low_Tom_2 40
#define GM_DRUM_Closed_Hi_hat 41
#define GM_DRUM_Low_Tom_1 42
#define GM_DRUM_Pedal_Hi_hat 43
#define GM_DRUM_Mid_Tom_2 44
#define GM_DRUM_Open_Hi_hat 45
#define GM_DRUM_Mid_Tom_1 46
#define GM_DRUM_High_Tom_2 47
#define GM_DRUM_Crash_Cymbal_1 48
#define GM_DRUM_High_Tom_1 49
#define GM_DRUM_Ride_Cymbal_1 50
#define GM_DRUM_Chinese_Cymbal 51
#define GM_DRUM_Ride_Bell 52
#define GM_DRUM_Tambourine 53
#define GM_DRUM_Splash_Cymbal 54
#define GM_DRUM_Cowbell 55
#define GM_DRUM_Crash_Cymbal_2 56
#define GM_DRUM_Vibra_Slap 58
#define GM_DRUM_Ride_Cymbal_2 58
#define GM_DRUM_High_Bongo 59
#define GM_DRUM_Low_Bongo 60
#define GM_DRUM_Mute_High_Conga 61
#define GM_DRUM_Open_High_Conga 62 
#define GM_DRUM_Low_Conga 63
#define GM_DRUM_High_Timbale 64
#define GM_DRUM_Low_Timbale 65
#define GM_DRUM_High_Agogo 66
#define GM_DRUM_Low_Agogo 67
#define GM_DRUM_Cabasa 68
#define GM_DRUM_Maracas 69
#define GM_DRUM_Short_Whistle 70
#define GM_DRUM_Long_Whistle 71
#define GM_DRUM_Short_Guiro 72
#define GM_DRUM_Long_Guiro 73
#define GM_DRUM_Claves 74
#define GM_DRUM_High_Wood_Block 75
#define GM_DRUM_Low_Wood_Block 76
#define GM_DRUM_Mute_Cuica 77
#define GM_DRUM_Open_Cuica 78 
#define GM_DRUM_Mute_Triangle 79
#define GM_DRUM_Open_Triangle 80 

#endif
