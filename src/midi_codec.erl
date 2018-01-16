%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    encode/decode midi commands
%%% @end
%%% Created :  1 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_codec).

-export([init/1, scan/1, scan/2]).
-export([scan_delta/1]).
-export([event_decode/2, event_encode/1]).
-export([control_decode/1, control_encode/1]).
-export([meta_decode/1, meta_encode/2]).
-export([length_decode/1, length_encode/1]).
-export([note_decode/1, note_encode/1]).

-include("midi.hrl").

init(Cs) ->
    {status, Cs, 0, 0, []}.

%% continue
scan(St={_State,[],_Status,_Running,_Params}) ->
    {more,St};
scan({State,Cs,Status,Running,Params}) ->
    parse(State,Cs,Status,Running,Params).

%% continue
scan(Chars, {State,Cs,Status,Running,Params}) ->
    parse(State,Cs++Chars,Status,Running,Params).

parse(status, Cs=[C|_], _Status, Running, _Params) when C band 16#80 =:= 0 ->
    parse(params, Cs, Running, Running, []);
parse(status, [C|Cs], Status, Running, _Params) ->
    if Status band 16#f0 =:= 16#f0 ->
	    if Status =< 16#f7 -> parse(params,Cs,C,0,[]);
	       true -> parse(params,Cs,C,Running,[])
	    end;
       true -> parse(params,Cs,C,C,[])
    end;
parse(status, [], Status, Running, Params) ->
    {eot, {status,[],Status,Running,Params}};
parse(params,Cs,Status,Running,Params) ->
    case Status band 16#f0 of
	?MIDI_EVENT_NOTEOFF ->
	    parse(params_2,Cs,Status,Running,Params);
	?MIDI_EVENT_NOTEON ->
	    parse(params_2,Cs,Status,Running,Params);
	?MIDI_EVENT_AFTERTOUCH ->
	    parse(params_2,Cs,Status,Running,Params);
	?MIDI_EVENT_CONTROLCHANGE ->
	    parse(params_2,Cs,Status,Running,Params);
	?MIDI_EVENT_PITCHBEND ->
	    parse(params_2,Cs,Status,Running,Params);
	?MIDI_EVENT_PROGRAMCHANGE ->
	    parse(params_1,Cs,Status,Running,Params);
	?MIDI_EVENT_PRESSURE ->
	    parse(params_1,Cs,Status,Running,Params);
	?MIDI_EVENT_SYS ->
	    case Status band 16#0f of
		0  -> parse(params_f7,Cs,Status,Running,Params);
		1  -> parse(params_0,Cs,Status,Running,Params);
		2  -> parse(params_2,Cs,Status,Running,Params);
		3  -> parse(params_1,Cs,Status,Running,Params);
		4  -> parse(params_0,Cs,Status,Running,Params);
		5  -> parse(params_0,Cs,Status,Running,Params);
		6  -> parse(params_0,Cs,Status,Running,Params);
		7  -> parse(params_0,Cs,Status,Running,Params);
		8  -> parse(params_0,Cs,Status,Running,Params);
		9  -> parse(params_0,Cs,Status,Running,Params);
		10 -> parse(params_0,Cs,Status,Running,Params);
		11 -> parse(params_0,Cs,Status,Running,Params);
		12 -> parse(params_0,Cs,Status,Running,Params);
		13 -> parse(params_0,Cs,Status,Running,Params);
		14 -> parse(params_0,Cs,Status,Running,Params);
		15 -> parse(params_v1,Cs,Status,Running,Params)
	    end
    end;

parse(params_0,Cs,Status,Running,Params) ->
    {{Status,Params},{status,Cs,0,Running,[]}};
parse(params_1,[C|Cs],Status,Running,_Params) ->
    {{Status,[C]},{status,Cs,0,Running,[]}};
parse(params_2,[C1,C2|Cs],Status,Running,_Params) ->
    {{Status,[C1,C2]},{status,Cs,0,Running,[]}};
parse(params_f7,[16#f7|Cs],Status,Running,Params) ->
    {{Status,lists:reverse(Params)},{status,Cs,0,Running,[]}};
parse(params_f7,[C|Cs],Status,Running,Params) ->
    parse(params_f7,Cs,Status,Running,[C|Params]);
parse(params_v1,[Meta|Cs],Status,Running,Params) ->
    parse(params_v,Cs,Status,Running,[Meta|Params]);
parse(params_v,[L0|Cs],Status,Running,Params)
  when L0 band 16#80 =:= 0 ->
    Len = L0,
    parse({params_v,Len},Cs,Status,Running,Params);
parse(params_v,[L0,L1|Cs],Status,Running,Params)
  when L0 band 16#80 =:= 16#80,
       L1 band 16#80 =:= 0 ->
    Len = ((L0 band 16#7f) bsl 7) bor L1,
    parse({params_v,Len},Cs,Status,Running,Params);
parse(params_v,[L0,L1,L2|Cs],Status,Running,Params)
  when L0 band 16#80 =:= 16#80,
       L1 band 16#80 =:= 16#80,
       L2 band 16#80 =:= 0 ->
    Len = ((L0 band 16#7f) bsl 14) bor ((L1 band 16#7f) bsl 7) bor L2,
    parse({params_v,Len},Cs,Status,Running,Params);
parse(params_v,[L0,L1,L2,L3|Cs],Status,Running,Params)
  when L0 band 16#80 =:= 16#80,
       L1 band 16#80 =:= 16#80,
       L2 band 16#80 =:= 16#80 ->
    %% maybe warn if L3 is > 16#7f?
    Len = ((L0 band 16#7f) bsl 21) bor ((L1 band 16#7f) bsl 14) bor
	((L2 band 16#7f) bsl 7) bor (L3 band 16#7f),
    parse({params_v,Len},Cs,Status,Running,Params);
parse({params_v,0},Cs,Status,Running,Params) ->
    {{Status,lists:reverse(Params)},{status,Cs,0,Running,[]}};
parse({params_v,I},[C|Cs],Status,Running,Params) ->
    parse({params_v,I-1},Cs,Status,Running,[C|Params]);
parse(State,[],Status,Running,Params) ->
    {more, {State,[],Status,Running,lists:reverse(Params)}}.


scan_delta({status,[],Status,Running,Params}) ->
    {eot, {status,[],Status,Running,Params}};
scan_delta({State,Cs,Status,Running,Params}) ->
    parse_delta(Cs,State,Status,Running,Params).

parse_delta([L0|Cs],State,Status,Running,Params)
  when L0 band 16#80 =:= 0 ->
    D = L0,
    {{ok,D}, {State,Cs,Status,Running,Params}};
parse_delta([L0,L1|Cs],State,Status,Running,Params)
  when L0 band 16#80 =:= 16#80,
       L1 band 16#80 =:= 0 ->
    D = ((L0 band 16#7f) bsl 7) bor L1,
    {{ok,D}, {State,Cs,Status,Running,Params}};
parse_delta([L0,L1,L2|Cs],State,Status,Running,Params)
  when L0 band 16#80 =:= 16#80,
       L1 band 16#80 =:= 16#80,
       L2 band 16#80 =:= 0 ->
    D = ((L0 band 16#7f) bsl 14) bor ((L1 band 16#7f) bsl 7) bor L1,
    {{ok,D}, {State,Cs,Status,Running,Params}};
parse_delta([L0,L1,L2,L3|Cs],State,Status,Running,Params)
  when L0 band 16#80 =:= 16#80,
       L1 band 16#80 =:= 16#80,
       L2 band 16#80 =:= 16#80 ->
    %% maybe warn if L3 is > 16#7f?
    D = ((L0 band 16#7f) bsl 21) bor ((L1 band 16#7f) bsl 14) bor
	((L2 band 16#7f) bsl 7) bor (L3 band 16#7f),
    {{ok,D}, {State,Cs,Status,Running,Params}};
parse_delta(Cs,State,Status,Running,Params) ->
    {{error,bad_delta}, {State,Cs,Status,Running,Params}}.

%% encode midi commans fixme binary?
event_encode({note_off,Chan,Note,Velocity}) ->
    [?MIDI_EVENT_NOTEOFF bor (Chan band 16#f),
     Note band 16#7f, Velocity band 16#7f];
event_encode({note_on,Chan,Note,Velocity}) ->
    [?MIDI_EVENT_NOTEON bor (Chan band 16#f),
     Note band 16#7f, Velocity band 16#7f];
event_encode({after_touch,Chan, B, C}) ->
    [?MIDI_EVENT_AFTERTOUCH bor (Chan band 16#f),
     B band 16#7f, C band 16#7f];
event_encode({control_change,Chan,Control,Param}) ->
    [?MIDI_EVENT_CONTROLCHANGE bor (Chan band 16#f),
     control_encode(Control), Param band 16#7f];
event_encode({pitch_bend,Chan,Bend}) ->
    Bend1 = Bend + 16#2000,
    [?MIDI_EVENT_PITCHBEND bor (Chan band 16#f),
     Bend1 band 16#7f, (Bend1 bsr 7) band 16#7f];
event_encode({program_change,Chan,Prog}) ->     
    [?MIDI_EVENT_PROGRAMCHANGE bor (Chan band 16#f),
     Prog band 16#7f];
event_encode({pressure,Chan,Pressure}) ->     
    [?MIDI_EVENT_PROGRAMCHANGE bor (Chan band 16#f),
     Pressure band 16#7f];
event_encode({sysex,Params}) ->
    [(?MIDI_EVENT_SYS bor 0) | Params];
event_encode({sys1,Params}) ->
    [(?MIDI_EVENT_SYS bor 1) | Params];
event_encode({song_position_pointer,Params}) ->
    [(?MIDI_EVENT_SYS bor 2) | Params];
event_encode({song_select,Params}) ->
    [(?MIDI_EVENT_SYS bor 3) | Params];
event_encode({sys4,Params}) ->
    [(?MIDI_EVENT_SYS bor 4) | Params];    
event_encode({sys5,Params}) ->
    [(?MIDI_EVENT_SYS bor 5) | Params];    
event_encode({tune_request,Params}) ->
    [(?MIDI_EVENT_SYS bor 6) | Params];
event_encode({eox,Params}) ->
    [(?MIDI_EVENT_SYS bor 7) | Params];
event_encode({timing_clock,Params}) ->
    [(?MIDI_EVENT_SYS bor 8) | Params];
event_encode({sys9,Params}) ->
    [(?MIDI_EVENT_SYS bor 9) | Params];
event_encode({start,Params}) ->
    [(?MIDI_EVENT_SYS bor 10) | Params];
event_encode({continue,Params}) ->
    [(?MIDI_EVENT_SYS bor 11) | Params];
event_encode({stop,Params}) ->
    [(?MIDI_EVENT_SYS bor 12) | Params];
event_encode({sys13,Params}) ->
    [(?MIDI_EVENT_SYS bor 13) | Params];
event_encode({active_sensing,Params}) ->
    [(?MIDI_EVENT_SYS bor 14) | Params];
event_encode({meta,text,Text}) ->
    meta_encode(?MIDI_META_TEXT,Text);
event_encode({meta,copyright,Text}) ->
    meta_encode(?MIDI_META_COPYRIGHT,Text);
event_encode({meta,track_name,Text}) ->
    meta_encode(?MIDI_META_TRACKNAME,Text);
event_encode({meta,instrument,Text}) ->
    meta_encode(?MIDI_META_INSTRUMENT,Text);
event_encode({meta,lyric,Text}) ->
    meta_encode(?MIDI_META_LYRIC,Text);
event_encode({meta,marker,Text}) ->
    meta_encode(?MIDI_META_MARKER,Text);
event_encode({meta,cue_point,Text}) ->
    meta_encode(?MIDI_META_CUE_POINT,Text);
event_encode({meta,program_name,Text}) ->
    meta_encode(?MIDI_META_PROGRAM_NAME,Text);
event_encode({meta,device_name,Text}) ->
    meta_encode(?MIDI_META_DEVICE_NAME,Text);
event_encode({meta,midi_channel,Channel}) ->
    meta_encode(?MIDI_META_MIDI_CHANNEL, [Channel band 16#7f]);
event_encode({meta,midi_port,Port}) ->
    meta_encode(?MIDI_META_MIDI_PORT, [Port band 16#7f]);
event_encode({meta,end_of_track,_}) ->
    meta_encode(?MIDI_META_END_OF_TRACK, []);
event_encode({meta,tempo,Tempo}) ->
    meta_encode(?MIDI_META_TEMPO, 
		[(Tempo bsl 16) band 16#ff,
		 (Tempo bsl 8) band 16#ff,
		 (Tempo bsl 0) band 16#ff]);
event_encode({meta, smpte_offset, [HR,MN,SE,FR,FF]}) ->
    meta_encode(?MIDI_META_SMPTE_OFFSET, [HR,MN,SE,FR,FF]);
event_encode({meta, time_signature, [NN,DD,CC,BB]}) ->
    meta_encode(?MIDI_META_TIME_SIGNATURE, [NN,DD,CC,BB]);
event_encode({meta, key_signature, [SF,MI]}) ->
    meta_encode(?MIDI_META_KEY_SIGNATURE, [SF,MI]);
event_encode({meta, proprietary, Params}) ->
    meta_encode(?MIDI_META_PROPRIETARY, Params);
event_encode({meta, Meta, Params}) when is_integer(Meta) ->
    meta_encode(Meta,Params).

meta_encode(Meta, Params) ->
    [(?MIDI_EVENT_SYS bor 15), Meta | length_encode(length(Params))] ++
	Params.

length_encode(L) when L =< 16#7f -> 
    [L];
length_encode(L) when L =< 16#3fff ->
    [((L bsr 7) band 16#7f) bor 16#80,
     (L band 16#7f)];
length_encode(L) when L =< 16#1fffff ->
    [((L bsr 14) band 16#7f) bor 16#80, 
     ((L bsr 7) band 16#7f) bor 16#80,
     (L band 16#7f)];
length_encode(L) when L =< 16#fffffff ->
    [((L bsr 21) band 16#7f) bor 16#80, 
     ((L bsr 14) band 16#7f) bor 16#80, 
     ((L bsr 7) band 16#7f) bor 16#80,
     (L band 16#7f)].

-ifdef(not_yet).
length_decode(<<0:1,L0:7>>) -> L0;
length_decode(<<1:1,L1:7,0:1,L0:7>>) ->
    (L1 bsl 7) + L0;
length_decode(<<1:1,L2:7,1:1,L1:7,0:1,L0:7>>) ->
    (L2 bsl 14) + (L1 bsl 7) + L0;
length_decode(<<1:1,L3:7,1:1,L2:7,1:1,L1:7,0:1,L0:7>>) ->
    (L3 bsl 21) + (L2 bsl 14) + (L1 bsl 7) + L0.
-endif.


length_decode([L0|Cs]) when L0 band 16#80 =:= 0 ->
    {L0,Cs};
length_decode([L0,L1|Cs]) when L0 band 16#80 =:= 16#80,
			       L1 band 16#80 =:= 0 ->
    {((L0 band 16#7f) bsl 7) bor L1, Cs};
length_decode([L0,L1,L2|Cs]) when L0 band 16#80 =:= 16#80,
				  L1 band 16#80 =:= 16#80,
				  L2 band 16#80 =:= 0 ->
    {((L0 band 16#7f) bsl 14) bor ((L1 band 16#7f) bsl 7) bor L2,Cs};
length_decode([L0,L1,L2,L3|Cs]) when L0 band 16#80 =:= 16#80,
				     L1 band 16#80 =:= 16#80,
				     L2 band 16#80 =:= 16#80 ->
    {((L0 band 16#7f) bsl 21) bor ((L1 band 16#7f) bsl 14) bor
	 ((L2 band 16#7f) bsl 7) bor (L3 band 16#7f), Cs}.


%% translate status and params into symbolic midi event form
event_decode(Status,Params=[B,C]) ->
    case Status band 16#F0 of
	?MIDI_EVENT_NOTEOFF ->
	    {note_off,Status band 16#0F, B, C};
	?MIDI_EVENT_NOTEON ->
	    {note_on,Status band 16#0F, B, C};
	?MIDI_EVENT_AFTERTOUCH ->
	    {after_touch,Status band 16#0F, B, C};
	?MIDI_EVENT_CONTROLCHANGE ->
	    {control_change,Status band 16#0F, control_decode(B), C};
	?MIDI_EVENT_PITCHBEND ->
	    {pitch_bend,Status band 16#0F, ((C bsl 7) bor B) - 16#2000};
	?MIDI_EVENT_SYS ->
	    sys_decode(Status band 16#0F, Params)
    end;
event_decode(Status,Params=[B]) ->
    case Status band 16#F0 of
	?MIDI_EVENT_PROGRAMCHANGE ->
	    {program_change,Status band 16#0F, B};
	?MIDI_EVENT_PRESSURE ->
	    {pressure,Status band 16#0F, B};
	?MIDI_EVENT_SYS ->
	    sys_decode(Status band 16#0F, Params)
    end;
event_decode(Status,Params) ->
    case Status band 16#F0 of
	?MIDI_EVENT_SYS ->
	    sys_decode(Status band 16#0F, Params)
    end.

sys_decode(Sys, Params) ->
    case Sys of
	0 -> {sysex,Params};
	1 -> {sys1,Params};
	2 -> {song_position_pointer,Params};
	3 -> {song_select,Params};
	4 -> {sys4,Params};
	5 -> {sys5,Params};
	6 -> {tune_request,Params};
	7 -> {eox,Params};
	8 -> {timing_clock,Params};
	9 -> {sys9,Params};
	10 -> {start,Params};
	11 -> {continue,Params};
	12 -> {stop,Params};
	13 -> {sys13,Params};
	14 -> {active_sensing,Params};
	15 -> meta_decode(Params)
    end.

meta_decode([Meta|Params]) ->
    case Meta of
	?MIDI_META_TEXT      -> {meta,text,Params};  %% text
	?MIDI_META_COPYRIGHT -> {meta,copyright,Params};  %% text
	?MIDI_META_TRACKNAME -> {meta,track_name,Params};   %% text
	?MIDI_META_INSTRUMENT -> {meta,instrument,Params};  %% text
	?MIDI_META_LYRIC -> {meta,lyric,Params};  %% text
	?MIDI_META_MARKER -> {meta,marker,Params};  %% text
	?MIDI_META_CUE_POINT -> {meta,cue_point,Params};  %% text
	?MIDI_META_PROGRAM_NAME -> {meta,program_name,Params};  %% text
	?MIDI_META_DEVICE_NAME -> {meta,device_name,Params};  %% text
	?MIDI_META_MIDI_CHANNEL -> {meta,midi_channel,Params}; %% uint8
	?MIDI_META_MIDI_PORT -> {meta,midi_port,Params};  %% uint8
	?MIDI_META_END_OF_TRACK -> {meta,end_of_track,[]}; %% -
	?MIDI_META_TEMPO -> %% uint24/big
	    case Params of
		[T2,T1,T0] -> {meta,tempo,T2*65536+T1*256+T0}  
	    end;
	?MIDI_META_SMPTE_OFFSET -> %% <<HR:8,MN:8,SE:8,FR:8,FF:8>>
	    case Params of
		[HR,MN,SE,FR,FF] ->
		    {meta, smpte_offset, [HR,MN,SE,FR,FF]}
	    end;
	?MIDI_META_TIME_SIGNATURE -> %% <<NN:8,DD:8,CC:8,BB:8>>
	    case Params of
		[NN,DD,CC,BB] ->
		    {meta,time_signature, [NN,DD,CC,BB]}
	    end;
	?MIDI_META_KEY_SIGNATURE -> %% <<SF:8,MI:8>>
	    case Params of
		[SF,MI] -> 
		    {meta, key_signature, [SF,MI]}
	    end;
	?MIDI_META_PROPRIETARY ->
	    {meta, proprietary, Params};
	_ ->
	    {meta, Meta, Params}
    end.

-define(ITEM(A,B), (A) => (B), (B) => (A)).
 
%% encode/decode control functions
control_map() ->
    #{
       ?ITEM(?MIDI_CTRL_BANK_SELECT, 'bank-select'),
       ?ITEM(?MIDI_CTRL_MODULATION_WHEEL, 'modulation-wheel'),
       ?ITEM(?MIDI_CTRL_BREATH_CONTROLLER, 'breath-controller'),
       ?ITEM(?MIDI_CTRL_FOOT_PEDAL, 'foot-pedal'),
       ?ITEM(?MIDI_CTRL_PORTAMENTO_TIME, 'portamento-time'),
       ?ITEM(?MIDI_CTRL_DATA_ENTRY, 'data-entry'),
       ?ITEM(?MIDI_CTRL_VOLUME, 'volume'),
       ?ITEM(?MIDI_CTRL_BALANCE, 'balance'),
       ?ITEM(?MIDI_CTRL_PAN_POSITION, 'pan-position'),
       ?ITEM(?MIDI_CTRL_EXPRESSION, 'expression'),
       ?ITEM(?MIDI_CTRL_EFFECT_CONTROL_1, 'effect-control-1'),
       ?ITEM(?MIDI_CTRL_EFFECT_CONTROL_2, 'effect-control-2'),
       ?ITEM(?MIDI_CTRL_GENERAL_PURPOSE_SLIDER_1, 'general-purpose-slider-1'),
       ?ITEM(?MIDI_CTRL_GENERAL_PURPOSE_SLIDER_2, 'general-purpose-slider-2'),
       ?ITEM(?MIDI_CTRL_GENERAL_PURPOSE_SLIDER_3, 'general-purpose-slider-3'),
       ?ITEM(?MIDI_CTRL_GENERAL_PURPOSE_SLIDER_4, 'general-purpose-slider-4'),
       ?ITEM(?MIDI_CTRL_BANK_SELECT_FINE, 'bank-select-fine'),
       ?ITEM(?MIDI_CTRL_MODULATION_WHEEL_FINE, 'modulation-wheel-fine'),
       ?ITEM(?MIDI_CTRL_BREATH_CONTROLLER_FINE, 'breath-controller-fine'),
       ?ITEM(?MIDI_CTRL_FOOT_PEDAL_FINE, 'foot-pedal-fine'),
       ?ITEM(?MIDI_CTRL_PORTAMENTO_TIME_FINE, 'portamento-time-fine'),
       ?ITEM(?MIDI_CTRL_DATA_ENTRY_FINE, 'data-entry-fine'),
       ?ITEM(?MIDI_CTRL_VOLUME_FINE, 'volume-fine'),
       ?ITEM(?MIDI_CTRL_BALANCE_FINE, 'balance-fine'),
       ?ITEM(?MIDI_CTRL_PAN_POSITION_FINE, 'pan-position-fine'),
       ?ITEM(?MIDI_CTRL_EXPRESSION_FINE, 'expression-fine'),
       ?ITEM(?MIDI_CTRL_EFFECT_CONTROL_1_FINE, 'effect-control-1-fine'),
       ?ITEM(?MIDI_CTRL_EFFECT_CONTROL_2_FINE, 'effect-control-2-fine'),
       ?ITEM(?MIDI_CTRL_HOLD_PEDAL, 'hold-pedal'),
       ?ITEM(?MIDI_CTRL_PORTAMENTO, 'portamento'),
       ?ITEM(?MIDI_CTRL_SUSTENUTO_PEDAL, 'sustenuto-pedal'),
       ?ITEM(?MIDI_CTRL_SOFT_PEDAL, 'soft-pedal'),
       ?ITEM(?MIDI_CTRL_LEGATO_PEDAL, 'legato-pedal'),
       ?ITEM(?MIDI_CTRL_HOLD_2_PEDAL, 'hold-2-pedal'),
       ?ITEM(?MIDI_CTRL_SOUND_VARIATION, 'sound-variation'),
       ?ITEM(?MIDI_CTRL_SOUND_TIMBRE, 'sound-timbre'),
       ?ITEM(?MIDI_CTRL_SOUND_RELEASE_TIME, 'sound-release-time'),
       ?ITEM(?MIDI_CTRL_SOUND_ATTACK_TIME, 'sound-attack-time'),
       ?ITEM(?MIDI_CTRL_SOUND_BRIGHTNESS, 'sound-brightness'),
       ?ITEM(?MIDI_CTRL_SOUND_CONTROL_6, 'sound-control-6'),
       ?ITEM(?MIDI_CTRL_SOUND_CONTROL_7, 'sound-control-7'),
       ?ITEM(?MIDI_CTRL_SOUND_CONTROL_8, 'sound-control-8'),
       ?ITEM(?MIDI_CTRL_SOUND_CONTROL_9, 'sound-control-9'),
       ?ITEM(?MIDI_CTRL_SOUND_CONTROL_10, 'sound-control-10'),
       ?ITEM(?MIDI_CTRL_GENERAL_PURPOSE_BUTTON_1, 'general-purpose-button-1'),
       ?ITEM(?MIDI_CTRL_GENERAL_PURPOSE_BUTTON_2, 'general-purpose-button-2'),
       ?ITEM(?MIDI_CTRL_GENERAL_PURPOSE_BUTTON_3, 'general-purpose-button-3'),
       ?ITEM(?MIDI_CTRL_GENERAL_PURPOSE_BUTTON_4, 'general-purpose-button-4'),
       ?ITEM(?MIDI_CTRL_EFFECTS_LEVEL, 'effects-level'),
       ?ITEM(?MIDI_CTRL_TREMULO_LEVEL, 'tremulo-level'),
       ?ITEM(?MIDI_CTRL_CHORUS_LEVEL, 'chorus-level'),
       ?ITEM(?MIDI_CTRL_CELESTE_LEVEL, 'celeste-level'),
       ?ITEM(?MIDI_CTRL_PHASER_LEVEL, 'phaser-level'),
       ?ITEM(?MIDI_CTRL_DATA_BUTTON_INCREMENT, 'data-button-increment'),
       ?ITEM(?MIDI_CTRL_DATA_BUTTON_DECREMENT, 'data-button-decrement'),
       ?ITEM(?MIDI_CTRL_NON_REGISTERED_PARAMETER_FINE, 
	     'non-registered-parameter-fine'),
       ?ITEM(?MIDI_CTRL_NON_REGISTERED_PARAMETER, 'non-registered-parameter'),
       ?ITEM(?MIDI_CTRL_REGISTERED_PARAMETER_FINE, 'registered-parameter-fine'),
       ?ITEM(?MIDI_CTRL_REGISTERED_PARAMETER, 'registered-parameter'),
       ?ITEM(?MIDI_CTRL_ALL_SOUND_OFF, 'all-sound-off'),
       ?ITEM(?MIDI_CTRL_ALL_CONTROLLERS_OFF, 'all-controllers-off'),
       ?ITEM(?MIDI_CTRL_LOCAL_KEYBOARD, 'local-keyboard'),
       ?ITEM(?MIDI_CTRL_ALL_NOTES_OFF, 'all-notes-off'),
       ?ITEM(?MIDI_CTRL_OMNI_MODE_OFF, 'omni-mode-off'),
       ?ITEM(?MIDI_CTRL_OMNI_MODE_ON, 'omni-mode-on'),
       ?ITEM(?MIDI_CTRL_MONO_OPERATION, 'mono-operation'),
       ?ITEM(?MIDI_CTRL_POLY_OPERATION, 'poly-operation')
     }.

%% general midi instrument names
gm_midi_map() ->
    #{
       ?ITEM(?GM_MIDI_Acoustic_Grand_Piano, "Acoustic Grand Piano"),
       ?ITEM(?GM_MIDI_Bright_Acoustic_Piano, "Bright Acoustic Piano"),
       ?ITEM(?GM_MIDI_Electric_Grand_Piano, "Electric Grand Piano"),
       ?ITEM(?GM_MIDI_Honky_tonk_Piano, "Honky tonk Piano"),
       ?ITEM(?GM_MIDI_Electric_Piano_1, "Electric Piano 1"),
       ?ITEM(?GM_MIDI_Electric_Piano_2, "Electric Piano 2"),
       ?ITEM(?GM_MIDI_Harpsichord, "Harpsichord"),
       ?ITEM(?GM_MIDI_Clavinet, "Clavinet"),
       ?ITEM(?GM_MIDI_Celesta, "Celesta"),
       ?ITEM(?GM_MIDI_Glockenspiel, "Glockenspiel"),
       ?ITEM(?GM_MIDI_Music_Box, "Music Box"),
       ?ITEM(?GM_MIDI_Vibraphone, "Vibraphone"),
       ?ITEM(?GM_MIDI_Marimba, "Marimba"),
       ?ITEM(?GM_MIDI_Xylophone, "Xylophone"),
       ?ITEM(?GM_MIDI_Tubular_Bells, "Tubular Bells"),
       ?ITEM(?GM_MIDI_Dulcimer, "Dulcimer"),
       ?ITEM(?GM_MIDI_Drawbar_Organ, "Drawbar Organ"),
       ?ITEM(?GM_MIDI_Percussive_Organ, "Percussive Organ"),
       ?ITEM(?GM_MIDI_Rock_Organ, "Rock Organ"),
       ?ITEM(?GM_MIDI_Church_Organ, "Church Organ"),
       ?ITEM(?GM_MIDI_Reed_Organ, "Reed Organ"),
       ?ITEM(?GM_MIDI_Accordion, "Accordion"),
       ?ITEM(?GM_MIDI_Harmonica, "Harmonica"),
       ?ITEM(?GM_MIDI_Tango_Accordion, "Tango Accordion"),
       ?ITEM(?GM_MIDI_Acoustic_Guitar_nylon, "Acoustic Guitar nylon"),
       ?ITEM(?GM_MIDI_Acoustic_Guitar_steel, "Acoustic_Guitar steel"),
       ?ITEM(?GM_MIDI_Electric_Guitar_jazz, "Electric_Guitar jazz"),
       ?ITEM(?GM_MIDI_Electric_Guitar_clean, "Electric_Guitar clean"),
       ?ITEM(?GM_MIDI_Electric_Guitar_muted, "Electric_Guitar muted"),
       ?ITEM(?GM_MIDI_Overdriven_Guitar, "Overdriven Guitar"),
       ?ITEM(?GM_MIDI_Distortion_Guitar, "Distortion Guitar"),
       ?ITEM(?GM_MIDI_Guitar_harmonics, "Guitar harmonics"),
       ?ITEM(?GM_MIDI_Acoustic_Bass, "Acoustic Bass"),
       ?ITEM(?GM_MIDI_Electric_Bass_finger, "Electric Bass finger"),
       ?ITEM(?GM_MIDI_Electric_Bass_pick, "Electric Bass pick"),
       ?ITEM(?GM_MIDI_Fretless_Bass, "Fretless Bass"),
       ?ITEM(?GM_MIDI_Slap_Bass_1, "Slap Bass 1"),
       ?ITEM(?GM_MIDI_Slap_Bass_2, "Slap Bass 2"),
       ?ITEM(?GM_MIDI_Synth_Bass_1, "Synth Bass 1"),
       ?ITEM(?GM_MIDI_Synth_Bass_2, "Synth_Bass 2"),
       ?ITEM(?GM_MIDI_Violin, "Violin"),
       ?ITEM(?GM_MIDI_Viola, "Viola"),
       ?ITEM(?GM_MIDI_Cello, "Cello"),
       ?ITEM(?GM_MIDI_Contrabass, "Contrabass"),
       ?ITEM(?GM_MIDI_Tremolo_Strings, "Tremolo Strings"),
       ?ITEM(?GM_MIDI_Pizzicato_Strings, "Pizzicato Strings"),
       ?ITEM(?GM_MIDI_Orchestral_Harp, "Orchestral Harp"),
       ?ITEM(?GM_MIDI_Timpani, "Timpani"),
       ?ITEM(?GM_MIDI_String_Ensemble_1, "String Ensemble 1"),
       ?ITEM(?GM_MIDI_String_Ensemble_2, "String Ensemble 2"),
       ?ITEM(?GM_MIDI_Synth_Strings_1, "Synth Strings 1"),
       ?ITEM(?GM_MIDI_Synth_Strings_2, "Synth Strings 2"),
       ?ITEM(?GM_MIDI_Choir_Aahs, "Choir Aahs"),
       ?ITEM(?GM_MIDI_Voice_Oohs, "Voice Oohs"),
       ?ITEM(?GM_MIDI_Synth_Voice, "Synth Voice"),
       ?ITEM(?GM_MIDI_Orchestra_Hit, "Orchestra Hit"),
       ?ITEM(?GM_MIDI_Trumpet, "Trumpet"),
       ?ITEM(?GM_MIDI_Trombone, "Trombone"),
       ?ITEM(?GM_MIDI_Tuba, "Tuba"),
       ?ITEM(?GM_MIDI_Muted_Trumpet, "Muted Trumpet"),
       ?ITEM(?GM_MIDI_French_Horn, "French Horn"),
       ?ITEM(?GM_MIDI_Brass_Section, "Brass Section"),
       ?ITEM(?GM_MIDI_Synth_Brass_1, "Synth Brass 1"),
       ?ITEM(?GM_MIDI_Synth_Brass_2, "Synth Brass 2"),
       ?ITEM(?GM_MIDI_Soprano_Sax, "Soprano Sax"),
       ?ITEM(?GM_MIDI_Alto_Sax, "Alto Sax"),
       ?ITEM(?GM_MIDI_Tenor_Sax, "Tenor Sax"),
       ?ITEM(?GM_MIDI_Baritone_Sax, "Baritone Sax"),
       ?ITEM(?GM_MIDI_Oboe, "Oboe"),
       ?ITEM(?GM_MIDI_English_Horn, "English Horn"),
       ?ITEM(?GM_MIDI_Bassoon, "Bassoon"),
       ?ITEM(?GM_MIDI_Clarinet, "Clarinet"),
       ?ITEM(?GM_MIDI_Piccolo, "Piccolo"),
       ?ITEM(?GM_MIDI_Flute, "Flute"),
       ?ITEM(?GM_MIDI_Recorder, "Recorder"),
       ?ITEM(?GM_MIDI_Pan_Flute, "Pan Flute"),
       ?ITEM(?GM_MIDI_Blown_Bottle, "Blown Bottle"),
       ?ITEM(?GM_MIDI_Shakuhachi, "Shakuhachi"),
       ?ITEM(?GM_MIDI_Whistle, "Whistle"),
       ?ITEM(?GM_MIDI_Ocarina, "Ocarina"),
       ?ITEM(?GM_MIDI_Lead_1_square, "Lead 1 square"),
       ?ITEM(?GM_MIDI_Lead_2_sawtooth, "Lead 2 sawtooth"),
       ?ITEM(?GM_MIDI_Lead_3_calliope, "Lead 3 calliope"),
       ?ITEM(?GM_MIDI_Lead_4_chiff, "Lead 4 chiff"),
       ?ITEM(?GM_MIDI_Lead_5_charang, "Lead 5 charang"),
       ?ITEM(?GM_MIDI_Lead_6_voice, "Lead 6 voice"),
       ?ITEM(?GM_MIDI_Lead_7_fifths, "Lead 7 fifths"),
       ?ITEM(?GM_MIDI_Lead_8_bass_plus_lead, "Lead 8 bass plus lead"),
       ?ITEM(?GM_MIDI_Pad_1_new_age, "Pad 1 new age"),
       ?ITEM(?GM_MIDI_Pad_2_warm, "Pad 2 warm"),
       ?ITEM(?GM_MIDI_Pad_3_polysynth, "Pad 3 polysynth"),
       ?ITEM(?GM_MIDI_Pad_4_choir, "Pad 4 choir"),
       ?ITEM(?GM_MIDI_Pad_5_bowed, "Pad 5 bowed"),
       ?ITEM(?GM_MIDI_Pad_6_metallic, "Pad 6 metallic"),
       ?ITEM(?GM_MIDI_Pad_7_halo, "Pad 7 halo"),
       ?ITEM(?GM_MIDI_Pad_8_sweep, "Pad_8_sweep"),
       ?ITEM(?GM_MIDI_FX_1_rain, "FX_1_rain"),
       ?ITEM(?GM_MIDI_FX_2_soundtrack, "FX 2 soundtrack"),
       ?ITEM(?GM_MIDI_FX_3_crystal, "FX 3 crystal"),
       ?ITEM(?GM_MIDI_FX_4_atmosphere, "FX 4 atmosphere"),
       ?ITEM(?GM_MIDI_FX_5_brightness, "FX 5 brightness"),
       ?ITEM(?GM_MIDI_FX_6_goblins, "FX 6 goblins"),
       ?ITEM(?GM_MIDI_FX_7_echoes, "FX 7 echoes"),
       ?ITEM(?GM_MIDI_FX_8_sci_fi, "FX 8 sci fi"),
       ?ITEM(?GM_MIDI_Sitar, "Sitar"),
       ?ITEM(?GM_MIDI_Banjo, "Banjo"),
       ?ITEM(?GM_MIDI_Shamisen, "Shamisen"),
       ?ITEM(?GM_MIDI_Koto, "Koto"),
       ?ITEM(?GM_MIDI_Kalimba, "Kalimba"),
       ?ITEM(?GM_MIDI_Bag_pipe, "Bag pipe"),
       ?ITEM(?GM_MIDI_Fiddle, "Fiddle"),
       ?ITEM(?GM_MIDI_Shanai, "Shanai"),
       ?ITEM(?GM_MIDI_Tinkle_Bell, "Tinkle Bell"),
       ?ITEM(?GM_MIDI_Agogo, "Agogo"),
       ?ITEM(?GM_MIDI_Steel_Drums, "Steel Drums"),
       ?ITEM(?GM_MIDI_Woodblock, "Woodblock"),
       ?ITEM(?GM_MIDI_Taiko_Drum, "Taiko Drum"),
       ?ITEM(?GM_MIDI_Melodic_Tom, "Melodic Tom"),
       ?ITEM(?GM_MIDI_Synth_Drum, "Synth Drum"),
       ?ITEM(?GM_MIDI_Reverse_Cymbal, "Reverse Cymbal"),
       ?ITEM(?GM_MIDI_Guitar_Fret_Noise, "Guitar Fret Noise"),
       ?ITEM(?GM_MIDI_Breath_Noise, "Breath Noise"),
       ?ITEM(?GM_MIDI_Seashore, "Seashore"),
       ?ITEM(?GM_MIDI_Bird_Tweet, "Bird Tweet"),
       ?ITEM(?GM_MIDI_Telephone_Ring, "Telephone Ring"),
       ?ITEM(?GM_MIDI_Helicopter, "Helicopter"),
       ?ITEM(?GM_MIDI_Applause, "Applause"),
       ?ITEM(?GM_MIDI_Gunshot, "Gunshot")
     }.

%% general midi drum names
gm_drum_map() ->
    #{
       ?ITEM(?GM_DRUM_Bass_Drum_2, "Bass_Drum_2"),
       ?ITEM(?GM_DRUM_Bass_Drum_1, "Bass_Drum_1"),
       ?ITEM(?GM_DRUM_Side_Stick, "Side_Stick"),
       ?ITEM(?GM_DRUM_Snare_Drum_1, "Snare_Drum_1"),
       ?ITEM(?GM_DRUM_Hand_Clap, "Hand_Clap"),
       ?ITEM(?GM_DRUM_Snare_Drum_2, "Snare_Drum_2"),
       ?ITEM(?GM_DRUM_Low_Tom_2, "Low_Tom_2"),
       ?ITEM(?GM_DRUM_Closed_Hi_hat, "Closed_Hi_hat"),
       ?ITEM(?GM_DRUM_Low_Tom_1, "Low_Tom_1"),
       ?ITEM(?GM_DRUM_Pedal_Hi_hat, "Pedal_Hi_hat"),
       ?ITEM(?GM_DRUM_Mid_Tom_2, "Mid_Tom_2"),
       ?ITEM(?GM_DRUM_Open_Hi_hat, "Open_Hi_hat"),
       ?ITEM(?GM_DRUM_Mid_Tom_1, "Mid_Tom_1"),
       ?ITEM(?GM_DRUM_High_Tom_2, "High_Tom_2"),
       ?ITEM(?GM_DRUM_Crash_Cymbal_1, "Crash_Cymbal_1"),
       ?ITEM(?GM_DRUM_High_Tom_1, "High_Tom_1"),
       ?ITEM(?GM_DRUM_Ride_Cymbal_1, "Ride_Cymbal_1"),
       ?ITEM(?GM_DRUM_Chinese_Cymbal, "Chinese_Cymbal"),
       ?ITEM(?GM_DRUM_Ride_Bell, "Ride_Bell"),
       ?ITEM(?GM_DRUM_Tambourine, "Tambourine"),
       ?ITEM(?GM_DRUM_Splash_Cymbal, "Splash_Cymbal"),
       ?ITEM(?GM_DRUM_Cowbell, "Cowbell"),
       ?ITEM(?GM_DRUM_Crash_Cymbal_2, "Crash_Cymbal_2"),
       ?ITEM(?GM_DRUM_Vibra_Slap, "Vibra_Slap"),
       ?ITEM(?GM_DRUM_Ride_Cymbal_2, "Ride_Cymbal_2"),
       ?ITEM(?GM_DRUM_High_Bongo, "High_Bongo"),
       ?ITEM(?GM_DRUM_Low_Bongo, "Low_Bongo"),
       ?ITEM(?GM_DRUM_Mute_High_Conga, "Mute_High_Conga"),
       ?ITEM(?GM_DRUM_Open_High_Conga, "Open_High_Conga"),
       ?ITEM(?GM_DRUM_Low_Conga, "Low_Conga"),
       ?ITEM(?GM_DRUM_High_Timbale, "High_Timbale"),
       ?ITEM(?GM_DRUM_Low_Timbale, "Low_Timbale"),
       ?ITEM(?GM_DRUM_High_Agogo, "High_Agogo"),
       ?ITEM(?GM_DRUM_Low_Agogo, "Low_Agogo"),
       ?ITEM(?GM_DRUM_Cabasa, "Cabasa"),
       ?ITEM(?GM_DRUM_Maracas, "Maracas"),
       ?ITEM(?GM_DRUM_Short_Whistle, "Short_Whistle"),
       ?ITEM(?GM_DRUM_Long_Whistle, "Long_Whistle"),
       ?ITEM(?GM_DRUM_Short_Guiro, "Short_Guiro"),
       ?ITEM(?GM_DRUM_Long_Guiro, "Long_Guiro"),
       ?ITEM(?GM_DRUM_Claves, "Claves"),
       ?ITEM(?GM_DRUM_High_Wood_Block, "High_Wood_Block"),
       ?ITEM(?GM_DRUM_Low_Wood_Block, "Low_Wood_Block"),
       ?ITEM(?GM_DRUM_Mute_Cuica, "Mute_Cuica"),
       ?ITEM(?GM_DRUM_Open_Cuica, "Open_Cuica"),
       ?ITEM(?GM_DRUM_Mute_Triangle, "Mute_Triangle"),
       ?ITEM(?GM_DRUM_Open_Triangle, "Open_Triangle")
     }.
    
control_decode(Control) when is_integer(Control) ->
    case maps:find(Control, control_map()) of
	{ok,Value} -> Value;
	error -> Control
    end.

control_encode(Control) when is_integer(Control) -> Control;
control_encode(Control) when is_atom(Control) ->
    case maps:find(Control, control_map()) of
	{ok,Value} -> Value
    end.

note_decode(Note) when is_integer(Note) ->
    Octave = Note div 12,
    N = element((Note rem 12)+1, {"C", "C#", "D", "D#", "E", "F",
				  "F#", "G", "G#", "A", "A#", "B"}),
    if Octave =:= 0 -> N;
       true -> N++integer_to_list(Octave-1)
    end.

note_encode("C") -> 0;
note_encode("C#") -> 1;
note_encode("D") -> 2;
note_encode("D#") -> 3;
note_encode("E") -> 4;
note_encode("F") -> 5;
note_encode("F#") -> 6;
note_encode("G") -> 7;
note_encode("G#") -> 8;
note_encode("A") -> 9;
note_encode("A#") -> 10;
note_encode("B") -> 11;
note_encode([$C,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+0;
note_encode([$C,$#,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+1;
note_encode([$D,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+2;
note_encode([$D,$#,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+3;
note_encode([$E,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+4;
note_encode([$F,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+5;
note_encode([$F,$#,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+6;
note_encode([$G,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+7;
note_encode([$G,$#,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+8;
note_encode([$A,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+9;
note_encode([$A,$#,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+10;
note_encode([$B,I]) when I>=$0, I=<$9 -> 12*(I-$0+1)+11.
