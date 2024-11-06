%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2018, Tony Rogvall
%%% @doc
%%%    encode/decode midi commands
%%% @end
%%% Created :  1 Jan 2018 by Tony Rogvall <tony@rogvall.se>

-module(midi_codec).

-export([encode/1, decode/1, decode/2]).
-export([init/0, init/1, init/2, scan/1, scan/2]).
-export([scan_delta/1]).
-export([event_decode/2, event_encode/1]).
-export([events_encode/1]).
-export([control_decode/1, control_encode/1]).
-export([meta_decode/1, meta_encode/2]).
-export([length_decode/1, length_encode/1]).
-export([note_decode/1, note_encode/1]).
-export([gm_midi_map/0, gm_drum_map/0]).

-include("../include/midi.hrl").

-define(DEBUG, true).

-ifdef(DEBUG).
-define(dbg(F), io:format((F))).
-define(dbg(F, A), io:format((F), A)).
-else.
-define(dbg(F), ok).
-define(dbg(F, A), ok).
-endif.


-record(s, 
	{
	 state = status :: status | params | params_eox | 
			   data | meta | sys_f0 | sys_f7 | vl,
	 buf  = <<>> :: binary(),
	 status  = 0 :: 16#f8..16#ff,
	 running = 0 :: 16#f8..16#ff,
	 len = 0,  %% number of parameters or number of bytes
	 params = [] :: [0..127],
	 midi_file = false :: boolean(),  %% parse from file or midi stream
	 callback = undefined :: undefined | fun((#sys{}) -> ok)
	}).

%% assume decode from file!
decode(Data) ->
    decode(init(), Data).

%% continue decode after more is detected with more data
decode(S=#s{buf=Buf}, Data) ->
    Buf1 = <<Buf/binary,(iolist_to_binary(Data))/binary>>,
    decode_(S#s{buf=Buf1}, []).

decode_(S,Acc) ->
    case scan(S) of
	{more,S1}  ->
	    {more,S1,Acc};
	{eot,_S1} ->
	    {ok,lists:reverse(Acc)};
	{{Status,Params},S1} ->
	    decode_(S1,[event_decode(Status,Params) | Acc])
    end.

encode(List) when is_list(List) ->
    events_encode(List);
encode(Event) when is_tuple(Event) ->
    event_encode(Event).

init() ->
    init(false,<<>>).
init(Data) when is_binary(Data) ->
    init(false,Data);
init(MidiFile) when is_boolean(MidiFile) ->
    init(MidiFile,<<>>).
init(MidiFile,Data) when is_boolean(MidiFile), is_binary(Data) ->
    #s { midi_file=MidiFile, buf = Data }.

scan(St=#s{state=status,buf=(<<>>)}) ->
    {eot,St};
scan(St=#s{buf=(<<>>)}) ->
    {more,St};
scan(S=#s{state=State,buf=Data}) ->
    parse(State,Data,S).

%% continue - add chars to input buffer and continue scan
scan(Chars, S=#s{buf=Buf}) ->
    Buf1 = <<Buf/binary,(iolist_to_binary(Chars))/binary>>,
    scan(S#s{buf=Buf1}).

parse(data, Data, S=#s{len=0}) ->
    Event = {S#s.status, lists:reverse(S#s.params)},
    {Event, S#s{state=status,buf=Data,status=0,params=[]}};
parse(data, <<B,Data/binary>>, S=#s{len=Len}) ->
    parse(data, Data, S#s { len=Len-1, params=[B|S#s.params] });

parse(Status, Data=(<<B,Data1/binary>>), S) ->
    ?dbg("parse ~w: ~w [s=~1000p]\n",[Status,Data,S]),
    if B >= 16#f8, B =< 16#ff -> %% real time (fixme: check file scan)
	    if B =:= 16#ff, S#s.midi_file =:= true ->
		    parse(meta,Data1,S#s{len=0,status=B});
	       true ->
		    real_time_event(B, S),
		    parse(Status, Data1, S)
	    end;
       true ->
	    parse_(Status, Data, S)
    end;
parse(status, <<>>, S) ->
    {eot, S#s{state=status}};
parse(Status, <<>>, S) ->
    {more, S#s{state=Status}}.

parse_(status, Data=(<<B,Data1/binary>>), S) ->
    if B band 16#80 =:= 0 ->
	    parse_status_(S#s.running, Data, S);
       B band 16#f0 =:= 16#f0 -> %% sys
	    if B =< 16#f7 -> %% non real time
		    parse_status_(B, Data1, S#s{running = 0 });
	       true ->
		    parse_status_(B, Data1, S#s{running = 0 })
	    end;
       true ->
	    parse_status_(B, Data1, S#s{running=B, status=B})
    end;
parse_(params, _Data=(<<B,Data1/binary>>), S) ->
    ?dbg("parse(params) ~w [s=~1000p]\n",[_Data,S]),
    Len = S#s.len,
    Ps = S#s.params,
    case Len - 1 of
	0 ->
	    event(Data1, S#s { len = 0, params = [B|Ps] });
	Len1 ->
	    parse(params, Data1, S#s { len=Len1, params=[B|Ps] })
    end;
parse_(params_eox, Data=(<<B,Data1/binary>>), S) ->
    if B band 16#80 =/= 0 ->
	    if B =:= 16#f7 ->
		    event(Data1, S);
	       true ->
		    event(Data, S)
	    end;
       true ->
	    Ps = S#s.params,
	    parse(params_eox, Data1, S#s { params = [B|Ps] })
    end;
parse_(meta, <<Meta,Data1/binary>>, S) ->
    parse(vl, Data1, S#s { len=0, params = [Meta] });
parse_(sys_f0, Data, S) ->
    parse(vl, Data, S#s { len=0, params = [] });
parse_(sys_f7, Data, S) ->
    parse(vl, Data, S#s { len=0, params = [] });
parse_(vl,<<0:1,Len:7,Data/binary>>,S) ->
    parse(data, Data, S#s{ len=Len });
parse_(vl,<<1:1,L0:7,0:1,L1:7,Data/binary>>,S) ->
    Len = (L0 bsl 7) bor L1,
    parse(data, Data, S#s{ len=Len });
parse_(vl,<<1:1,L0:7,1:1,L1:7,0:1,L2:7,Data/binary>>, S) ->
    Len = (L0 bsl 14) bor (L1 bsl 7) bor L2,
    parse(data, Data, S#s { len=Len });
parse_(vl,<<1:1,L0:7,1:1,L1:7,1:1,L2:7,0:1,L3:7,Data/binary>>,S) ->
    Len = (L0 bsl 21) bor (L1 bsl 14) bor (L2 bsl 7) bor L3,
    parse(data, Data, S#s { len=Len });
parse_(vl,<<_,_,_,_,_Data/binary>>,S) ->
    {{error,bad_parameter_length},S}.

%% Status = 2#1xxx_vvvv
parse_status_(Status, Data, S) ->
    ?dbg("parse_status_ ~w: ~w [s=~1000p]\n",[Status,Data,S]),
    case Status bsr 4 of
	?MIDI_EVENT_NOTEOFF ->
	    parse(params, Data, S#s { len=2, status=Status });
	?MIDI_EVENT_NOTEON ->
	    parse(params, Data, S#s { len=2, status=Status });
	?MIDI_EVENT_AFTERTOUCH ->
	    parse(params, Data, S#s { len=2, status=Status });
	?MIDI_EVENT_CONTROLCHANGE ->
	    parse(params, Data, S#s { len=2, status=Status });
	?MIDI_EVENT_PITCHBEND ->
	    parse(params, Data, S#s { len=2, status=Status });
	?MIDI_EVENT_PROGRAMCHANGE ->
	    parse(params, Data, S#s { len=1, status=Status });
	?MIDI_EVENT_PRESSURE ->
	    parse(params, Data, S#s { len=1, status=Status });
	?MIDI_EVENT_SYS ->
	    case Status band 16#0f of
		0  ->
		    if S#s.midi_file =:= true ->
			    parse(sys_f0, Data, S#s { len=0, status=Status });
		       true ->
			    parse(params_eox, Data, S#s { len=0, status=Status})
		    end;
		1  -> parse(params,Data,S#s {len=1, status=Status });
		2  -> parse(params,Data,S#s {len=2, status=Status });
		3  -> parse(params,Data,S#s {len=1, status=Status });
		4  -> parse(status,Data,S);
		5  -> parse(status,Data,S);
		6  -> parse(params,Data,S#s {len=0, status=Status});
		7  ->
		    if S#s.midi_file =:= true ->
			    parse(sys_f7, Data, S#s {len=0, status=Status });
		       true ->
			    parse(status,Data,S)
		    end;
		15 ->
		    if S#s.midi_file =:= true ->
			    parse(meta,Data,S#s{len=0,status=Status});
		       true ->
			    real_time_event(15, S),
			    parse(status,Data,S)
		    end;
		Sys ->
		    real_time_event(Sys, S),
		    parse(status,Data,S)
	    end
    end.

event(Data, S) ->
    {{S#s.status, lists:reverse(S#s.params)},
     S#s{state=status,buf=Data,status=0,params=[]}}.

real_time_event(Sys, S) ->
    Event = sys_decode(Sys, undefined),
    if is_function(S#s.callback) ->
	    (S#s.callback)(Event);
       true ->
	    ?dbg("realtime event: ~p\n", [Event])
    end.


scan_delta(S=#s{state=status,buf=(<<>>)}) ->
    {eot, S};
scan_delta(S=#s{state=status,buf=Buf}) ->
    parse_delta(Buf,S).

parse_delta(<<0:1,L0:7,Data/binary>>,S) ->
    Len = L0,
    {{ok,Len}, S#s{buf=Data}};
parse_delta(<<1:1,L0:7,0:1,L1:7,Data/binary>>,S) ->
    Len = (L0 bsl 7) bor L1,
    {{ok,Len}, S#s{buf=Data}};
parse_delta(<<1:1,L0:7,1:1,L1:7,0:1,L2:7,Data/binary>>, S) ->
    Len = (L0 bsl 14) bor (L1 bsl 7) bor L2,
    {{ok,Len}, S#s{buf=Data}};
parse_delta(<<1:1,L0:7,1:1,L1:7,1:1,L2:7,0:1,L3:7,Data/binary>>,S) ->
    Len = (L0 bsl 21) bor (L1 bsl 14) bor (L2 bsl 7) bor L3,
    {{ok,Len}, S#s{buf=Data}};
parse_delta(<<_,_,_,_,_/binary>>, S) ->
    {{error,bad_delta}, S}.


events_encode(Es) when is_list(Es) ->
    events_encode(<<>>,Es,[]);    
events_encode(E) when is_tuple(E) ->
    events_encode(<<>>,[E],[]).

events_encode(Run,[E|Es],Acc) ->
    {Run1,Acc1} = event_encode(E,Run,Acc),
    events_encode(Run1,Es,Acc1);
events_encode(_Run,[],Acc) ->
    list_to_binary(lists:reverse(Acc)).

event_encode(E) when is_tuple(E) ->
    {_,[List]} = event_encode(E,<<>>,[]),
    iolist_to_binary(List).


event_encode(#note_off{chan=Chan,note=Note,velocity=Velocity},Run,Acc) ->
    event_encode_(<<?MIDI_EVENT_NOTEOFF:4,Chan:4>>,
		  Run,
		  <<0:1, Note:7, 
		    0:1, Velocity:7>>, 
		  Acc);
event_encode(#note_on{chan=Chan,note=Note,velocity=Velocity},Run,Acc) ->
    event_encode_(<<?MIDI_EVENT_NOTEON:4,Chan:4>>,
		  Run,
		  <<0:1, Note:7,
		    0:1, Velocity:7>>, 
		  Acc);

event_encode(#after_touch{chan=Chan, note=B, value=C},Run,Acc) ->
    event_encode_(<<?MIDI_EVENT_AFTERTOUCH:4,Chan:4>>,
		  Run,
		  <<0:1,B:7,
		    0:1,C:7>>, 
		  Acc);
event_encode(#control_change{chan=Chan,control=Control,param=Param},Run,Acc) ->
    event_encode_(<<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4>>,
		  Run,
		  <<0:1,(control_encode(Control)):7,
		    0:1,Param:7>>, 
		  Acc);
event_encode(#pitch_bend{chan=Chan,bend=Bend},Run,Acc) ->
    Bend1 = clamp(Bend,-8182,8191) + 16#2000,
    event_encode_(<<?MIDI_EVENT_PITCHBEND:4,Chan:4>>,
		  Run,
		  <<0:1,Bend1:7,
		    0:1,(Bend1 bsr 7):7>>,
		  Acc);
event_encode(#program_change{chan=Chan,prog=Prog},Run,Acc) ->
    event_encode_(<<?MIDI_EVENT_PROGRAMCHANGE:4,Chan:4>>,
		  Run,
		  <<0:1,Prog:7>>,
		  Acc);
event_encode(#pressure{chan=Chan,pressure=Pressure},Run,Acc) ->
    event_encode_(<<?MIDI_EVENT_PRESSURE:4,Chan:4>>,
		  Run,
		  <<0:1,Pressure:7>>,
		  Acc);
event_encode(#sys{type=Type,params=Params},Run,Acc) ->
    case Type of
	ex ->      sys_event_encode(0,Params,Run,Acc);
	sys1 ->    sys_event_encode(1,Params,Run,Acc);
	song_position_pointer ->
	    sys_event_encode(2,Params,Run,Acc);
	song_select ->
	    sys_event_encode(3,Params,Run,Acc);
	sys4 ->    sys_event_encode(4,Params,Run,Acc);
	sys5 ->    sys_event_encode(5,Params,Run,Acc);
	tune_request ->
	    sys_event_encode(6,Params,Run,Acc);
	eox -> sys_event_encode(7,Params,Run,Acc);
	timing_clock ->
	    sys_event_encode(8,Params,Run,Acc);
	sys9 -> sys_event_encode(9,Params,Run,Acc);
	start -> sys_event_encode(10,Params,Run,Acc);
	continue -> sys_event_encode(11,Params,Run,Acc);
	stop -> sys_event_encode(12,Params,Run,Acc);
	sys13 -> sys_event_encode(13,Params,Run,Acc);
	active_sensing -> sys_event_encode(14,Params,Run,Acc)
    end;
event_encode(eox, Run, Acc) ->
    sys_event_encode(7,[],Run,Acc);
event_encode(#meta{type=Type,params=Data}, Run, Acc) ->
    case Type of
	text -> meta_encode(?MIDI_META_TEXT,Data,Run,Acc);
	copyright ->
	    meta_encode(?MIDI_META_COPYRIGHT,Data,Run,Acc);
	track_name ->
	    meta_encode(?MIDI_META_TRACKNAME,Data,Run,Acc);
	instrument ->
	    meta_encode(?MIDI_META_INSTRUMENT,Data,Run,Acc);
	lyric ->
	    meta_encode(?MIDI_META_LYRIC,Data,Run,Acc);
	marker ->
	    meta_encode(?MIDI_META_MARKER,Data,Run,Acc);
	cue_point ->
	    meta_encode(?MIDI_META_CUE_POINT,Data,Run,Acc);
	program_name ->
	    meta_encode(?MIDI_META_PROGRAM_NAME,Data,Run,Acc);
	device_name ->
	    meta_encode(?MIDI_META_DEVICE_NAME,Data,Run,Acc);
	midi_channel ->
	    Channel = Data band 16#7f,
	    meta_encode(?MIDI_META_MIDI_CHANNEL,[Channel],Run,Acc);
	midi_port ->
	    Port = Data band 16#7f,
	    meta_encode(?MIDI_META_MIDI_PORT,[Port],Run,Acc);
	end_of_track ->
	    meta_encode(?MIDI_META_END_OF_TRACK,[],Run,Acc);
	tempo ->
	    <<T2,T1,T0>> = <<(trunc(Data)):24>>,
	    %% fixme: encode tempo 7 bit?
	    meta_encode(?MIDI_META_TEMPO,[T2,T1,T0],Run,Acc);
	smpte_offset ->
	    <<HR,MN,SE,FR,FF>> = Data,
	    meta_encode(?MIDI_META_SMPTE_OFFSET,[HR,MN,SE,FR,FF],Run,Acc);
	time_signature ->
	    [NN,DD,CC,BB] = Data,
	    DDi = trunc(math:log2(DD)),
	    meta_encode(?MIDI_META_TIME_SIGNATURE,[NN,DDi,CC,BB],Run,Acc);
	key_signature ->
	    [SF,MI] = Data,
	    meta_encode(?MIDI_META_KEY_SIGNATURE,[SF,MI],Run,Acc);
	proprietary ->
	    meta_encode(?MIDI_META_PROPRIETARY,Data,Run,Acc);
	_ when is_integer(Type) ->
	    meta_encode(Type,Data,Run,Acc)
    end.

meta_encode(Meta, Params) ->
    {_, [List]} = meta_encode(Meta, Params, <<>>, []),
    iolist_to_binary(List).

meta_encode(Meta, Params, Run, Acc) ->
    LCode = length_encode(length(Params)),
    Bin = << <<P>> || P <- Params >>,
    event_encode_(<<?MIDI_EVENT_SYS:4,15:4>>, 
		  Run,
		  <<0:1,Meta:7,LCode/binary,Bin/binary>>,
		  Acc).

sys_event_encode(Sys, Params, Run, Acc) ->
    Params1 = if Params =:= undefined -> []; true -> Params end,
    Bin = << <<0:1,P:7>> || P <- Params1 >>,
    event_encode_(<<?MIDI_EVENT_SYS:4,Sys:4>>,
		  Run,
		  Bin,
		  Acc).

event_encode_(Status,Status,Params,Acc) ->
    {Status, [Params | Acc]};
event_encode_(Status,_Run,Params,Acc) ->
    {Status, [[Status,Params] | Acc]}.


length_encode(L) when L >= 0, L =< 16#0fffffff ->
    length_encode_(L).

length_encode_(L) when L < 16#80 -> 
    <<0:1,L:7>>;
length_encode_(L) when L < 16#4000 ->
    <<1:1,(L bsr 7):7,0:1,L:7>>;
length_encode_(L) when L < 16#200000 ->
    <<1:1,(L bsr 14):7,1:1,(L bsr 7):7,0:1,L:7>>;
length_encode_(L) when L < 16#10000000 ->
    <<1:1,(L bsr 21):7, 1:1,(L bsr 14):7,1:1,(L bsr 7):7, 0:1,L:7>>.

length_decode(<<0:1,L0:7>>) ->
    L0;
length_decode(<<1:1,L0:7,0:1,L1:7>>) ->
    (L0 bsl 7) + L1;
length_decode(<<1:1,L0:7,1:1,L1:7,0:1,L2:7>>) ->
    (L0 bsl 14) + (L1 bsl 7) + L2;
length_decode(<<1:1,L0:7,1:1,L1:7,1:1,L2:7,0:1,L3:7>>) ->
    (L0 bsl 21) + (L1 bsl 14) + (L2 bsl 7) + L3.

%% translate status and params into symbolic midi event form
event_decode(Status,Params=[B,C]) ->
    case Status bsr 4 of
	?MIDI_EVENT_NOTEOFF ->
	    #note_off{chan=Status band 16#0F, note=B, velocity=C};
	?MIDI_EVENT_NOTEON ->
	    #note_on{chan=Status band 16#0F, note=B, velocity=C};
	?MIDI_EVENT_AFTERTOUCH ->
	    #after_touch{chan=Status band 16#0F, note=B, value=C};
	?MIDI_EVENT_CONTROLCHANGE ->
	    #control_change{chan=Status band 16#0F, control=control_decode(B),
			    param=C};
	?MIDI_EVENT_PITCHBEND ->
	    #pitch_bend{chan=Status band 16#0F, 
			bend=((C bsl 7) bor B) - 16#2000};
	?MIDI_EVENT_SYS ->
	    sys_decode(Status, Params)
    end;
event_decode(Status,Params=[B]) ->
    case Status bsr 4 of
	?MIDI_EVENT_PROGRAMCHANGE ->
	    #program_change{chan=Status band 16#0F, prog=B};
	?MIDI_EVENT_PRESSURE ->
	    #pressure{chan=Status band 16#0F, pressure=B};
	?MIDI_EVENT_SYS ->
	    sys_decode(Status, Params)
    end;
event_decode(Status,Params) ->
    case Status bsr 4 of
	?MIDI_EVENT_SYS ->
	    sys_decode(Status, Params)
    end.

sys_decode(Sys, Params) ->
    case Sys band 16#0f of
	%% non-real time (ex have some real time as well...)
	0 -> #sys{type=ex,params=Params};
	1 -> #sys{type=sys1,params=Params};
	2 -> #sys{type=song_position_pointer,params=Params};
	3 -> #sys{type=song_select,params=Params};
	4 -> #sys{type=sys4,params=Params};
	5 -> #sys{type=sys5,params=Params};
	6 -> #sys{type=tune_request,params=Params};
	7 -> #sys{type=eox,params=Params};
	%% real time
	8 -> #sys{type=timing_clock,params=Params};
	9 -> #sys{type=sys9,params=Params};
	10 -> #sys{type=start,params=Params};
	11 -> #sys{type=continue,params=Params};
	12 -> #sys{type=stop,params=Params};
	13 -> #sys{type=sys13,params=Params};
	14 -> #sys{type=active_sensing,params=Params};
	15 -> 
	    if Params =:= undefined; Params =:= [] ->
		    #sys{type=system_reset};
	       true ->
		    meta_decode(Params)
	    end
    end.

meta_decode([Meta|Params]) ->
    case Meta of
	?MIDI_META_TEXT      -> #meta{type=text,params=Params};  %% text
	?MIDI_META_COPYRIGHT -> #meta{type=copyright,params=Params};  %% text
	?MIDI_META_TRACKNAME -> #meta{type=track_name,params=Params};   %% text
	?MIDI_META_INSTRUMENT -> #meta{type=instrument,params=Params};  %% text
	?MIDI_META_LYRIC -> #meta{type=lyric,params=Params};  %% text
	?MIDI_META_MARKER -> #meta{type=marker,params=Params};  %% text
	?MIDI_META_CUE_POINT -> #meta{type=cue_point,params=Params};  %% text
	?MIDI_META_PROGRAM_NAME -> #meta{type=program_name,params=Params};  %% text
	?MIDI_META_DEVICE_NAME -> #meta{type=device_name,params=Params};  %% text
	?MIDI_META_MIDI_CHANNEL -> #meta{type=midi_channel,params=Params}; %% uint8
	?MIDI_META_MIDI_PORT -> #meta{type=midi_port,params=Params};  %% uint8
	?MIDI_META_END_OF_TRACK -> #meta{type=end_of_track,params=[]}; %% -
	?MIDI_META_TEMPO -> %% uint24/big
	    case Params of
		[T2,T1,T0] -> #meta{type=tempo,params=(T2 bsl 16)+(T1 bsl 8)+T0}
	    end;
	?MIDI_META_SMPTE_OFFSET -> %% <<HR:8,MN:8,SE:8,FR:8,FF:8>>
	    case Params of
		[HR,MN,SE,FR,FF] ->
		    #meta{type= smpte_offset,params=[HR,MN,SE,FR,FF]}
	    end;
	?MIDI_META_TIME_SIGNATURE -> %% <<NN:8,DD:8,CC:8,BB:8>>
	    case Params of
		[NN,DD,CC,BB] ->
		    #meta{type=time_signature,params=[NN,(1 bsl DD),CC,BB]}
	    end;
	?MIDI_META_KEY_SIGNATURE -> %% <<SF:8,MI:8>>
	    case Params of
		[SF,MI] -> 
		    #meta{type= key_signature,params=[SF,MI]}
	    end;
	?MIDI_META_PROPRIETARY ->
	    #meta{type= proprietary,params=Params};
	_ ->
	    #meta{type=Meta,params=Params}
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

clamp(X, Min, _Max) when X < Min -> Min;
clamp(X, _Min, Max) when X > Max -> Max;
clamp(X, _, _) -> X.
