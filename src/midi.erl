%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    midi driver
%%% @end
%%% Created : 29 Dec 2017 by Tony Rogvall <tony@rogvall.se>

-module(midi).

-on_load(init/0).
-export([close/1, read/1, write/2]).
-export([note_on/4, note_off/4, note_off/3, 
	 pitch_bend/3, pressure/3,
	 all_off/2, reset_all/2,
	 program_change/3]).
-export([pan/3, pan2/3]).
-export([bank/3, bank2/3]).
-export([volume/3, volume2/3]).
-export([control7/4, control14/5]).
-export([nrpn/4]).

-export([expression/3, expression2/3]).

-export([start/0, start/1, stop/0]).
-export([setup_synth/0, open_synth/0]).
-export([with_device/2, open_device/1]).
-export([devices/0]).
-export([find_synth_device/2]).
-export([find_device_by_port/2]).
-export([find_device_by_name/2]).
-export([find_synth_input_port/1]).

-export([io_prog/1]).
-export([proxy/2]).
-export([shared_input/1]).
-export([tparam/2]).
-export([tparam_set_tempo/2]).
-export([tparam_set_time_signature/2]).

-export([device_inquiry/1, device_inquiry/2]).
-export([request_device_inquiry/1, request_device_inquiry/2]).
-export([read_sys/1, read_sys/2]).
-export([recv_sys/1, recv_sys/2]).
-export([flush/1]).
%% nifs
-export([open/2, close_/1, read_/1, write_/2, select_/2]).
-export([backend/0, synth/0]).

%% util
-export([play_file/1]).

-type handle() :: reference().
-type mode() :: read | write | read_write.
-type chan() :: 0..15.
-type note() :: 0..127.
-type velocity() :: 0..127.

-define(DEFAULT_MIDI_SYNTH, midi_fluid).
-define(DEFAULT_MIDI_BACKEND, midi_alsa).

-define(is_handle(H), is_reference((H))).

-ifdef(DEBUG).
-define(dbg(F), io:format((F))).
-define(dbg(F, A), io:format((F), A)).
-else.
-define(dbg(F), ok).
-define(dbg(F, A), ok).
-endif.

-include("../include/midi.hrl").

-define(nif_stub(),
	erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

init() ->
    Nif = filename:join(code:priv_dir(midi), "midi_alsa"),
    erlang:load_nif(Nif, 0).

start() ->
    case os:cmd("lsmod | grep snd_virmidi") of
	[] ->
	    error(no_snd_virmidi);
	_ ->
	    application:ensure_all_started(midi)
    end.

start([File]) when is_atom(File) ->
    start(),
    play_file(File).

play_file(Filename) when is_atom(Filename) ->
    play_file(atom_to_list(Filename));
play_file(Filename) when is_list(Filename) ->    
    case filename:extension(Filename) of
	".mid" ->
	    midi_play:file(Filename);
	".abc" ->
	    midi_abc:play_file(Filename)
    end.    


stop() ->
    application:stop(midi).

%% open midi device
open(_DeviceName, _OptionList) ->
    ?nif_stub().

% close midi device
close(Name) when is_atom(Name) ->
    close_(midi_reg:whereis(Name));
close(Handle) when ?is_handle(Handle) ->
    ?dbg("close\n"),
    flush(Handle),
    close_(Handle).

close_(_Handle) ->
    ?nif_stub().

%% write "blocking" data
-spec write(Handle::atom()|handle(), Data::iolist()) ->
	  {ok, Written::integer()} | {error, term()}.

write(Name,Data) when is_atom(Name) ->
    write(midi_reg:whereis(Name),iolist_to_binary(Data), 0);
write(Handle,Data) ->
    write(Handle,iolist_to_binary(Data),0).

-spec write(Handle::atom()|handle(), Data::binary(), SoFar::integer()) ->
	  {ok, Written::integer()} | {error, term()}.

write(Handle, Bin, SoFar) ->
    Size = byte_size(Bin),
    case write_(Handle, Bin) of    
	{ok, Written} ->
	    if Written =:= Size ->
		    {ok, Written+SoFar};
	       Written =:= 0 ->
		    {ok, SoFar};
	       true ->
		    <<_:Written/binary, Bin1/binary>> = Bin,
		    write(Handle, Bin1, SoFar+Written)
	    end;
	{error, eagain} ->
	    ?dbg("eagain\n"),
	    ok = select_(Handle, write),
	    receive
		{select,Handle,undefined,_Ready} ->
		    write(Handle, Bin, SoFar)
	    end;
	{error, Error} ->
	    {error, Error}
    end.

-spec write_(Handle::handle(), Data::iolist()) ->
	  {ok, Written::integer()} | {error, eagain} | {error, term()}.

write_(_Handle, _Data) ->
    ?nif_stub().

-spec select_(handle(), mode()) -> ok | {error, term()}.

select_(_Handle, _Mode) ->
    ?nif_stub().

read(Name) when is_atom(Name) ->
    read_(midi_reg:whereis(Name));
read(Handle) ->
    read_(Handle).

-spec read_(Handle::handle()) ->
	  {ok,NumEvents::integer()} |
	  {ok,Data::binary()} |
	  {error, eagain} |
	  {error, term()}.

read_(_Handle) ->
    ?nif_stub().

-spec note_on(Synth::handle(), Chan::chan(), Note::note(),
	      Velocity::velocity()) ->
	  {ok,Written::integer()} | {error, Reason::term()}.
note_on(Synth, Chan, Note, Velocity) ->
    write(Synth, <<?MIDI_EVENT_NOTEON:4,Chan:4,0:1,Note:7,0:1,Velocity:7>>).

%% note! using ?MIDI_NOTE_ON(Chan), Note, 0>> 
%% may save a byte in the running status when stopping a chord
note_off(Synth, Chan, Note) ->
    write(Synth, <<?MIDI_EVENT_NOTEON:4,Chan:4,0:1,Note:7,0>>).
note_off(Synth, Chan, Note, Velocity) ->
    write(Synth, <<?MIDI_EVENT_NOTEOFF:4,Chan:4,0:1,Note:7,
		   0:1,Velocity:7>>).

volume(Synth, Chan, Volume) ->
    control7(Synth, Chan, 
	     ?MIDI_CTRL_VOLUME,
	     unsigned7(Volume)).

volume2(Synth, Chan, Volume) ->
    control14(Synth,Chan,
	      ?MIDI_CTRL_VOLUME,?MIDI_CTRL_VOLUME_FINE,
	      unsigned14(Volume)).

pan(Synth, Chan, Pan) ->
    Value7 = signed7(Pan),
    ?dbg("pan=~w\n", [Value7]),
    control7(Synth, Chan, 
	     ?MIDI_CTRL_PAN_POSITION, 
	     Value7).

pan2(Synth, Chan, Pan) ->
    Value14 = signed14(Pan),
    ?dbg("pan=~w\n", [Value14]),
    control14(Synth,Chan,
	      ?MIDI_CTRL_PAN_POSITION,
	      ?MIDI_CTRL_PAN_POSITION_FINE,
	      Value14).

pitch_bend(Synth, Chan, Bend) ->
    Bend1 = signed14(Bend),
    write(Synth, <<?MIDI_EVENT_PITCHBEND:4,Chan:4,
		     0:1,Bend1:7, 0:1,(Bend1 bsr 7):7>>).

pressure(Synth, Chan, Pressure) ->
    write(Synth, <<?MIDI_EVENT_PRESSURE:4,Chan:4,0:1,Pressure:7>>).

all_off(Synth, Chan) ->
    control7(Synth, Chan, ?MIDI_CTRL_ALL_NOTES_OFF, 0).

reset_all(Synth, Chan) ->
    control7(Synth, Chan, ?MIDI_CTRL_ALL_CONTROLLERS_OFF, 0).

bank(Synth, Chan, Bank) when is_integer(Bank), Bank >=0 ->
    control7(Synth, Chan, 
	     ?MIDI_CTRL_BANK_SELECT,
	     Bank).

bank2(Synth, Chan, Bank) when is_integer(Bank), Bank >= 0 ->
    control14(Synth, Chan, 
	      ?MIDI_CTRL_BANK_SELECT,
	      ?MIDI_CTRL_BANK_SELECT_FINE, 
	      Bank).

expression(Synth, Chan, Value) ->
    control7(Synth, Chan, 
	     ?MIDI_CTRL_EXPRESSION,
	     unsigned7(Value)).

expression2(Synth, Chan, Value) ->
    control14(Synth, Chan, 
	      ?MIDI_CTRL_EXPRESSION,
	      ?MIDI_CTRL_EXPRESSION_FINE,
	      unsigned14(Value)).

signed7(Value) when is_integer(Value) ->
    (Value+16#40) band 16#7f;
signed7(Value) when is_float(Value), abs(Value) =< 1.0 ->
    trunc((Value+1.0)*16#3f) band 16#7f.

unsigned7(Value) when is_integer(Value), Value >= 0 ->
    Value band 16#7f;
unsigned7(Value) when is_float(Value), Value >= 0, Value =< 1.0 ->
    trunc(Value * 16#7f) band 16#7f.

signed14(Value) when is_integer(Value) ->
    (Value+16#2000) band 16#3fff;
signed14(Value) when is_float(Value), abs(Value) =< 1.0 ->
    trunc((Value+1.0)*16#1fff) band 16#3fff.

unsigned14(Value) when is_integer(Value), Value >= 0 ->
    Value band 16#3fff;
unsigned14(Value) when is_float(Value), Value >= 0, Value =< 1.0 ->
    trunc((Value*16#1fff) + 16#2000) band 16#3fff.


control7(Synth, Chan, Control, Arg) ->
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1,Control:7, 0:1,Arg:7>>).

control14(Synth, Chan, Control, ControlFine, Arg) ->
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1, Control:7, 
		   0:1, (Arg bsr 7):7,
		   0:1, ControlFine:7, 
		   0:1, Arg:7
		 >>).

%% like control14 * 2
nrpn(Synth, Chan, Param, Value) ->
    write(Synth, <<?MIDI_EVENT_CONTROLCHANGE:4,Chan:4,
		   0:1, ?MIDI_CTRL_NON_REGISTERED_PARAMETER:7, 
		   0:1, (Param bsr 7):7,
		   0:1, ?MIDI_CTRL_NON_REGISTERED_PARAMETER_FINE:7, 
		   0:1, Param:7,

		   0:1, ?MIDI_CTRL_DATA_ENTRY:7, 
		   0:1, (Value bsr 7):7,
		   0:1, ?MIDI_CTRL_DATA_ENTRY_FINE:7, 
		   0:1, Value:7
		 >>).



program_change(Synth, Chan, Prog) ->
    write(Synth, <<?MIDI_EVENT_PROGRAMCHANGE:4,Chan:4,0:1,Prog:7>>).

%% time parameters

%% time_us()
%% WaitUs = (Now - Ticks)*TParam#tparam.uspp,
tparam(BPM, Division) ->
    MPQN = ?USEC_PER_MINUTE/BPM,
    PPQN = if Division >= 0 -> Division; true -> 1.0 end,
    USPP =
	if Division >= 0 ->
		MPQN / PPQN;
	   true ->
		Div = trunc(Division),
		TicksPerFrame = Div band 16#ff,
		FramesPerSec = 
		    case Div bsr 8 of
			-24 -> 24.0;
			-25 -> 25.0;
			-29 -> 29.97;
			-30 -> 30.0
		    end,
		MPQN / (FramesPerSec*TicksPerFrame)
	end,
    #tparam{mpqn=MPQN,ppqn=PPQN,bpm=BPM,uspp=USPP}.

tparam_set_tempo(TParam=#tparam{ppqn=PPQN}, MPQN) ->
    BPM  = ?USEC_PER_MINUTE / MPQN,
    USPP = MPQN / PPQN,  %% micro seconds per (midi) pulse
    TParam#tparam{mpqn=MPQN,bpm=BPM,uspp=USPP}.

tparam_set_time_signature(TParam, [NN,DD,CC,BB]) ->
    TParam#tparam{sig={NN,(1 bsl DD)},cc=CC,bb=BB}.

with_device(DeviceName, Fun) ->
    case open_device(DeviceName) of
	{ok,Handle} ->
	    try Fun(Handle) of
		Result ->
		    Result
	    after
		close(Handle)
	    end;
	Error ->
	    Error
    end.

open_device(DeviceName = "/"++_) ->
    open(DeviceName,[event,binary,running]);
open_device(DeviceName = "hw:"++_) ->
    open(DeviceName,[event,binary,running]);
open_device(DeviceName = "hw"++_) ->
    open(DeviceName,[event,binary,running]);
open_device(DeviceName = "virtual"++_) ->
    open(DeviceName,[event,binary,running]);
open_device(Name) ->
    case shared_input(Name) of
	#{device := DeviceName } ->
	    open_device(DeviceName);
	Error = {error,_} ->
	    Error
    end.

%% 
proxy(InputDevice, OutputDevice) ->
    {ok,In}  = open(InputDevice,[event,list,running]),
    {ok,Out} = open(OutputDevice, [running]),
    proxy_in(In,Out).

proxy_in(In,Out) ->
    case read(In) of
	{error,eagain} ->
	    ok = select_(In, read),
	    receive
		{select,In,undefined,ready_input} ->
		    proxy_in(In,Out)
	    end;
	{ok,N} when is_integer(N) -> %% got N events
	    proxy_out(In,Out);
	Error->
	    Error
    end.

proxy_out(In, Out) ->
    receive
	{midi,In,Event} -> %% driver handle packet
	    ?dbg("midi event ~p\n", [Event]),
	    write(Out, midi_codec:event_encode(Event)),
	    proxy_out(In,Out);
	{midi,In,Event,_Delta} -> %% driver handle packet
	    ?dbg("midi event ~p\n", [Event]),
	    write(Out, midi_codec:event_encode(Event)),
	    proxy_out(In,Out)
    after 0 ->
	    proxy_in(In,Out)
    end.

io_prog(Synth) when is_reference(Synth) ->
    input_loop(Synth, 
	       fun(_Handle,Event) ->
		       io:format("~p\n", [Event])
	       end,
	       midi_codec:init());	    
io_prog(DeviceName) when is_list(DeviceName) ->
    case open_device(DeviceName) of
	{ok,Handle} ->
	    input_loop(Handle, 
		       fun(_Handle,Event) ->
			       io:format("~p\n", [Event])
		       end,
		       midi_codec:init());
	Error ->
	    Error
    end.

input_loop(Handle, CallBack, State) ->
    case read(Handle) of
	{ok,Chars} when is_binary(Chars); is_list(Chars) ->
	    dump_bin(iolist_to_binary(Chars)),
	    codec_loop(Handle, Chars, CallBack, State);
	{ok,_N} when is_integer(_N) -> %% got N events
	    %% io:format("events ~p\n", [_N]),
	    message_loop(Handle, CallBack, State);
	{error,eagain} ->
	    %% io:format("error eagain\n"),
	    ok = select_(Handle, read),
	    receive
		{select,Handle,undefined,ready_input} ->
		    input_loop(Handle,CallBack,State)
	    end;
	Error ->
	    Error
    end.

codec_loop(Handle, Chars, CallBack, State) ->
    case midi_codec:scan(Chars, State) of
	{more, State1} ->  %% more data needed (not in status)
	    input_loop(Handle, CallBack, State1);
	{eot, State1} -> %% no more input needed
	    input_loop(Handle, CallBack, State1);
	{{Status,Params},State1} ->
	    Event = midi_codec:event_decode(Status, Params),
	    CallBack(Handle, Event),
	    codec_loop(Handle, <<>>, CallBack, State1)
    end.

message_loop(Handle, CallBack, State) ->
    receive
	{midi,Handle,Event} -> %% driver handle packet
	    %% io:format("midi event ~p\n", [Event]),
	    CallBack(Handle, Event),
	    message_loop(Handle, CallBack, State);
	{midi,Handle,Event,_Delta} -> %% driver handle packet
	    %% io:format("midi event ~p\n", [Event]),
	    CallBack(Handle, Event),
	    message_loop(Handle, CallBack, State)
    after 0 ->
	    input_loop(Handle, CallBack, State)
    end.

dump_bin(<<C,Cs/binary>>) ->
    io:put_chars([$>,$\s,tl(integer_to_list(16#100+C, 2)),$\n]),
    dump_bin(Cs);
dump_bin(<<>>) ->
    ok.


synth() ->
    application:get_env(midi, midi_synth, ?DEFAULT_MIDI_SYNTH).

backend() ->
    application:get_env(midi, midi_backend, ?DEFAULT_MIDI_BACKEND).

devices() ->
    (backend()):devices().

%% open "shared" input midi device (keyboard etc) by first try to locate
%% a virtual device already connected to the midi devie.
%% otherwise find a "free" virtual device and connect the
%% then return that device for application to open instead of "real"
%% hardware device that is mostly one at the time access...

shared_input(Name) ->
    B = backend(),    
    Devices = B:devices(),
    case find_device_by_name(Name, Devices) of
	false ->
	    {error, enoent};
	#{output := [Port]} -> %% already connected
	    case find_device_by_port(Port, Devices) of
		false ->
		    {error, port_not_found};  %% maybe closing, retry?
		Parent ->
		    %% maybe check that "Parent" is a virtual device?
		    Parent
	    end;
	Input ->
	    %% find free virtual device
	    case B:find_free_virtual_port(Devices) of
		false ->
		    io:format("forgot to install the virmid? run setup.sh\n"),
		    {error, no_ports_available};
		Virtual = #{ device := _DeviceName } ->
		    case B:connect(Input, Virtual) of
			"" -> Virtual;
			Err -> {error, Err}
		    end
	    end
    end.

open_synth() ->
    case setup_synth() of
	{ok,#{ device:=Device} } -> open(Device, [running]);
	Error -> Error
    end.

%% try start synth as server 
setup_synth() ->
    B = backend(),
    S = synth(),
    Ds = B:devices(),
    case find_synth_device(S, Ds) of
	false ->
	    start_synth(S),
	    Ds1 = B:devices(),
	    %% retry lookup
	    case find_synth_device(S, Ds1) of
		false -> {error, synth_not_started};
		Synth -> setup_synth(Synth,Ds1)
	    end;
	Synth ->
	    setup_synth(Synth,Ds)
    end.

setup_synth(Synth,Ds) ->
    case find_synth_input_port(Ds) of
	false -> setup_synth_input(Synth,Ds);
	Input -> {ok,Input}
    end.

setup_synth_input(Synth,Ds) ->
    B = backend(),
    case B:find_free_virtual_port(Ds) of
	false ->
	    io:format("forgot to install the virmid? run setup.sh\n"),
	    {error, no_ports_available};
	Virt ->
	    case B:connect(Virt, Synth) of
		"" -> {ok,Virt};
		Err -> {error,Err}
	    end
    end.


start_synth(S) when is_atom(S) ->
    S:start();
start_synth(Name) when is_list(Name) ->
    undefined.

find_synth_device(S, Ds) when is_atom(S) ->
    Name = S:portname(),
    find_device_by_name(Name, Ds);
find_synth_device(Name, Ds) when is_list(Name) ->
    find_device_by_name(Name, Ds).


%% Lookup Device given a port 
find_device_by_port(Port, [D=#{port:=Port}|_Ds]) ->
    D;
find_device_by_port(Port, [_|Ds]) ->
    find_device_by_port(Port, Ds);
find_device_by_port(_Port, []) ->
    false.

%% Lookup Device given client name (actually by prefix)
find_device_by_name(Name, [D=#{client_name:=ClientName}|Ds]) ->
    case lists:prefix(Name, ClientName) of
	true -> D;
	false -> find_device_by_name(Name, Ds)
    end;
find_device_by_name(_Name, []) ->
    false.

%% Locate the synth input port
find_synth_input_port(Ds) ->
    S = synth(),
    case find_synth_device(S, Ds) of
	false -> false;
	Synth -> find_input_port(maps:get(port,Synth),Ds)
    end.

%% Locate the input port/device 
find_input_port(Port,[D=#{output:=OUT}|Ds]) ->
    case lists:member(Port, OUT) of
	true -> D;
	false -> find_input_port(Port,Ds)
    end;
find_input_port(_Port,[]) ->
    false.

device_inquiry(DeviceName) when is_list(DeviceName) ->
    with_device(DeviceName, fun device_inquiry/1);
device_inquiry(Handle) ->
    device_inquiry(Handle, 16#7f).
device_inquiry(Handle, DID) when ?is_handle(Handle) ->
    flush(Handle),
    request_device_inquiry(Handle, DID),
    case read_sys(Handle) of
	{ok, <<0:1, ?SYSEX_NON_REALTIME:7, %% non-realtime message
	       0:1, DeviceID:7,   %% 1 bytes!
	       0:1, ?SYSEX_DEVICE_INQUIRY:7,
	       0:1, ?DEVICE_INQUIRY_REPLY:7,
	       0:1, 00:7,    %% Manufacturers id code (0001-7F7F)
	       0:1, MH:7,
	       0:1, ML:7,	
	       0:1, FamilyLs:7, %% Family (1)
	       0:1, FamilyMs:7, %% Family (1)
	       0:1, MemberLs:7, %% Product (0)
	       0:1, MemberMs:7, %% Product (0)
	       Vsn/binary       %% Version (vary 3-4 bytes!!!)
	     >>} ->
	    ManufID = <<16#00,MH,ML>>,
	    Version = list_to_tuple(binary_to_list(Vsn)),
	    Family = (FamilyMs bsl 7) bor FamilyLs,	    
	    Member = (MemberMs bsl 7) bor MemberLs,
	    ManufName = midi_manufacturer:lookup(ManufID),
	    {ok, #{id=>DeviceID,
		   manufacturer => ManufID,
		   manufacturer_name => ManufName,
		   family=>Family, 
		   member=>Member, 
		   version=>Version }};
	{ok, <<0:1, ?SYSEX_NON_REALTIME:7,    %% non-realtime message
	       0:1, ID:7,       %% 1 bytes!
	       0:1, ?SYSEX_DEVICE_INQUIRY:7,
	       0:1, ?DEVICE_INQUIRY_REPLY:7,
	       0:1, MM:7,       %% Manufacturers id code (01-7F)
	       0:1, FamilyLs:7, %% Family (1)
	       0:1, FamilyMs:7, %% Family (1)
	       0:1, MemberLs:7, %% Product (0)
	       0:1, MemberMs:7, %% Product (0)
	       Vsn/binary       %% Version (vary 3-4 bytes!!!)
	     >>} ->
	    ManufID = <<MM>>,
	    Version = list_to_tuple(binary_to_list(Vsn)),
	    Family = (FamilyMs bsl 7) bor FamilyLs,	    
	    Member = (MemberMs bsl 7) bor MemberLs,
	    ManufName = midi_manufacturer:lookup(ManufID),
	    {ok, #{id=>ID,
		   manufacturer => ManufID,
		   manufacturer_name => ManufName,
		   family=>Family, member=>Member, version=>Version }};
	Error ->	    
	    Error
	end.

%% sysex messages
request_device_inquiry(Handle) ->
    request_device_inquiry(Handle,all).
request_device_inquiry(Handle, all) ->
    request_device_inquiry_(Handle, 16#7f);
request_device_inquiry(Handle, ID) when
      ?is_handle(Handle), is_integer(ID), ID >= 1, ID =< 16#7f ->
    request_device_inquiry_(Handle, ID-1).

request_device_inquiry_(Handle, DeviceID) ->
    write(Handle, <<?MIDI_EVENT_SYS:4, 0:4,
		    0:1, ?SYSEX_NON_REALTIME:7,    %% non-realtime
		    0:1, DeviceID:7, %% +1 = 1-127
		    0:1, ?SYSEX_DEVICE_INQUIRY:7,
		    0:1, ?DEVICE_INQUIRY_REQUEST:7,
		    ?MIDI_EVENT_SYS:4, 7:4>>).

%% read sys messages, drop other events (fixme?)
read_sys(Handle) when ?is_handle(Handle) ->
    read_sys_(Handle, 4, 3000).
read_sys(Handle, Timeout) when ?is_handle(Handle) ->
    read_sys_(Handle, 3, Timeout).

read_sys_(_Handle, 0, _Timeout) ->
    {error, not_found};
read_sys_(Handle, I, Timeout) ->
    ?dbg("read_sys_ i=~w, timeout=~w\n", [I, Timeout]),
    case read(Handle) of
	{error,eagain} ->
	    ?dbg("read_sys_ eagain\n"),
	    ok = select_(Handle, read),
	    receive
		{select,Handle,undefined,ready_input} ->
		    read_sys_(Handle, I, Timeout)
	    after Timeout ->
		    select_(Handle, cancel_read),
		    receive
			{select,Handle,undefined,ready_input} ->
			    read_sys_(Handle, I, Timeout)
		    after 0 -> %% wait for cancel_read
			    {error, timeout}
		    end
	    end;
	{ok, 0} ->
	    ?dbg("read_sys_ got 0 events (retry)\n", []),
	    read_sys_(Handle, I-1, Timeout);
	{ok,_N} when is_integer(_N) -> %% got N events
	    ?dbg("read_sys_ got ~w events\n", [_N]),
	    case recv_sys(Handle, 0) of
		{error, timeout} ->
		    read_sys_(Handle, I-1, Timeout);
		Other ->
		    Other
	    end;
	Error->
	    Error
    end.

%% first scan for sys message in the queue
%% then try to read more events
recv_sys(Handle) when ?is_handle(Handle) ->
    case recv_sys(Handle, 10) of
	{error, timeout} ->
	    read_sys(Handle);
	Other ->
	    Other
    end.

recv_sys(Handle,Timeout) ->
    ?dbg("recv_sys_: timeout=~w\n", [Timeout]),
    receive
	{midi,Handle,{sys,_N,<<>>}} -> %% skip (buggy?) empty sys message
	    recv_sys(Handle,Timeout);
	{midi,Handle,{sys,_N,Data}} ->
	    {ok,Data}
	%% {midi,Handle,_Event} ->
	%%   ?dbg("flush event ~p\n", [_Event]),
	%%   recv_sys_(Handle,I,Timeout)
    after %% must wait and context switch!
	Timeout ->
	    {error, timeout}
    end.

flush(Handle) when ?is_handle(Handle) ->
    ?dbg("flush: begin\n", []),
    R = flush_(Handle, 0),
    ?dbg("flush: end ~p\n", [R]),
    R.

flush_(Handle, I) ->
    I1 = flush_events_(Handle, I),
    case read(Handle) of
	{error,eagain} ->
	    ?dbg("flush: eagain\n", []),
	    ok = select_(Handle, read),
	    ?dbg("flush: select read\n", []),
	    receive
		{select,Handle,undefined,ready_input} ->
		    flush_(Handle, I1)
	    after 100 ->
		    ?dbg("flush: cancel_read\n", []),
		    select_(Handle, cancel_read),
		    flush_select(Handle),
		    I1
	    end;
	{ok,0} ->
	    I1;
	{ok,N} when is_integer(N) -> %% got N events
	    ?dbg("flush detected ~w events\n", [N]),
	    flush_(Handle, I);
	Error->
	    Error
    end.

flush_events_(Handle, I) ->
    receive
	{midi,Handle,_Event} ->
	    ?dbg("flushed event ~p\n", [_Event]),
	    flush_events_(Handle, I+1)
    after 0 ->
	    ?dbg("flushed ~w events\n", [I]),
	    I
    end.

flush_select(Handle) ->
    receive
	{select,Handle,_Info,_Mesg} ->
	    ?dbg("flush_ready_input:  ~p ~p\n", [_Info, _Mesg]),
	    ok
    after 0 ->
	    ok
    end.
