%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2020, Tony Rogvall
%%% @doc
%%%    Test various features
%%% @end
%%% Created : 30 May 2020 by Tony Rogvall <tony@rogvall.se>

-module(midi_test).
-compile(export_all).

-include("../include/midi.hrl").

test_pan() ->
    application:start(midi),
    Note = ?C,
    Delay = 100,
    midi:note_on(synth, 0, Note, 100),
    test_pan_(synth, -1.0, 0.1, 1.0, Note, Delay),
    midi:note_on(synth, 0, Note, 100),
    test_pan_(synth,  1.0, -0.1, -1.0, Note, Delay),
    midi:note_on(synth, 0, Note, 100),
    timer:sleep(Delay),
    midi:note_off(synth, 0, Note, 100),
    ok.
    
test_pan_(Synth, Pan, PanStep, PanEnd, Note, Delay)
  when PanStep>0,Pan =< PanEnd; PanStep<0, Pan >= PanEnd ->
    midi:pan(Synth, 0, Pan),
    timer:sleep(Delay),
    test_pan_(Synth, Pan+PanStep, PanStep, PanEnd, Note, Delay);
test_pan_(_Synth, _Pan, _PanStep, _PanEnd, _Note, _Delay) ->
    ok.


test_f0_f7() ->
    Data = <<
	     16#0,
	     16#F0, 16#03, 16#43, 16#12, 16#00,
	     16#81, 16#48,
	     16#F7, 16#06, 16#43, 16#12, 16#00, 16#43, 16#12, 16#00,
	     16#64,    
	     16#F7, 16#04, 16#43, 16#12, 16#00, 16#F7
	   >>,
    S = midi_codec:init(true, Data),
    midi_file:parse_track(S, []).

test_f0() ->
    Data = <<
	     16#0,
	     16#F0, 16#0D, 16#43, 16#12, 16#00,
	            16#43, 16#12, 16#00, 16#43, 16#12, 16#00,
	            16#43, 16#12, 16#00, 16#F7
	   >>,
    S = midi_codec:init(true, Data),
    midi_file:parse_track(S, []).

