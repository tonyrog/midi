%%
%% Format a midi file
%%
-module(midi_format).

-compile(export_all).

-include("../include/midi.hrl").

-define(HUGE, 16#ffffffff).
-define(USEC_PER_MINUTE, 60000000).
-define(DEFAULT_MPQN,    500000).
-define(EOT, '-').

file(File) ->
    file(File,user,?USEC_PER_MINUTE / ?DEFAULT_MPQN).    

file(File,Fd,BPM) ->
    case midi_file:load(File) of
	{ok,{1,_NumTracks,Division},Tracks} ->
	    MPQN = ?USEC_PER_MINUTE/BPM,
	    PPQN = if Division >= 0 -> Division; true -> 1.0 end,
	    _USPP =
		if Division >= 0 ->
			MPQN / PPQN;
		   true ->
			TicksPerFrame = Division band 16#ff,
			FramesPerSec =
			    case Division bsr 8 of
				-24 -> 24.0;
				-25 -> 25.0;
				-29 -> 29.97;
				-30 -> 30.0
			    end,
			?DEFAULT_MPQN / (FramesPerSec*TicksPerFrame)
		end,
	    Tracks1 = [T || {_TID,T} <- Tracks],
	    format(Fd,Tracks1),
	    ok;
	Error ->
	    Error
    end.

%% do a horizontal layout 

format(Fd,Trs) ->
    {Trs1,Ts1} = init(Trs,[],[]),
    format_(Fd,Trs1,Ts1).

format_(Fd,Trs,Ts) ->
    case lists:min(Ts) of
	?HUGE -> ok;
	Tick ->
	    io:format(Fd, "~w:", [Tick]),
	    next_(Fd,Trs,Ts,[],[],[],Tick)
    end.

next_(Fd,[?EOT|Trs],[_T|Ts],Trs1,Ts1,_Fld,Tick) ->
    format_fld(Fd, [?EOT]),
    next_(Fd,Trs,Ts,[?EOT|Trs1],[?HUGE|Ts1],[],Tick);
next_(Fd,[[E|Es]|Trs],[T|Ts],Trs1,Ts1,Fld,Tick) when T =< Tick ->
    E1 = case E of
	     {meta,tempo,V} ->
		 BPM = trunc(?USEC_PER_MINUTE / V),
		 {tempo,BPM};
	     {note_on,Chan,Note,0} ->
		 {off,Chan,Note};
	     {note_off,Chan,Note,_Vel} ->
		 {off,Chan,Note};
	     {note_on,Chan,Note,Vel} ->
		 {on,Chan,Note,Vel};
	     {meta,end_of_track,_} ->
		 eof;
	     _ -> E
	 end,
    case E1 of
	eot ->
	    format_fld(Fd, [eot|Fld]),
	    next_(Fd,Trs,Ts,[?EOT|Trs1],[?HUGE|Ts1],[],Tick);
	_ ->
	    case Es of
		[D|Es1] ->
		    T1 = T+D,
		    next_(Fd,[Es1|Trs],[T1|Ts],Trs1,Ts1,[E1|Fld],
			  Tick);
		[] ->
		    %% warn end of track not signaled ...
		    format_fld(Fd, [E1|Fld]),
		    next_(Fd,Trs,Ts,[?EOT|Trs1],[?HUGE|Ts1],[],Tick)
	    end
    end;
next_(Fd,[Es|Trs],[T|Ts],Trs1,Ts1,Fld,Tick) ->
    format_fld(Fd, Fld),
    next_(Fd,Trs,Ts,[Es|Trs1],[T|Ts1],[],Tick);
next_(Fd,[],[],Trs1,Ts1,_Fld,_Tick) ->
    io:format("\n"),
    format_(Fd,lists:reverse(Trs1),lists:reverse(Ts1)).

format_fld(Fd, Fs) ->
    io:format(Fd, " ~p", [lists:reverse(Fs)]).

%% load initial delta time from 0
init([[D|Es] | Trs], Trs1, Ts) when is_integer(D) ->
    init(Trs, [Es|Trs1], [D|Ts]);
init([], Trs, Ts) ->
    {lists:reverse(Trs), lists:reverse(Ts)}.
