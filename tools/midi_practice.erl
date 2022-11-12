%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2022 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(midi_practice).

-behaviour(epxw).

%% API
-export([start/0, start/1]). 
-export([start_lpk25/0]).
-export([start_usb_midi/0]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).
%% epxw
-export([draw/3]).

-type chan() :: 0..15.
-type note() :: 0..127.
-type voice() :: integer().
-type pressure() :: 0..127.

-define(SERVER, ?MODULE).

-define(TEXT_COLOR,              {0,0,0,0}).       %% black text
-define(FONT_NAME, "Arial").
-define(FONT_SIZE, 48).

-record(state,
	{
	 midi_in,      %% midi input device
	 pending = 0,  %% number of pending events
	 voices :: [voice()],
	 active = #{} :: #{ {chan(),note()} => voice() },
	 pressure = #{} :: #{ chan() => pressure() },
	 opts = [],
	 match = ignore,
	 seq,
	 glyph
	}).

start() -> start([{name,"LPK25"},{lesson,{seq,white_major}}]).
start_lpk25() -> start([{name,"LPK25"},{lesson,{random,white_major}}]). 
start_usb_midi() -> start([{name,"USB-MIDI"},{lesson,{random,white_major}}]).

start(Opts) ->
    application:ensure_all_started(epx),
    application:ensure_all_started(midi),
    epxw:start(?MODULE,
	       Opts,
	       [{title, "Midi Practice"},
		{scroll_horizontal, none},  %% none|top|bottom
		{scroll_vertical,   none},  %% none|left|right
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{scroll_bar_size,   14},
		{scroll_hndl_size,  10},
		{left_bar,0},
		{top_bar,0},
		{right_bar,0},
		{width, 512},
		{height, 512},
		{view_width, 512},
		{view_height, 512}]).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init(Opts) ->
    process_flag(trap_exit, true),
    case find_device(Opts) of
	{ok, Device} ->
	    {ok,In}  = midi:open(Device,[event,list,running]),
	    ok = flush_until_select(In),
	    Voices = lists:seq(1, 10),
	    Lesson = proplists:get_value(lesson, Opts, {random,white_major}),
	    Seq = {_,_,_,Choord} = lesson_first(Lesson),
	    {ok,Font} = epx_font:match([{name,?FONT_NAME},{size,?FONT_SIZE}]),
	    {W,H}  = epx_font:dimension(Font,"0"),
	    Ascent = epx:font_info(Font, ascent),
	    G = {Font,{W,H},Ascent},
	    io:format("PLAY: ~s\n", [Choord]),
	    {ok, #state { midi_in=In, voices=Voices, active=#{}, 
			  opts = Opts,
			  seq = Seq,
			  pressure=#{},
			  glyph = G
			}};
	Error ->
	    {stop, Error}
    end.

    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({select,In,undefined,ready_input}, State) when 
      In =:= State#state.midi_in, State#state.pending =:= 0 ->
    case midi:read(In) of
	select -> 
	    io:format("select: select\n", []),
	    {noreply, State};
	{ok, N} -> %% got N midi_events
	    io:format("select: ~w new\n", [N]),
	    {noreply, State#state { pending = N} }
    end;
handle_info({midi,In,Event}, State) when In =:= State#state.midi_in ->
    State1 = midi_event(Event, State),
    {noreply, midi_next(State1)};
handle_info({midi,In,Event,_Delta}, State) when In =:= State#state.midi_in ->
    State1 = midi_event(Event, State),
    {noreply, midi_next(State1)};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%% Draw callback
draw(Pixels, _Rect, State) ->
    io:format("Draw\n"),
    case State#state.match of
	true ->
	    epx:pixmap_fill(Pixels, green);
	false ->
	    epx:pixmap_fill(Pixels, red);
	ignore ->
	    epx:pixmap_fill(Pixels, grey)
    end,
    {_,_,_,Choord} = State#state.seq,
    {Font,_WH,Ascent} = State#state.glyph,
    epx_gc:set_font(Font),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    epx:draw_string(Pixels, 32, 64+Ascent, Choord),
    State.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_device(Opts) ->
    Ds = midi:devices(),
    Name = proplists:get_value(name, Opts, ""),
    case midi:find_device_by_name(Name, Ds) of
	#{ device := Device } -> 
	    {ok, Device};
	#{ output := [Out|_]} ->
	    case midi:find_device_by_port(Out, Ds) of
		#{ device := Device } ->
		    {ok, Device};
		_ ->
		    {error, not_found}
	    end;
	_ ->
	    case midi:shared_input(Name) of
		#{ device := Device } ->
		    {ok, Device};
		_ ->
		    {error, not_found}
	    end
    end.

flush_until_select(In) ->
    case midi:read(In) of
	select -> ok;
	{ok, N} ->
	    case flush_midi_events(In, N) of
		ok -> flush_until_select(In);
		error -> error
	    end
    end.

flush_midi_events(_In,0) ->
    ok;
flush_midi_events(In, I) ->
    receive
	{midi,In,_Event} ->
	    flush_midi_events(In, I-1);
	{midi,In,_Event,_Delta} ->
	    flush_midi_events(In, I-1)
    after 0 ->
	    error %% missing events
    end.

midi_event({note_on,Chan,Note,Velocity}, State) ->
    note_on(Chan, Note, Velocity, State);
midi_event({note_off,Chan,Note,Velocity}, State) ->
    note_off(Chan, Note, Velocity, State);
midi_event({pressure, Chan, Pressure}, State) ->
    Pressure = maps:put(Chan, Pressure, State#state.pressure),
    State#state { pressure = Pressure };
midi_event(_, State) ->
    State.

midi_next(State = #state { midi_in = In, pending = N}) ->
    if N > 1 ->
	    %% io:format("midi_next: ~w remain\n", [N-1]),
	    State#state { pending = N-1 };
       N =:= 1 ->
	    case midi:read(In) of
		select ->
		    %% io:format("midi_next: select\n", []),
		    State#state { pending = 0 };  %% wait for select
		{ok,N1} -> %% we got N1 midi messages
		    %% io:format("midi_next: ~w new\n", [N1]),		    
		    State#state { pending = N1 }
	    end
    end.


note_on(Chan,Note,0,State) ->
    note_off(Chan,Note,0,State);
note_on(Chan,Note,_Velocity,State) ->
    case maps:take({Chan,Note},State#state.active) of
	error ->
	    case allocate_voice(State#state.voices) of
		{V,Vs} ->
		    %%io:format("~w: allocate ~w, vs:~p\n", [{Chan,Note},V,Vs]),
		    Active0 = State#state.active,
		    Active = Active0# { {Chan,Note} =>  V },
		    epxw:invalidate(),
		    {_,_,_,Choord} = State#state.seq,
		    case test_active(Choord, Active) of
			true ->
			    Seq = {_,_,_,Choord1} = 
				lesson_next(State#state.seq),
			    io:format("PLAY: ~s\n", [Choord1]),
			    State#state { match = true,
					  voices = Vs, active = Active,
					  seq = Seq };
			false ->
			    State#state { match = false,
					  voices = Vs, active = Active };
			ignore -> %% no choord
			    State#state { match = ignore,
					  voices = Vs, active = Active }
		    end;
		false ->
		    State
	    end;
	_ ->
	    %% ignore, already pressed
	    State
    end.

note_off(Chan,Note,_Velocity,State) ->
    case maps:take({Chan,Note}, State#state.active) of
	{V, Active1} ->
	    Voices = release_voice(V, State#state.voices),
	    %% io:format("~w: release ~w vs: ~p\n", [{Chan,Note},V,Voices]),
	    State#state { voices = Voices, active = Active1 };
	error -> %% ignore not pressed
	    State
    end.

release_voice(V, Vs) ->
    [V|Vs].

allocate_voice([V|Vs]) ->
    {V, Vs};
allocate_voice([]) ->
    false.

test_active(Match, Active) ->
    Ns = maps:fold(fun({_Chan,Note},_V,Acc) -> [Note|Acc] end, [], Active),
    Choord = note_info_to_choord(Ns),
    Notes = [case note_to_note_info(N) of
	       {_,_,4,Name} -> Name;
	       {_,_,Oct,Name} when Oct =/= 4 -> 
		   Name++integer_to_list(Oct)
	   end || N <- lists:sort(Ns)],
    io:format("~p: ~s\n", [Notes, Choord]),
    if Choord =/= "" ->
	    Match =:= Choord;
       true ->
	    ignore
    end.


note_info_to_choord([]) -> "";
note_info_to_choord(Ns) -> notes_to_choord(Ns, 0).

notes_to_choord(Ns0, Cnt) ->
    [R|Rs] = Rs1 = lists:sort(Ns0),
    {_,_Note,_Octave,Name} = note_to_note_info(R),
    Rd = [(Ri-R) || Ri<-Rs],
    io:format("R:~w, Rd:~w\n", [R,Rd]),
    case Rd of
	[4,7]          -> Name ++ "maj";
	[4,7,9]        -> Name ++ "6";
	[4,7,10]       -> Name ++ "7";
	[4,7,9,13]     -> Name ++ "9";
	[4,7,10,13,17] -> Name ++ "11";
	[10,14,17,21]  -> Name ++ "13";
	[4,7,14]       -> Name ++ "add9";
	[4,6,10]       -> Name ++ "maj7b5";
	[4,7,11]       -> Name ++ "maj7";
	[4,7,11,14]    -> Name ++ "maj9";
	[3,7]          -> Name ++ "min";
	[3,7,10]       -> Name ++ "min7";
	[3,7,11]       -> Name ++ "min#7";
	[3,6]          -> Name ++ "dim";
	[3,6,9]        -> Name ++ "dim7";
	_ when Cnt =:= 0 ->
	    case Rs1 of
		[A,B,C|Ns]  ->
		    case notes_to_choord([A+12,B+12,C|Ns], Cnt+1) of
			"" ->
			    case notes_to_choord([A+12,B,C|Ns], Cnt+1) of
				"" -> "";
				Choord -> Choord ++ " 2nd"
			    end;
			Choord -> Choord ++ " 1st"
		    end;
		_ -> ""
	    end;
	_ -> ""
    end.

note_to_note_info(N) ->
    Octave = (N div 12) - 1,
    Note = (N rem 12) + 1,
    Name = case Note of
	       1 -> "C";
	       2 -> "C#";
	       3 -> "D";
	       4 -> "D#";
	       5 -> "E";
	       6 -> "F";
	       7 -> "F#";
	       8 -> "G";
	       9 -> "G#";
	       10 -> "A";
	       11 -> "A#";
	       12 -> "B"
	   end,
    {N,Note,Octave,Name}.


lesson_first({random,What}) -> lesson_next({random,0,What,""});
lesson_first({seq,What}) -> lesson_next({seq,-1,What,""}).

lesson_next({random,_I,Less,_Choord}) ->
    L = less(Less),
    N = maps:size(L),
    I = rand:uniform(N),
    {random,I,Less,maps:get(I, L)};
lesson_next({seq,I,Less,_Choord}) ->
    L = less(Less),
    N = maps:size(L),
    I1 = (I+1) rem N,
    {seq,I1,Less,maps:get(I1+1, L)}.

less(white_major) -> white_major();
less(black_major) -> black_major();
less(major) -> major().

white_major() ->
    #{ 1 => "Cmaj",
       2 => "Dmaj",
       3 => "Emaj",
       4 => "Fmaj",
       5 => "Gmaj",
       6 => "Amaj",
       7 => "Bmaj" }.

black_major() ->
    #{ 	1 => "C#maj",
	2 => "D#maj",
	3 => "F#maj",
	4 => "G#maj",
	5 => "A#maj" }.

major() ->
    #{ 1 => "Cmaj",
       2 => "Dmaj",
       3 => "Emaj",
       4 => "Fmaj",
       5 => "Gmaj",
       6 => "Amaj",
       7 => "Bmaj",
       8 => "C#maj",
       9 => "D#maj",
       10 => "F#maj",
       11 => "G#maj",
       12 => "A#maj" }.
