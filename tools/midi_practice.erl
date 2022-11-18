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
-export([start_vmpk/0]).
-export([start_usb_midi/0]).
-compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).
%% epxw
-export([draw/3, draw/4, button_press/2]).

-type chan() :: 0..15.
-type note() :: 0..127.
-type voice() :: integer().
-type pressure() :: 0..127.

-define(SERVER, ?MODULE).

-define(TEXT_COLOR,              {0,0,0,0}).       %% black text
-define(FONT_NAME, "Arial").
-define(FONT_SIZE, 48).
-define(SHARP_FONT_SIZE, 14).

-define(BUTTON_FONT_SIZE, 10).
-define(BUTTON_HEIGHT, 14).
-define(LEFT_BAR_SIZE, 64).
-define(LEFT_BAR_COLOR, orange).
-define(BUTTON_COLOR, gray).
-define(BUTTON_SELECTED_COLOR, green).


-record(lesson,
	{
	 type :: random | seq,
	 pos = 0 :: integer(),
	 step = 0 :: -1 | 0 | 1,
	 map_name :: atom(),
	 chord = "" :: string()  %% current chord
	}).

-record(button,
	{
	 id :: atom(),
	 x :: number(),
	 y :: number(),
	 w :: number(),
	 h :: number(),
	 enable :: boolean(),
	 name :: string()
	}).

-record(state,
	{
	 midi_in,      %% midi input device
	 pending = 0,  %% number of pending events
	 voices :: [voice()],
	 active = #{} :: #{ {chan(),note()} => voice() },
	 pressure = #{} :: #{ chan() => pressure() },
	 opts = [],
	 match = ignore,
	 quality = 0.0,
	 chord :: string(), %% current chord if any
	 seq,
	 main_font,
	 sharp_font,
	 button_font,
	 buttons = [] :: [#button{}],
	 g_clef
	}).

start() -> start([{name,"LPK25"}]).
start_lpk25() -> start([{name,"LPK25"}]). 
start_vmpk() -> start([{name,"VMPK Input"}]).
start_usb_midi() -> start([{name,"USB-MIDI"}]).

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
		{left_bar,?LEFT_BAR_SIZE},
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
    case find_input_device(Opts) of
	{ok, Device} ->
	    {ok,In}  = midi:open(Device,[event,list,running]),
	    ok = flush_until_select(In),
	    Voices = lists:seq(1, 10),
	    {ok,Font} = epx_font:match([{name,?FONT_NAME},{size,?FONT_SIZE}]),
	    {ok,BFont} = epx_font:match([{name,?FONT_NAME},{size,?BUTTON_FONT_SIZE}]),
	    {ok,SFont} = epx_font:match([{name,?FONT_NAME},
					 {slant, italic},
					 {size,?SHARP_FONT_SIZE}]),
	    %% io:format("PLAY: ~s\n", [Chord]),
	    {ok,IMG} = epx_image:load(filename:join(code:priv_dir(midi), 
						    "g_clef.png")),
	    [GClef|_] = epx_image:pixmaps(IMG),

	    Default = [major,raising],
	    Buttons = make_buttons([major,minor,sharp,random,raising,falling],
				  Default),

	    Seq = #lesson{chord=_Chord} = lesson_first({raising,major}),

	    {ok, #state { midi_in=In, voices=Voices, active=#{}, 
			  opts = Opts,
			  seq = Seq,
			  pressure=#{},
			  main_font = {Font,epx:font_info(Font, ascent)},
			  sharp_font = {SFont,epx:font_info(SFont, ascent)},
			  button_font = {BFont,epx:font_info(BFont, ascent)},
			  g_clef = GClef,
			  buttons = Buttons 
			}};
	Error ->
	    {stop, Error}
    end.

make_buttons(Bs, Def) ->
    make_buttons_(Bs, 32, Def).

make_buttons_([B|Bs], Y, Def) ->
    Enable = lists:member(B, Def),
    Name = string:substr(string:titlecase(atom_to_list(B)),1,3),
    [#button{id=B,y=Y+2,h=?BUTTON_HEIGHT-4,x=4,w=?LEFT_BAR_SIZE-8,
	     enable=Enable,name=Name} |
     make_buttons_(Bs, Y+?BUTTON_HEIGHT, Def)];
make_buttons_([], _Y, _Def) ->
    [].
    
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
	    %% io:format("select: select\n", []),
	    {noreply, State};
	{ok, N} -> %% got N midi_events
	    %% io:format("select: ~w new\n", [N]),
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
    draw_octave(Pixels, 2),
    draw_active(Pixels, 2, State#state.active),
    draw_notes(Pixels, 2, State),
    #lesson{chord=Chord} = State#state.seq,
    {Font,Ascent} = State#state.main_font,
    epx_gc:set_font(Font),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    epx:draw_string(Pixels, 32, 64+Ascent, Chord),
    {SFont,SAscent} = State#state.sharp_font,
    epx_gc:set_font(SFont),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    epx:draw_string(Pixels, 32, 164+SAscent, State#state.chord),
    if State#state.match =:= false ->
	    epx:draw_string(Pixels, 32, 184+SAscent, 
			    "Q="++integer_to_list(trunc(State#state.quality))++"%");
       true ->
	    ok
    end,
    State.

%% draw left bar (practice selection)
draw(left, Pixels, _Rect, State) ->
    {BFont,Ascent} = State#state.button_font,
    Height = epx:pixmap_info(Pixels, height),
    epx:pixmap_fill_area(Pixels, 0, 0, ?LEFT_BAR_SIZE, Height, 
			 ?LEFT_BAR_COLOR),
    epx_gc:set_font(BFont),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(?BUTTON_COLOR),
    epx_gc:set_border_color(?BUTTON_SELECTED_COLOR),
    lists:foreach(
      fun(#button{enable=E,x=X,y=Y,w=W,h=H,name=Name}) ->
	      epx_gc:set_border_width(if E -> 2; true -> 0 end),
	      {Wi,_Hi} = epx_font:dimension(BFont, Name),
	      epx:draw_rectangle(Pixels,X,Y,W,H),
	      epx:draw_string(Pixels,X+((W-Wi) div 2), Y+Ascent, Name)
      end, State#state.buttons),
    State.

button_press({button_press, left, Pos={Xv,Yv}}, State) ->
    {Xw,Yw} = epxw:view_to_window_pos(Pos),
    X = Xw + ?LEFT_BAR_SIZE,
    if X >= 0, X < ?LEFT_BAR_SIZE ->
	    io:format("Pos=~w\n", [{X, Yw}]);
       true ->
	    io:format("~w\n", [{X, Yw}])
    end,
    State.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-define(WHITE_KEY_WIDTH, 32).
-define(WHITE_KEY_HEIGHT, 200).
-define(BLACK_KEY_WIDTH,  trunc(?WHITE_KEY_WIDTH*0.8)).
-define(BLACK_KEY_HEIGHT, trunc(?WHITE_KEY_HEIGHT*0.6)).
-define(FINGER_WIDTH, (?BLACK_KEY_WIDTH-2)).
-define(NUM_OCTAVES, 2).
-define(NOTE_FIRST, 60).  %% C4
-define(NOTE_LAST,  (?NOTE_FIRST + 12*?NUM_OCTAVES - 1)).
-define(NOTE_WIDTH, 20).

draw_octave(Pixels,N) ->
    X0 = 16*2,
    Y0 = 250,
    epx_gc:set_fill_style(solid),
    epx_gc:set_border_color(black),
    epx_gc:set_border_width(1),
    epx_gc:set_fill_color(white),
    %% draw white
    lists:foreach(
	fun(I) ->
		Y = Y0,
		X = X0+I*?WHITE_KEY_WIDTH,
		epx:draw_rectangle(Pixels, X, Y, 
				   ?WHITE_KEY_WIDTH, ?WHITE_KEY_HEIGHT)
	end, [I*7+J || I <- lists:seq(0,N-1), J <- [0,1,2,3,4,5,6]]),
    %% draw black
    epx_gc:set_fill_color(black),
    lists:foreach(
	fun(I) ->
		Y = Y0,
		X = X0+I*?WHITE_KEY_WIDTH+(?WHITE_KEY_WIDTH div 2),
		epx:draw_rectangle(Pixels, X, Y, 
				   ?BLACK_KEY_WIDTH, ?BLACK_KEY_HEIGHT)
	end, [I*7+J || I <- lists:seq(0,N-1), J <- [0,1,3,4,5]]),
    ok.

%% draw active notes
draw_active(Pixels, N, Active) ->
    X0 = 16*2,
    Y0 = 250,
    epx_gc:set_fill_color(beige),
    maps_foreach(
      fun({_Chan,Note},_V) ->
	      if Note >= ?NOTE_FIRST, Note =< ?NOTE_LAST ->
		      Nd = Note-?NOTE_FIRST,
		      Ni = Nd div 12,
		      {K,Sharp} = note_to_sharp(Nd),
		      I = Ni*7 + K,
		      Yoffs = 
			  if Sharp ->
				  ?BLACK_KEY_HEIGHT - 2*?FINGER_WIDTH;
			     true ->
				  ?WHITE_KEY_HEIGHT - 2*?FINGER_WIDTH
			  end,
		      XOffs = 
			  if Sharp ->
				  (?WHITE_KEY_WIDTH div 2) +
				      (?BLACK_KEY_WIDTH-?FINGER_WIDTH) div 2;
			     true ->
				  (?WHITE_KEY_WIDTH - ?FINGER_WIDTH) div 2
			  end,
		      Y = Y0 + Yoffs,
		      X = X0+I*?WHITE_KEY_WIDTH + XOffs,
		      epx:draw_ellipse(Pixels, X, Y, 
				       ?FINGER_WIDTH, ?FINGER_WIDTH);
		 true ->
		      ignore
	      end
      end, Active),
    ok.

draw_notes(Pixels, N, #state{active=Active,
			     sharp_font={Font,Ascent},
			     g_clef=Gclef}) ->
    IWidth = 320,
    IHeight = 628,
    epx:pixmap_scale_area(Gclef, Pixels, 220, 33, IWidth/4, IHeight/4, [blend]),
    Step  = 22,
    Yoffs = 64,
    Xoffs = 216,
    epx_gc:set_foreground_color(black),
    epx_gc:set_line_width(2),
    epx_gc:set_font(Font),
    lists:foreach(
      fun(Y) ->
	      X0 = Xoffs+80,
	      X1 = Xoffs+80+100,
	      epx:draw_line(Pixels, X0, Y, X1, Y)
      end, [Yoffs+I*Step || I<-lists:seq(0, 5)]),
    epx_gc:set_border_width(4),
    epx_gc:set_fill_style(none),
    epx_gc:set_foreground_color(?TEXT_COLOR),
    maps_foreach(
      fun({_Chan,Note},_) ->
	      if Note >= ?NOTE_FIRST, Note =< ?NOTE_LAST ->
		      Nd = Note-?NOTE_FIRST,
		      Ni = Nd div 12,
		      {K,Sharp} = note_to_sharp(Nd),
		      I = Ni*7 + K,
		      Y = Yoffs + (5*Step - I*Step/2 - ?NOTE_WIDTH/2),
		      X = Xoffs + 80 + 2*?NOTE_WIDTH + 
			  if Sharp -> ?NOTE_WIDTH;
			     true -> 0
			  end,
		      epx:draw_ellipse(Pixels, X, Y, 
				       ?NOTE_WIDTH, ?NOTE_WIDTH),
		      if Sharp ->
			      epx:draw_string(Pixels, X-10, Y+Ascent, "#");
			 true ->
			      ok
		      end;
		 true ->
		      ok
	      end
      end, Active).

-spec note_to_sharp(N::integer) -> {Key::0..6,Sharp::boolean()}.
note_to_sharp(N) ->
    case N rem 12 of
	0 -> {0,false};
	1 -> {0,true};
	2 -> {1,false};
	3 -> {1,true};
	4 -> {2,false};
	5 -> {3,false};
	6 -> {3,true};
	7 -> {4,false};
	8 -> {4,true};
	9 -> {5,false};
	10 -> {5,true};
	11 -> {6,false}
    end.


find_input_device(Opts) ->
    Ds = midi:devices(),
    Name = proplists:get_value(name, Opts, ""),
    case midi:find_device_by_name(Name, Ds) of
	#{ device := Device } -> 
	    {ok, Device};
	#{ input := [In|_]} ->
	    case midi:find_device_by_port(In, Ds) of
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
		    Ts = erlang:monotonic_time(micro_seconds),
		    Active = Active0# { {Chan,Note} =>  {V,Ts} },
		    epxw:invalidate(),
		    #lesson{chord=Chord} = State#state.seq,
		    case match_chord(Chord, Active) of
			{true,Q,_} ->
			    Seq = lesson_next(State#state.seq),
			    State#state { match = true,
					  quality = Q,
					  chord = "",
					  voices = Vs, active = Active,
					  seq = Seq };
			{false,Q,Other} ->
			    State#state { match = false,
					  quality = Q,
					  chord = Other,
					  voices = Vs, active = Active };
			ignore -> %% no chord
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
	{{V,_Delta}, Active1} ->
	    Voices = release_voice(V, State#state.voices),
	    %%%% io:format("~w: release ~w vs: ~p\n", [{Chan,Note},V,Voices]),
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

%% Test if active notes match the Chord
%% - remove potential play along base notes;
%%      [R,R+12|Ns] -> [R+12|Ns]
%%      [R,R+12,R+24,...] -> [R+24|Ns]
%% - check timing 
%%
match_chord(MatchChord, Active) ->
    NTs = maps:fold(fun({_Chan,Note},{_V,Ts},Acc) -> [{Note,Ts}|Acc] end, 
		    [], Active),
    NTs1 = normalize_notes(NTs),
    Ns1 = [N || {N,T} <- NTs1],

    %% check the chord time diff
    Ts1 = [T || {N,T} <- NTs1],
    Tmin = lists:min(Ts1),
    Tmax = lists:max(Ts1),
    Td = ((Tmax-Tmin)/1000),  %% ok round 100 ms? 
    Quality = 100*(1-(min(Td,1000)/1000)),  %% 0-100% (FIXME)
    Chord = notes_to_chord(Ns1),
    display_chord(Chord, Quality, Ns1),
    {(Chord =:= MatchChord) andalso (Quality >= 95), Quality, Chord}.

display_chord(Chord, Quality, Ns) ->
    Notes = [case note_info(N) of
		 {_,_,4,Name} -> Name;
		 {_,_,Oct,Name} when Oct =/= 4 -> 
		     Name++integer_to_list(Oct)
	     end || N <- Ns],
    io:format("~p: quality=~f%, ~s\n", [Notes, Quality, Chord]).


%% sort and remove "play along" one/two base notes
normalize_notes([]) -> [];
normalize_notes(Rs=[_]) -> Rs;
normalize_notes(Rs=[_,_]) -> Rs;
normalize_notes(NTs) ->
    [{R,_T}|Rs] = lists:keysort(1, NTs),
    R12 = R+12,
    R24 = R+24,
    case Rs of
	[{R12,_T12}|Rs1=[{R24,_T24}|Rs1]] -> Rs1;
	[{R12,_T12}|_] -> Rs;
	_ -> NTs
    end.
   
notes_to_chord([]) -> "";
notes_to_chord(Ns) -> notes_to_chord_(Ns, 0).

notes_to_chord_(Ns0, Cnt) ->
    [R|Rs] = Rs1 = lists:sort(Ns0),
    {_,_Note,_Octave,Name} = note_info(R),
    Rd = [(Ri-R) || Ri<-Rs],
    io:format("R:~w, Rd:~w\n", [R,Rd]),
    case Rd of
	%% triads
	[4,7]          -> Name ++ "maj";
	[3,7]          -> Name ++ "min";
	[3,6]          -> Name ++ "dim";
	[4,9]          -> Name ++ "aug";
	[5,7]          -> Name ++ "sus4";
	[2,7]          -> Name ++ "sus2";

	%% extensions
	[3,7,10]       -> Name ++ "min7";
	[3,7,11]       -> Name ++ "min#7";
	[3,6,9]        -> Name ++ "dim7";
	[4,6,10]       -> Name ++ "maj7b5";
	[4,7,9]        -> Name ++ "6";
	[4,7,10]       -> Name ++ "7";
	[4,7,11]       -> Name ++ "maj7";
	[4,7,14]       -> Name ++ "add9";  %% major 9

	%% quint
	[4,7,9,13]     -> Name ++ "9";
	[4,7,11,14]    -> Name ++ "maj9";
	[4,7,10,13,17] -> Name ++ "11";
	[10,14,17,21]  -> Name ++ "13";
	_ when Cnt =:= 0 ->
	    case Rs1 of
		[A,B,C|Ns]  ->
		    case notes_to_chord_([A+12,B+12,C|Ns], Cnt+1) of
			"" ->
			    case notes_to_chord_([A+12,B,C|Ns], Cnt+1) of
				"" -> "";
				Chord -> Chord ++ " 2nd"
			    end;
			Chord -> Chord ++ " 1st"
		    end;
		_ -> ""
	    end;
	_ -> ""
    end.

note_info(N) ->
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


lesson_first({random,What}) -> 
    lesson_next(#lesson{type=random,map_name=What});
lesson_first({raising,What}) -> 
    lesson_next(#lesson{type=seq,pos=-1,step=1,map_name=What});
lesson_first({falling,What}) -> 
    lesson_next(#lesson{type=seq,pos=-1,step=-1,map_name=What}).

lesson_next(L=#lesson{type=random,map_name=Less}) ->
    Map = less(Less),
    N = maps:size(Map),
    I = rand:uniform(N),
    Next = maps:get(I, Map),
    L#lesson{pos=I,chord=Next};
lesson_next(L=#lesson{type=seq,pos=I,step=S,map_name=Less}) ->
    Map = less(Less),
    N = maps:size(Map),
    I1 = (I+S+N) rem N,
    Next = maps:get(I1+1, Map),
    L#lesson{pos=I1,chord=Next}.

-define(WHITE_NOTES, ["C","D","E","F","G","A","B"]).
-define(BLACK_NOTES, ["C#","D#","F#","G#","A#"]).

-define(WHITE_MAJ, [N++"maj" || N <- ?WHITE_NOTES]).
-define(WHITE_MIN, [N++"min" || N <- ?WHITE_NOTES]).
-define(BLACK_MAJ, [N++"maj" || N <- ?BLACK_NOTES]).
-define(BLACK_MIN, [N++"min" || N <- ?BLACK_NOTES]).

less(white_major) -> notes_to_map(?WHITE_MAJ);
less(black_major) -> notes_to_map(?BLACK_MAJ);
less(white_minor) -> notes_to_map(?WHITE_MIN);
less(black_minor) -> notes_to_map(?BLACK_MIN);
less(white) -> notes_to_map(?WHITE_MAJ++?WHITE_MIN);
less(black) -> notes_to_map(?BLACK_MAJ++?BLACK_MIN);
less(major) -> notes_to_map(?WHITE_MAJ++?BLACK_MAJ);
less(minor) -> notes_to_map(?WHITE_MIN++?BLACK_MIN);
less(all) -> notes_to_map(?WHITE_MAJ++?WHITE_MIN++?BLACK_MAJ++?BLACK_MIN).

notes_to_map(Ns) ->
    maps:from_list(lists:zip(lists:seq(1,length(Ns)), Ns)).

%%
maps_foreach(Fun, Map) ->
    try maps:foreach(Fun, Map) of
	ok -> ok
    catch
	error:undef ->
	    maps:fold(fun(K,V,_) -> Fun(K,V) end, [], Map),
	    ok
    end.
