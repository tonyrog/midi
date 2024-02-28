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

%% -compile(export_all).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).
%% epxw
-export([draw/3, draw/4, button_press/2, key_press/2,
	 menu/2, command/3]).
	 
-type chan() :: 0..15.
-type note() :: 0..127.
-type voice() :: integer().
-type pressure() :: 0..127.

-define(SERVER, ?MODULE).

-define(verbose(F, A), io:format((F),(A))).

-define(DEFAULT_SCREEN_WIDTH,  800).
-define(DEFAULT_SCREEN_HEIGHT, 480).

-define(WHITE, grey5).
-define(BLACK, grey10).

-include_lib("epx/include/epx_menu.hrl").

-define(BLACK_TEXT_COLOR,              {0,0,0,0}).
-define(WHITE_TEXT_COLOR,              {0,255,255,255}).
-define(FONT_NAME, "Arial").
-define(FONT_SIZE, 48).
-define(SHARP_FONT_SIZE, 14).
-define(DRUM_FONT_SIZE, 10).
-define(BUTTON_FONT_SIZE, 12).
-define(BUTTON_HEIGHT, 18).
-define(LEFT_BAR_SIZE, 80).
-define(LEFT_BAR_COLOR, {255, 165, 0}).
-define(TOP_BAR_SIZE, 80).
-define(BOT_BAR_SIZE, 18).
-define(TOP_BAR_COLOR, {20,200,20}).
-define(BOT_BAR_COLOR, {20,200,20}).

-define(BUTTON_COLOR, {128,128,128}).
-define(BUTTON_SELECTED_COLOR, {0,200,0}).

-define(DEFAULT_BPM,     80).  %% quarter note
-define(DEFAULT_DIVISION, 4).  %% resulution 4 ticks per quater note 1/16
-define(MAX_BPM, 240).
-define(MIN_BPM, 30).

-define(KEYBOARD_X0, 32).
-define(KEYBOARD_Y0, 128).

-define(GCLEF_X0, 512). %% 220).
-define(GCLEF_Y0, 33).

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

%% color profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 %% epxw: menu info
	 menu_font_name                = "Arial",
	 menu_font_size                = 14,
	 menu_font_color               = ?WHITE,
	 menu_background_color         = ?BLACK,
	 menu_border_color             = green,

	 border_width                  = 1,
	 border_color                  = ?BLACK,
	 select_color                  = ?WHITE,
	 select_border_width           = 2,
	 select_border_color           = ?BLACK,
	 highlight_color               = grey6,
	 highlight_border_width        = 2,
	 highlight_border_color        = red,

	 label_font                    = "Arial",
	 label_font_size               = 12,
	 label_font_color              = ?BLACK,
	 label_background_color        = ?BLACK,
	 label_border_color            = yellow
	}).

-record(state,
	{
	 midi_in,      %% midi input device
	 midi_out,     %% output synth
	 menu_profile,
	 pending = 0,  %% number of pending events
	 voices :: [voice()],   %% pressed keys
	 active = #{} :: #{ {chan(),note()} => voice() },
	 pressure = #{} :: #{ chan() => pressure() },
	 bpm = ?DEFAULT_BPM,
	 division = ?DEFAULT_DIVISION,

	 rythm_dir :: file:name_all(),  %% dir path rythms
	 %% rythm_map :: #{},              %% current rythm, needed?
	 
	 %% div = 4 then the start on each group is a quarter note
	 pattern = #{},
	 pattern_num_ticks = 0,
	 fill = #{},
	 fill_num_ticks = 0,

	 fill_offset = 0,   %% fill variation offset
	 fill_req = false,  %% fill requested
	 fill_count = undefined, %% count when fill started

	 %% drum => ID, [ ID => drum  whem allocated ]
	 drums = #{ drum_list => [closed_hi_hat, open_hi_hat,
				  bass_drum, snare_drum, id_crash_cymbal ],
		    closed_hi_hat => undefined,
		    open_hi_hat => undefined,
		    bass_drum => undefined,
		    snare_drum => undefined,
		    crash_cymbal => undefined
		  },

	 drum_map = #{},  %% drum name => voice id
	 volume_map = #{}, %% drum name => volume
	 drum_set_dir :: file:name_all(),  %% dir path to drums

	 clock,           %% clock timer
	 next_clock_tick, %% clock when next tick is expected
	 pause = false,
	 count = 0,       %% tick counter 0..
	 milli_to_div,

	 opts = [],
	 match = ignore,
	 quality = 0.0,
	 chord :: string(), %% current chord if any
	 seq,
	 main_font,
	 sharp_font,
	 button_font,
	 drum_font,
	 buttons = [] :: [#button{}],
	 g_clef
	}).

start() -> start([{name,"LPK25"}]).
start_lpk25() -> start([{name,"LPK25"}]). 
start_vmpk() -> start([{name,"VMPK Input"}]).
start_usb_midi() -> start([{name,"USB-MIDI"}]).

start(Opts0) ->
    application:load(epx),
    Opts =
	case application:get_env(epx, backend, default) of
	    "fb" ->
		%% application:set_env(epx, backend, "fb"),
		application:set_env(epx, pixel_format, 'argb/little'),
		application:set_env(epx, input_mouse_device, "/dev/input/event0"),
		[{pixel_format, 'argb/little'}|Opts0];
	_ ->
		Opts0
	end,
    application:ensure_all_started(epx),
    application:ensure_all_started(midi),
    application:ensure_all_started(xbus),
    application:load(egear),
    application:set_env(egear, xbus, true),
    application:set_env(egear, device, "/dev/serial/by-id/usb-Palette_Palette_Multi-function_Device_*"),
    application:ensure_all_started(egear),
    %% catch egear:screen_write(1, egear_icon:erlang()),
    %% catch egear:screen_display(1),
    alsa_play:start(#{ channels => 2 }),

    Width  = application:get_env(midi, screen_width, ?DEFAULT_SCREEN_WIDTH),
    Height = application:get_env(midi, screen_height, ?DEFAULT_SCREEN_HEIGHT),

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
		{left_bar_color, ?LEFT_BAR_COLOR},
		{top_bar,?TOP_BAR_SIZE},
		{top_bar_color, ?TOP_BAR_COLOR},
		{bottom_bar, ?BOT_BAR_SIZE},
		{bottom_bar_color, ?BOT_BAR_COLOR},
		{right_bar,0},
		{width, Width},
		{height, Height},
		{view_width, Width-?LEFT_BAR_SIZE},
		{view_height, Height-?TOP_BAR_SIZE-?BOT_BAR_SIZE}]).

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
    Env = Opts ++ application:get_all_env(midi),
    In = case find_input_device(Opts) of
	     {ok, Device} ->
		 {ok,Fd}  = midi:open(Device,[event,list,running]),
		 ok = flush_until_select(Fd),
		 Fd;
	     {error, Err} ->
		 io:format("midi:open error ~p\n", [Err]),
		 undefined
	 end,
    Out = case midi:open_synth() of
	      {ok, MidiOut} -> MidiOut;
	      {error,_} -> undefined
	  end,
    Voices = lists:seq(1, 10),

    Profile = load_profile(Env),
    MenuProfile = create_menu_profile(Profile),

    {ok,Font} = epx_font:match([{name,?FONT_NAME},{size,?FONT_SIZE}]),
    {ok,DFont} = epx_font:match([{name,?FONT_NAME},{size,?DRUM_FONT_SIZE}]),
    {ok,BFont} = epx_font:match([{name,?FONT_NAME},{size,?BUTTON_FONT_SIZE}]),
    {ok,SFont} = epx_font:match([{name,?FONT_NAME},
				 {slant, italic},
				 {size,?SHARP_FONT_SIZE}]),

    %% io:format("PLAY: ~s\n", [Chord]),
    {ok,IMG} = epx_image:load(filename:join(code:priv_dir(midi), 
					    "g_clef.png")),
    [GClef|_] = epx_image:pixmaps(IMG),
    
    Selected = [major,raising],
    Buttons = make_buttons([major,minor,sharp,random,raising,falling],
			   Selected),
    Seq = #lesson{chord=_Chord} = lesson_first({raising,major}),

    DrumSet = application:get_env(midi, drum_set, "Default"),
    DrumSetDir = case application:get_env(midi, drum_dir, undefined) of
		     undefined ->
			 filename:join([code:lib_dir(midi),"tools","DrumSet"]);
		     DDir -> DDir
		 end,
    Drums = alloc_drums(),
    {Drums1,Volumes1} = load_drums(filename:join(DrumSetDir, DrumSet), Drums),
    RythmDir = case application:get_env(midi, rythm_dir, undefined) of
		   undefined ->
		       filename:join([code:lib_dir(midi),"tools","Rythm"]);
		   RDir -> RDir
	       end,
    Rythm = application:get_env(midi, rythm, "Default"),
    RythmMap = load_rythm(RythmDir, Rythm),
    Pattern = maps:get(pattern, RythmMap, #{}),
    NumTicks = tuple_size(maps:get(closed_hi_hat, Pattern, {1,0,0,0})),
    Fill = maps:get(fill, RythmMap, #{}),
    FillNumTicks = tuple_size(maps:get(closed_hi_hat, Fill, {1,0,0,0})),
    
    RythmDivision = maps:get(division, RythmMap, ?DEFAULT_DIVISION),
    RythmBpm = maps:get(bpm, RythmMap, ?DEFAULT_BPM),

    Division0 = proplists:get_value(division, Opts, RythmDivision),
    Division = Division0 + (NumTicks rem Division0),

    Clock = erlang:start_timer(16#ffffffff, undefined, undefined),
    Bpm = proplists:get_value(bpm, Opts, RythmBpm),
    epxw:set_status_text("Bpm: "++integer_to_list(Bpm)),
    MilliToDiv = round((60000 / Bpm)/Division),
    %% io:format("ms=~w\n", [MilliToDiv]),
    erlang:start_timer(MilliToDiv, self(), tick),
    ClockTick = erlang:read_timer(Clock),
    alsa_play:resume(),

    catch xbus:sub("egear.*"),

    {ok, #state { midi_in=In, voices=Voices, active=#{}, 
		  midi_out=Out,
		  menu_profile = MenuProfile,
		  opts = Opts,
		  seq = Seq,
		  bpm = Bpm,
		  division = Division,
		  milli_to_div = MilliToDiv,
		  clock = Clock,
		  next_clock_tick = ClockTick - MilliToDiv,  %% remain!
		  drum_map = Drums1,
		  volume_map = Volumes1,
		  drum_set_dir = DrumSetDir,
		  %% ythm_map = RythmMap,
		  rythm_dir = RythmDir,
		  pattern = Pattern,
		  pattern_num_ticks = NumTicks,
		  fill = Fill,
		  fill_num_ticks = FillNumTicks,
		  pressure=#{},
		  drum_font = {DFont,epx:font_info(DFont, ascent)},
		  main_font = {Font,epx:font_info(Font, ascent)},
		  sharp_font = {SFont,epx:font_info(SFont, ascent)},
		  button_font = {BFont,epx:font_info(BFont, ascent)},
		  g_clef = GClef,
		  buttons = Buttons 
		}}.

alloc_drums() ->
    #{ closed_hi_hat => alsa_play:alloc(),
       open_hi_hat => alsa_play:alloc(),
       snare_drum => alsa_play:alloc(),
       bass_drum => alsa_play:alloc(),
       crash_cymbal => alsa_play:alloc() }.

load_drums(Dir, Map) ->
    {ok,List} = file:list_dir(Dir),
    case lists:member("set.config", List) of
	true -> 
	    {ok,[Set|_]} = file:consult(filename:join(Dir, "set.config")),
	    load_drum_map(Dir, Set, Map);
	false ->
	    Vol = 0.7,
	    load_drum_dir(List, Dir, Vol, Map, #{})
    end.

load_drum_dir([File="42_"++_|List], Dir, Vol, Map, VMap) ->
    load_drum_dir_(closed_hi_hat, List, File, Dir, Vol, Map, VMap);
load_drum_dir([File="46_"++_|List], Dir, Vol, Map, VMap) ->
    load_drum_dir_(open_hi_hat, List, File, Dir, Vol, Map, VMap);
load_drum_dir([File="38_"++_|List], Dir, Vol, Map, VMap) ->
    load_drum_dir_(snare_drum, List, File, Dir, Vol, Map, VMap);
load_drum_dir([File="36_"++_|List], Dir, Vol, Map, VMap) ->
    load_drum_dir_(bass_drum, List, File, Dir, Vol, Map, VMap);
load_drum_dir([File="49_"++_|List], Dir, Vol, Map, VMap) ->
    load_drum_dir_(crash_cymbal, List, File, Dir, Vol, Map, VMap);
load_drum_dir([_|List], Dir, Vol, Map, VMap) ->
    load_drum_dir(List, Dir, Vol, Map, VMap);
load_drum_dir([], _Dir, _Vol, Map, VMap) ->
    {Map, VMap}.

load_drum_dir_(Drum, List, File, Dir, Vol, Map, VMap) ->
    ID = maps:get(Drum, Map),
    alsa_play:clear(ID),
    alsa_play:set_volume(ID, Vol),
    %% FIXME: fILL with some silence and insert a jump instruction
    %% that can vary the beat offset
    alsa_play:append_file(ID, 0, filename:join(Dir, File)),
    load_drum_dir(List, Dir, Vol, Map, VMap#{ Drum => Vol }).

load_drum_map(Dir, Set, Map) ->
    Drums = [closed_hi_hat, open_hi_hat, snare_drum,
	     bass_drum, crash_cymbal],
    load_drum_map_(Drums, Dir, Set, Map, #{}).

load_drum_map_([D|Ds], Dir, Set, Map, VMap) ->
    ID = maps:get(D, Map),
    alsa_play:clear(ID),
    Drum  = maps:get(D, Set),
    case maps:get(file, Drum, undefined) of
	undefined -> %% look form midi_chan and then xy_<name>.wav ?
	    load_drum_map_(Ds, Dir, Set, Map, VMap);
	File ->
	    Vol = maps:get(volume, Drum, 0.7),
	    alsa_play:set_volume(ID, Vol),
	    alsa_play:append_file(ID, 0, filename:join(Dir, File)),
	    load_drum_map_(Ds, Dir, Set, Map, VMap#{ D => Vol })
    end;
load_drum_map_([], _Dir, _Set, Map, VMap) ->
    {Map, VMap}.

load_rythm(Dir, File) ->
    {ok,[R|_]} = file:consult(filename:join(Dir, File)),
    R.

make_buttons(Bs, Def) ->
    make_buttons_(Bs, 32, Def).

make_buttons_([B|Bs], Y, Def) ->
    Enable = lists:member(B, Def),
    Name = string:substr(string:titlecase(atom_to_list(B)),1,3),
    But = #button{id=B,
		  y =?TOP_BAR_SIZE+Y+2,
		  h =?BUTTON_HEIGHT-4,
		  x =4,
		  w =?LEFT_BAR_SIZE-8,
		  enable=Enable,name=Name},
    io:format("button ~w: ~p\n", [B, But]),
    [But | make_buttons_(Bs, Y+?BUTTON_HEIGHT, Def)];
make_buttons_([], _Y, _Def) ->
    [].

find_button({X,Y}, State) ->
    find_button_(X, Y, State#state.buttons).
find_button_(Bx, By, [#button{id=Id,x=X,y=Y,w=W,h=H}|Bs]) ->
    if Bx >= X, Bx =< X+W,
       By >= Y, By =< Y+H -> {ok,Id};
       true -> find_button_(Bx, By, Bs)
    end;
find_button_(_Bx,_By,[]) ->
    false.

toggle_button(ID, State) ->
    State#state {buttons = toggle_button_(ID, State#state.buttons)}.

toggle_button_(ID, [B=#button{id=ID,enable=E}|Bs]) ->
    [B#button { enable = not E} | Bs];
toggle_button_(ID, [B|Bs]) ->
    [B|toggle_button_(ID, Bs)];
toggle_button_(_ID, []) ->
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
    case State#state.midi_out of
	undefined -> ok;
	Out -> midi:write(Out, midi_codec:event_encode(Event))
    end,
    {noreply, midi_next(State1)};
handle_info({midi,In,Event,_Delta}, State) when In =:= State#state.midi_in ->
    State1 = midi_event(Event, State),
    case State#state.midi_out of
	undefined -> ok;
	Out -> midi:write(Out, midi_codec:event_encode(Event))
    end,
    {noreply, midi_next(State1)};
handle_info({timeout, _Ref, tick}, State=#state { pause = true }) ->
    {noreply, State};
handle_info({timeout, _Ref, tick}, State) ->
    N  = State#state.pattern_num_ticks,
    Count = State#state.count,
    I = (Count rem N) + 1,
    FillStart = ((I =:= 1) andalso State#state.fill_req),
    Fill = FillStart orelse is_integer(State#state.fill_count),
    UpdateList = id_list(Fill, Count, N, State),
    alsa_play:update(UpdateList),
    IDList = [ID || {ID,_} <- UpdateList],
    alsa_play:run(IDList),

    draw_tick(I, N, State#state.fill_req, State#state.main_font),

    {FillReq1,FillCount1,FillOffset1} =
	if is_integer(State#state.fill_count) ->
		if (Count - State#state.fill_count) =:= 8 ->
			{false,undefined,State#state.fill_offset+1};
		   true ->
			{false,State#state.fill_count,State#state.fill_offset}
		end;
	   FillStart ->
		%% fill starting, rearma
		{false,Count,State#state.fill_offset};
	   true ->
		{State#state.fill_req,undefined,State#state.fill_offset}
	end,
    ClockTick = erlang:read_timer(State#state.clock),
    MilliToDiv = State#state.milli_to_div,
    NextClockTick = State#state.next_clock_tick,
    Delta = NextClockTick - ClockTick,
    %% io:format("ms=~w,delta=~w\n", [MilliToDiv-Delta,Delta]),
    if Delta < MilliToDiv, Delta < 20 -> 
	    erlang:start_timer(MilliToDiv-Delta, self(), tick),
	    {noreply, State#state{count=Count+1,
				  next_clock_tick = NextClockTick-MilliToDiv,
				  fill_req = FillReq1, 
				  fill_count = FillCount1,
				  fill_offset = FillOffset1
				 }};
       true -> %% too long delay - adjust 
	    erlang:start_timer(MilliToDiv, self(), tick),
	    {noreply, State#state{count=Count+1,
				  next_clock_tick = ClockTick-MilliToDiv,
				  fill_req = FillReq1, 
				  fill_count = FillCount1,
				  fill_offset = FillOffset1
				 }}
    end;
handle_info({xbus, _Pattern, _Event=#{ topic := <<"egear.",Path/binary>>, 
				       value := Value }}, State) ->
    case binary:split(Path, <<".">>, [global]) of
	[<<"button">>,ID,_Index,<<"connect">>] ->
	    State1 = handle_connect(ID, Value, State),
	    {noreply, State1};
	[<<"dial">>,ID,_Index,<<"connect">>] ->
	    State1 = handle_connect(ID, Value, State),
	    {noreply, State1};
	[<<"button">>,ID,_Index,<<"state">>] ->
	    State1 = handle_value(ID, Value, State),
	    {noreply, State1};
	[<<"dial">>,ID,_Index,<<"state">>] ->
	    State1 = handle_value(ID, Value, State),
	    {noreply, State1};
	[<<"dial">>,_ID,_Index,<<"value">>] ->
	    State1 = handle_bpm_value(Value, State),
	    {noreply, State1};
	[<<"slider">>,_ID,_Index,<<"value">>] ->
	    State1 = handle_bpm_value(Value, State),
	    {noreply, State1};
	_ ->
	    io:format("xbus: ~p\n", [_Event]),
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    io:format("got: ~p\n", [_Info]),
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

id_list(true, Count, _N, State=#state{fill=Fill, volume_map=Vol}) ->
    N = State#state.fill_num_ticks,
    io:format("fill: offset=~w\n", [State#state.fill_offset]),
    Count1 = Count + State#state.fill_offset,
    I = (Count1 rem N) + 1,
    J = (Count1 div N),
    cons_drum(I, J, closed_hi_hat, Fill, Vol, State,
    cons_drum(I, J, open_hi_hat,   Fill, Vol, State, 
    cons_drum(I, J, bass_drum,     Fill, Vol, State, 
    cons_drum(I, J, snare_drum,    Fill, Vol, State, 
    cons_drum(I, J, crash_cymbal,  Fill, Vol, State, [])))));
id_list(false, Count, N, State=#state { pattern=Pat, volume_map=Vol}) ->
    I = (Count rem N) + 1,
    J = (Count div N),
    cons_drum(I, J, closed_hi_hat, Pat, Vol, State,
    cons_drum(I, J, open_hi_hat,   Pat, Vol, State, 
    cons_drum(I, J, bass_drum,     Pat, Vol, State, 
    cons_drum(I, J, snare_drum,    Pat, Vol, State, 
    cons_drum(I, J, crash_cymbal,  Pat, Vol, State, []))))).


cons_drum(I, J, Drum, PatMap, VolMap, State, Acc) ->
    Pattern = maps:get(Drum, PatMap),
    DVol = maps:get(Drum, VolMap),
    case element(I, Pattern) of
	Vol when is_number(Vol), Vol > 0, Vol =< 1 ->
	    case maps:get(Drum, State#state.drum_map,undefined) of
		undefined -> Acc;
		ID -> [{ID,[{volume,DVol*Vol},restart]}|Acc]
	    end;
	{N,Vol} when is_number(Vol), Vol > 0, Vol =< 1 -> 
	    %% repeat every N'th time
	    case J rem N of
		0 ->
		    case maps:get(Drum, State#state.drum_map,undefined) of
			undefined -> Acc;
			ID -> [{ID,[{volume,DVol*Vol},restart]}|Acc]
		    end;
		_ -> Acc
	    end;
	0 -> Acc;
	{_,0} -> Acc
    end.

toggle_pause(State) ->
    case not State#state.pause of
	false ->
	    MilliToDiv = State#state.milli_to_div,
	    ClockTick = erlang:read_timer(State#state.clock),
	    erlang:start_timer(MilliToDiv, self(), tick),
	    State#state { pause = false,
			  next_clock_tick = ClockTick - MilliToDiv };
	true ->
	    State#state { pause = true }
    end.

handle_bpm_value(Value, State) ->
    X = (Value / 255),
    Bpm = trunc(?MIN_BPM*(1-X) + ?MAX_BPM*X),
    epxw:set_status_text("Bpm: "++integer_to_list(Bpm)),
    epxw:invalidate(),
    Division = State#state.division,
    MilliToDiv = round((60000 / Bpm)/Division),
    State#state { bpm = Bpm, 
		  division = Division,
		  milli_to_div = MilliToDiv }.

update_bpm_value(Value, State) ->
    Bpm = min(max(?MIN_BPM, State#state.bpm + Value), ?MAX_BPM),
    epxw:set_status_text("Bpm: "++integer_to_list(Bpm)),
    epxw:invalidate(),
    Division = State#state.division,
    MilliToDiv = round((60000 / Bpm)/Division),
    State#state { bpm = Bpm, 
		  division = Division,
		  milli_to_div = MilliToDiv }.

handle_connect(ID, "ok", State) ->
    assign_drum(ID, State);
handle_connect(ID, "enoent", State) ->
    release_drum(ID, State).

handle_value(ID, 1, State) ->
    case lookup_drum(ID, State) of
	error ->
	    State;
	DrumID ->
	    case maps:get(DrumID, State#state.drum_map, undefined) of
		undefined -> 
		    State;
		VoiceID ->
		    alsa_play:restart(VoiceID),
		    alsa_play:run(VoiceID),
		    State
	    end
    end;
handle_value(_ID, 0, State) ->
    State.
    

assign_drum(ID, State=#state{drums=Drums}) ->
    io:format("assign id ~p\n", [ID]),
    case maps:get(drum_list, Drums) of
	[D|Ds] ->
	    io:format("  to drum ~p\n", [D]),
	    State#state{drums=Drums#{ D => ID, ID => D,  drum_list => Ds }};
	[] ->
	    State
    end.

release_drum(ID, State=#state{drums=Drums}) ->
    io:format("unassign id ~p\n", [ID]),
    case maps:get(ID, Drums, undefined) of
	undefined -> State;
	D ->
	    io:format("release drum ~p\n", [D]),
	    Ds = maps:get(drum_list, Drums),
	    State#state{drums=Drums#{ D => undefined, 
				      ID => undefined,  drum_list => [D|Ds] }}
    end.

lookup_drum(ID, #state{drums=Drums}) ->
    R = maps:get(ID, Drums, undefined),
    io:format("lookup_drum ~p res = ~p\n", [ID, R]),
    R.



-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%% Draw callback
draw(Pixels, _Rect, State) ->
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
    epx_gc:set_foreground_color(?BLACK_TEXT_COLOR),
    epx:draw_string(Pixels, 32, 64+Ascent, Chord),
    {SFont,SAscent} = State#state.sharp_font,
    epx_gc:set_font(SFont),
    epx_gc:set_foreground_color(?BLACK_TEXT_COLOR),
    epx:draw_string(Pixels, 32, 164+SAscent, State#state.chord),
    if State#state.match =:= false ->
	    epx:draw_string(Pixels, 32, 32+SAscent,
			    "Q="++integer_to_list(trunc(State#state.quality))++"%");
       true ->
	    ok
    end,
    State.

%% draw the drum -grid
draw(top, Pixels, _Rect, State) ->
    {Font,Ascent} = State#state.drum_font,
    epx_gc:set_font(Font),
    epx_gc:set_foreground_color(?WHITE_TEXT_COLOR),

    epx_gc:set_border_color(black),
    epx_gc:set_border_width(1),
    epx_gc:set_fill_color(blue),

    Empty = erlang:make_tuple(State#state.pattern_num_ticks, 0),

    Left = 8, %% ?LEFT_BAR_SIZE
    Top  = 10,
    W    = 10,
    H    = 10,
    draw_grid_line(Pixels, Left+40, 0, W, H, 1, Empty),
    epx:draw_string(Pixels,Left,    Top+Ascent, "closed:"),
    draw_grid_line(Pixels, Left+40, Top, W, H, 1, 
		   maps:get(closed_hi_hat, State#state.pattern)),
    epx:draw_string(Pixels,Left,    Top+12+Ascent, "open:"),
    draw_grid_line(Pixels, Left+40, Top+12, W, H, 1, 
		   maps:get(open_hi_hat, State#state.pattern)),
    epx:draw_string(Pixels,Left,    Top+24+Ascent, "snare:"),
    draw_grid_line(Pixels, Left+40, Top+24, W, H, 1,
		   maps:get(snare_drum, State#state.pattern)),
    epx:draw_string(Pixels,Left,    Top+36+Ascent, "bass:"),
    draw_grid_line(Pixels, Left+40, Top+36, W, H, 1,
		   maps:get(bass_drum, State#state.pattern)),
    epx:draw_string(Pixels,Left,    Top+48+Ascent, "crash:"),
    draw_grid_line(Pixels, Left+40, Top+48, W, H, 1, 
		   maps:get(crash_cymbal, State#state.pattern)),
    State;
%% draw left bar (practice selection)
draw(left, Pixels, _Rect, State) ->
    {Font,Ascent} = State#state.button_font,
    epx_gc:set_font(Font),
    epx_gc:set_foreground_color(?BLACK_TEXT_COLOR),
    epx_gc:set_fill_style(solid),
    epx_gc:set_fill_color(?BUTTON_COLOR),
    epx_gc:set_border_color(?BUTTON_SELECTED_COLOR),
    lists:foreach(
      fun(#button{enable=E,x=X,y=Y,w=W,h=H,name=Name}) ->
	      epx_gc:set_border_width(if E -> 2; true -> 0 end),
	      {Wi,_Hi} = epx_font:dimension(Font, Name),
	      epx:draw_rectangle(Pixels,X,Y,W,H),
	      epx:draw_string(Pixels,X+((W-Wi) div 2), Y+Ascent, Name)
      end, State#state.buttons),
    State.

draw_grid_line(Pixels, X, Y, W, H, I, Bits) when I =< tuple_size(Bits) ->
    epx_gc:set_fill_style(none),
    epx:draw_rectangle(Pixels,X,Y,W,H),
    case element(I, Bits) of
	0 -> ok;
	_N ->
	    epx_gc:set_fill_style(solid),
	    epx:draw_ellipse(Pixels,X,Y,W,H)
    end,
    draw_grid_line(Pixels,X+W+1,Y,W,H,I+1,Bits);
draw_grid_line(_,_X,_Y,_W,_H,_I,_Bits) ->
    ok.

toggle_grid({X,Y}, State) ->
    Size = State#state.pattern_num_ticks,
    Left = 8,
    Top  = 10,
    W    = 10,
    H    = 10,
    if X >= Left+40, X =< Left+40+Size*(W+1),
       Y >= Top, Y =< Top+48+H ->
	    I = trunc(X - (Left+40)) div (W+1),
	    J = trunc(Y - Top) div (H+2),
	    %% io:format("I=~w, J=~w\n", [I, J]),
	    Pattern = State#state.pattern,
	    case J of
		0 ->
		    Pattern1 = toggle_element(I+1, Pattern, closed_hi_hat),
		    State#state{ pattern = Pattern1 };
		1 ->
		    Pattern1 = toggle_element(I+1, Pattern, open_hi_hat),
		    State#state{ pattern = Pattern1 };
		2 ->
		    Pattern1 = toggle_element(I+1, Pattern, snare_drum),
		    State#state{ pattern = Pattern1 };
		3 ->
		    Pattern1 = toggle_element(I+1, Pattern, bass_drum),
		    State#state{ pattern = Pattern1 };
		4 ->
		    Pattern1 = toggle_element(I+1, Pattern, crash_cymbal),
		    State#state{ pattern = Pattern1 };
		_ ->
		    State
	    end;
       true ->
	    State
    end.

%% FIXME: adjust beat volume
toggle_element(I, Pattern, Drum) ->
    Tuple = maps:get(Drum, Pattern),
    Tuple1 = case element(I, Tuple) of
		 0 -> setelement(I, Tuple,  1);
		 _N -> setelement(I, Tuple, 0)
	     end,
    maps:put(Drum, Tuple1, Pattern).

%% mark current tick red and clear previous tick
draw_tick(I,N,Fill,BFont) ->
    Left = 8,
    W    = 10,
    H    = 10,
    X0 = if I == 1 -> Left+40+(N-1)*(W+1);
	    true -> Left+40+(I-2)*(W+1)
	 end,
    X1    = Left+40+(I-1)*(W+1),
    Y    = 0,

    epx_gc:set_fill_style(solid),
    epxw:draw(top, 
	      fun(Pixels, _Rect) ->
		      epx_gc:set_fill_color(?TOP_BAR_COLOR),
		      epx:draw_rectangle(Pixels,X0+1,Y+1,W-2,H-2),

		      epx_gc:set_fill_color(white),
		      epx:draw_rectangle(Pixels,X1+1,Y+1,W-2,H-2),

		      if Fill ->
			      {Font,Ascent} = BFont,
			      epx_gc:set_border_width(2),
			      epx_gc:set_border_color(black),
			      epx_gc:set_fill_color(white),
  			      epx:draw_ellipse(Pixels,240,8,48,48),
			      epx_gc:set_font(Font),
			      epx_gc:set_foreground_color(?BLACK_TEXT_COLOR),
			      epx:draw_string(Pixels, 244, 4+Ascent, "F"),
			      epx_gc:set_border_width(0);
			 true ->
			      epx_gc:set_fill_color(?TOP_BAR_COLOR),
			      epx:draw_rectangle(Pixels,240,6,52,52)
		      end
	      end).

    
button_press({button_press, [left], Pos={_X,_Y}}, State) ->
    WPos = {Xw,Yw} = epxw:view_to_window_pos(Pos),
    if  Yw >= 0, Yw < ?TOP_BAR_SIZE ->
	    epxw:invalidate(),
	    toggle_grid(WPos, State);
	Xw >= 0, Xw < ?LEFT_BAR_SIZE ->
	    case find_button(WPos, State) of
		false ->
		    io:format("nothing found @Pos=~w\n", [WPos]),
		    State;
		{ok,ID} ->
		    io:format("found button ~p\n", [ID]),
		    toggle_button(ID, State)
	    end;
       true ->
	    io:format("~w\n", [WPos]),
	    State
    end;
button_press(_Event, State) ->
    State.

key_press({key_press, Sym, _Mod, _Code}, State) ->
    case Sym of
	$k ->
	    play(closed_hi_hat, State);
	$j ->
	    play(snare_drum, State);
	$a ->
	    play(bass_drum, State);
	$s ->
	    play(open_hi_hat, State);
	$l ->
  	    play(crash_cymbal, State);
	$f ->
	    io:format("fill = true\n"),
  	    State#state { fill_req = true };
	up ->
	    update_bpm_value(+1, State);
	down ->
	    update_bpm_value(-1, State);
	pageup ->
	    update_bpm_value(+10, State);
	pagedown ->
	    update_bpm_value(-10, State);
	$\s ->
	    toggle_pause(State);
	$\r ->
	    State#state { count = 1 };
	_ ->
	    io:format("key ~p not handled\n", [Sym]),
	    State
    end.

menu({menu,_Pos}, State) ->
    ?verbose("MENU: Pos = ~p\n", [_Pos]),
    Path = filename:join([code:lib_dir(midi),"tools","DrumSet"]),
    {ok, List} = file:list_dir(Path),
    MProfile = State#state.menu_profile,
    MenuList = [{"Default", {load_drumset, "Default"}} |
		[{Set, {load_drumset,Set}} || Set <- List -- ["Default"]]],
    Menu = epx_menu:create(MProfile#menu_profile{background_color=green},
			   MenuList),
    {reply, Menu, State}.

command(Command, Mod, State) ->
    ?verbose("Command: ~p, mod:~p\n", [Command, Mod]),
    case Command of
	{load_drumset, Name} ->
	    Dir = filename:join(State#state.drum_set_dir, Name),
	    {Drums,Volums} = load_drums(Dir, State#state.drum_map),
	    {noreply, State#state { drum_map = Drums,
				    volume_map = Volums }};
	_ ->
	    {noreply, State}
    end.

play(Drum, State) ->
    case maps:get(Drum, State#state.drum_map, undefined) of
	undefined -> State;
	ID ->
	    alsa_play:restart(ID),
	    alsa_play:run(ID),
	    State
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_menu_profile(Profile) ->
    #menu_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.menu_font_name,
       font_size        = Profile#profile.menu_font_size,
       font_color       = Profile#profile.menu_font_color,
       background_color = Profile#profile.menu_background_color,
       border_color     = Profile#profile.menu_border_color
      }.

%% load #profile from environment
load_profile(E) ->
    D = #profile{},
    S = load_value(scheme, E, D),
    #profile {
       scheme          = S,
       border_width     = load_value(border_width, E, D),
       border_color     = load_color(S,border_color, E, D),
       select_color     = load_color(S,select_color, E, D),
       select_border_width = load_value(select_border_width,E,D),
       select_border_color = load_color(S,select_border_color,E,D),
       highlight_color     = load_color(S,highlight_color,E,D),
       highlight_border_width = load_value(highlight_border_width,E,D),
       highlight_border_color = load_color(S,highlight_border_color,E,D),
       label_font = load_value(label_font, E, D),
       label_font_size = load_value(label_font_size, E, D),
       label_font_color = load_color(S,label_font_color,E,D),
       menu_font_name = load_value(menu_font_name, E, D),
       menu_font_size = load_value(menu_font_size, E, D),
       menu_font_color = load_color(S,menu_font_color,E,D),
       menu_background_color = load_color(S,menu_background_color,E,D),
       menu_border_color = load_color(S,menu_border_color,E,D)
      }.

%% map of profile record names and indices in the record
profile_index() ->
    maps:from_list(lists:zip(record_info(fields, profile), 
			     lists:seq(2,record_info(size,profile)))).
%% load value from environment or profile
load_value(Key, Env, Profile) ->
    case proplists:get_value(Key, Env) of
	undefined ->
	    Ix = profile_index(),
	    Pos = maps:get(Key, Ix),
	    element(Pos, Profile);
	Value ->
	    Value
    end.

%% load RGB value from environment or profile
load_color(Scheme, Key, Env, Profile) ->
    Value = load_value(Key, Env, Profile),
    epx_profile:color(Scheme, Value).


-define(WHITE_KEY_WIDTH, 32).
-define(WHITE_KEY_HEIGHT, 200).
-define(BLACK_KEY_WIDTH,  trunc(?WHITE_KEY_WIDTH*0.8)).
-define(BLACK_KEY_HEIGHT, trunc(?WHITE_KEY_HEIGHT*0.6)).
-define(FINGER_WIDTH, (?BLACK_KEY_WIDTH-2)).
-define(NUM_OCTAVES, 2).
-define(NOTE_FIRST, 48).  %% C3
-define(NOTE_LAST,  (?NOTE_FIRST + 12*?NUM_OCTAVES - 1)).
-define(NOTE_WIDTH, 20).

draw_octave(Pixels,N) ->
    X0 = ?KEYBOARD_X0,
    Y0 = ?KEYBOARD_Y0,

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
draw_active(Pixels, _N, Active) ->
    X0 = ?KEYBOARD_X0,
    Y0 = ?KEYBOARD_Y0,
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

draw_notes(Pixels, _N, #state{active=Active,
			     sharp_font={Font,Ascent},
			     g_clef=Gclef}) ->
    IWidth = 320,
    IHeight = 628,
    epx:pixmap_scale_area(Gclef, Pixels, ?GCLEF_X0, ?GCLEF_Y0,
			  IWidth/4, IHeight/4, [blend]),
    Step  = 22,
    Xoffs = ?GCLEF_X0 - 4, %% 216,
    Yoffs = ?GCLEF_Y0 + 31, %% 64,

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
    epx_gc:set_foreground_color(?BLACK_TEXT_COLOR),
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
%% - remove potential play along bass notes;
%%      [R,R+12|Ns] -> [R+12|Ns]
%%      [R,R+12,R+24,...] -> [R+24|Ns]
%% - check timing 
%%
match_chord(MatchChord, Active) ->
    NTs = maps:fold(fun({_Chan,Note},{_V,Ts},Acc) -> [{Note,Ts}|Acc] end, 
		    [], Active),
    NTs1 = normalize_notes(NTs),
    Ns1 = [N || {N,_T} <- NTs1],

    %% check the chord time diff
    Ts1 = [T || {_N,T} <- NTs1],
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


%% sort and remove "play along" one/two bass notes
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
