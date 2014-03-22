%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@hibiscus>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 17 Nov 2013 by HIROE Shin <shin@hibiscus>
%%%-------------------------------------------------------------------
-module(gpio_pin).

-behaviour(gen_server).

%% API
-export([start_link/1,
         set_pin_mode/2,
         read/1,
         write/2,
         set_int/2,
         pullup/1,
         pulldown/1,
         pullnone/1,
         get_active_low/1,
         set_active_low/2,
         all_digital/0]).

-export([export/1, unexport/1, set_mode/2, gpio_filename/2]).

%% fort receive inturrupt
-export([digital_change_notify/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type mode() :: in | out.
-type edge() :: falling | rising | both | none.
-type pull() :: up | down | none | strong.

-define(SERVER, ?MODULE).

-record(state, {pin_no      :: non_neg_integer(),
                file_io     :: file:io_device(),
                edge        :: edge(),
                mode        :: mode(),
                pull        :: pull() }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link({PinNo, Mode} | {PinNo, Mode, Opts}) -> 
			{ok, Pid} |
			ignore    |
			{error, Error} when
      PinNo :: non_neg_integer(),
      Mode :: mode(),
      Opts :: [tuple()],
      Pid :: pid(),
      Error :: term().
start_link({PinNo, Mode}) when is_integer(PinNo),
			       (Mode =:= in orelse Mode =:= out) ->
    start_link({PinNo, Mode, []});

start_link({PinNo, Mode, Opts}) when is_integer(PinNo),
				     (Mode =:= in orelse Mode =:= out) ->
    gen_server:start_link(?MODULE, [{PinNo, Mode, Opts}], []).

%%--------------------------------------------------------------------
%% @doc set pin mode, in or out.
%% @end
%%--------------------------------------------------------------------
-spec set_pin_mode(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: mode().
set_pin_mode(PinNo, Mode) when Mode =:= in; 
			       Mode =:= out ->
    gen_server:call(get_child(PinNo), {set_pin_mode, Mode}).

%%--------------------------------------------------------------------
%% @doc read gpio value.
%% @end
%%--------------------------------------------------------------------
-spec read(PinNo) -> Val when
      PinNo :: non_neg_integer(),
      Val :: 0 | 1.
read(PinNo) when is_integer(PinNo) ->
    gen_server:call(get_child(PinNo), read).

%%--------------------------------------------------------------------
%% @doc write value to gpio.
%% @end
%%--------------------------------------------------------------------
-spec write(PinNo, Val) -> ok when
      PinNo :: non_neg_integer(),
      Val :: 0 | 1.      
write(PinNo, Val) when is_integer(PinNo) andalso is_integer(Val) ->
    gen_server:call(get_child(PinNo), {write, Val}).

%%--------------------------------------------------------------------
%% @doc set interrupt that fire when gpio's input or output status is chaned.
%% @end
%%--------------------------------------------------------------------
-spec set_int(PinNo, Mode) -> ok | {error, Reason} when
      PinNo :: non_neg_integer(),
      Mode :: edge(),
      Reason :: term().
set_int(PinNo, Mode) ->
    gen_server:call(get_child(PinNo), {set_int, Mode}).    

%%--------------------------------------------------------------------
%% @doc set pullup to a pin.
%%
%% RaspberryPi内蔵のプルアップ抵抗を用いてpinをプルアップ有りに設定します
%% 入力無しの状態で常時3.3Vの電圧がかかり、GNDと接地された場合のみ0となります
%% @end
%%--------------------------------------------------------------------
-spec pullup(PinNo) -> ok when
      PinNo :: non_neg_integer().
pullup(PinNo) ->
    gen_server:cast(get_child(PinNo), pullup).

%%--------------------------------------------------------------------
%% @doc set pulldown to a pin.
%%
%% RaspberryPi内蔵のプルダウン抵抗を用いてpinをプルダウン有りに設定します
%% 入力無しの状態で常時GND接地の0.0Vとなり、3.3Vと短絡された場合のみ1となります
%% @end
%%--------------------------------------------------------------------
-spec pulldown(PinNo) -> ok when
      PinNo :: non_neg_integer().
pulldown(PinNo) ->
    gen_server:cast(get_child(PinNo), pulldown).

%%--------------------------------------------------------------------
%% @doc release pin mode from pullup pulldown.
%%
%% RaspberryPi内蔵のプルアップ、プルダウン抵抗を用いません
%% 入力無しの状態では不安定な電圧となり、外部回路でプルアップまたはプルダウンが必要です
%% @end
%%--------------------------------------------------------------------
-spec pullnone(PinNo) -> ok when
      PinNo :: non_neg_integer().
pullnone(PinNo) ->
    gen_server:cast(get_child(PinNo), pullnone).

%%--------------------------------------------------------------------
%% @doc get active low from a pin.
%%
%% Mode=0: 通電時のread/1の結果は 通電->1 解放->0 (デフォルト)
%% Mode=1: 通電時のread/1の結果は 通電->0 解放->1
%% @end
%%--------------------------------------------------------------------
-spec get_active_low(PinNo) -> 0 | 1 when
      PinNo :: non_neg_integer().
get_active_low(PinNo) ->
    gen_server:call(get_child(PinNo), get_active_low).

%%--------------------------------------------------------------------
%% @doc set active low to a pin.
%%
%% Mode=1: active_lowを1に設定して、通電->0 解放->1 となるようにビット反転します
%% Mode=0: active_lowを0に設定して、通電->1 解放->0 となるようにします
%% @end
%%--------------------------------------------------------------------
-spec set_active_low(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: 0 | 1.
set_active_low(PinNo, Mode) ->
    gen_server:call(get_child(PinNo), {set_active_low, Mode}).

%%--------------------------------------------------------------------
%% @doc get status list.
%%
%% example: [0,1,1,0,0,0,0,0]
%% @end
%%--------------------------------------------------------------------
-spec all_digital() -> [ 1 | 0 ].
all_digital() ->
    {ok, GpioList} = application:get_env(gpio),
    all_digital(GpioList, []).

all_digital([], Result) ->
    lists:reverse(Result);

all_digital([{PinNo, _Mode, _Opts} | Tail], Result) ->
    all_digital(Tail, [gpio_pin:read(PinNo) | Result]).

%%--------------------------------------------------------------------
%% @doc receive digital state change notify from gpio_port.
%% @end
%%--------------------------------------------------------------------
digital_change_notify(PinNo) ->
    gpio_pin_db:update_digital_pin(PinNo, read(PinNo)).

%%%===================================================================
%%% Private API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc unexport gpio.
%% @end
%%--------------------------------------------------------------------
-spec unexport(PinNo) -> ok when
      PinNo :: non_neg_integer().
unexport(PinNo) ->
    {ok, FileIO} = file:open("/sys/class/gpio/unexport", [write]),
    case file:write(FileIO, io_lib:format("~w", [PinNo])) of
	ok -> ok;
	{error, einval} -> ok %% not exported.
    end,
    ok = file:close(FileIO).

%%--------------------------------------------------------------------
%% @doc export gpio.
%% @end
%%--------------------------------------------------------------------
-spec export(PinNo) -> ok when
      PinNo :: non_neg_integer().
export(PinNo) ->
    {ok, FileIO} = file:open("/sys/class/gpio/export", [write]),
    ok = file:write(FileIO, io_lib:format("~w", [PinNo])),
    ok = file:close(FileIO).

%%--------------------------------------------------------------------
%% @doc set open mode, in or out.
%% @end
%%--------------------------------------------------------------------
-spec set_mode(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: mode().
set_mode(PinNo, Mode) when Mode =:= in orelse
			   Mode =:= out ->
    FileName = gpio_filename(PinNo, "direction"),
    {ok, FileIO} = file:open(FileName, [write]),
    ok = file:write(FileIO, atom_to_list(Mode)),
    ok = file:close(FileIO).    

%%--------------------------------------------------------------------
%% @doc gpio device file name.
%% @end
%%--------------------------------------------------------------------
-spec gpio_filename(non_neg_integer(), string()) -> string().
gpio_filename(PinNo, FileName) ->
    string:join(["/sys/class/gpio/gpio", integer_to_list(PinNo), 
		 "/", FileName], "").

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{PinNo, Mode, Opts}]) ->
    Edge = proplists:get_value(edge, Opts, none), 
    Pull = proplists:get_value(pull, Opts),
    ActiveLow = proplists:get_value(active_low, Opts),
    ok = unexport(PinNo), timer:sleep(300), %% waiting for file deleted...
    ok = export(PinNo),   timer:sleep(300), %% waiting for file created...
    ok = set_mode(PinNo, Mode), 

    case Pull of
	up     -> set_pull(pullup,   PinNo);
	down   -> set_pull(pulldown, PinNo);
	none   -> set_pull(pullnone, PinNo);
	strong -> set_pull(strong, PinNo);
	undefined -> ok
    end,

    case ActiveLow of
	true  -> ok = write_active_low(PinNo, 1);
	false -> ok = write_active_low(PinNo, 0);
	undefined -> ok
    end,

    case Edge of
	none -> undefined;
	Edge when Edge =:= rising orelse
		  Edge =:= falling orelse
		  Edge =:= both ->
	    ok  = set_interrupt(PinNo, Edge)
    end,

    {ok, FileIO} = open(PinNo, Mode),
    {ok, #state{pin_no = PinNo, file_io = FileIO, edge = Edge, 
		mode = Mode, pull = Pull}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({set_pin_mode, Mode}, _From, State) ->
    PinNo = State#state.pin_no,
    ok = set_mode(PinNo, Mode),
    {ok, FileIO} = open(PinNo, Mode),
    {reply, ok, State#state{file_io = FileIO}};

%% read pin state
handle_call(read, _From, State) ->
    Reply = case read_row(State#state.file_io) of
		{ok, Val} -> list_to_integer(Val);
		{error, Reason} -> {error, Reason}
	    end,

    {reply, Reply, State};

%% write pin state
handle_call({write, Val}, _From, State) ->
    FileIO = State#state.file_io,
    {ok, 0} = file:position(FileIO, 0),

    Reply = case file:write(FileIO, integer_to_list(Val)) of
		ok -> ok;
		{error, Reason} -> {error, Reason}
	    end,

    {reply, Reply, State};

% set interrupt
handle_call({set_int, Mode}, _From, #state{pin_no = PinNo} = State) ->
    Reply = set_interrupt(PinNo, Mode),
    {reply, Reply, State};

handle_call(get_active_low, _From, #state{pin_no = PinNo} = State) ->
    Reply = read_active_low(PinNo),
    {reply, Reply, State};

%% set active_low
handle_call({set_active_low, Mode}, _From, #state{pin_no = PinNo} = State) when
      Mode =:= 0 orelse
      Mode =:= 1 ->
    Reply = write_active_low(PinNo, Mode),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(pullup, #state{pin_no = PinNo} = State) ->
    set_pull(pullup, PinNo),
    {noreply, State};

handle_cast(pulldown, #state{pin_no = PinNo} = State) ->
    set_pull(pulldown, PinNo),
    {noreply, State};

handle_cast(pullnone, #state{pin_no = PinNo} = State) ->
    set_pull(pullnone, PinNo),
    {noreply, State};

handle_cast(strong, #state{pin_no = PinNo} = State) ->
    set_pull(strong, PinNo),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ok = unexport(State#state.pin_no),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc get child pid from supervisor.
%% @end
%%--------------------------------------------------------------------
-spec get_child(PinNo) -> Pid | {error, not_started} when
      PinNo :: non_neg_integer(),
      Pid :: pid().
get_child(PinNo) ->
    List = supervisor:which_children(gpio_sup),

    Fun = fun({{gpio_pin, No}, _, _, _}) ->
		  No =:= PinNo;
	     ({_, _, _, _}) ->
		  false
	  end,

    case lists:filter(Fun, List) of
	[] ->
	    {error, not_started};
	[ {{gpio_pin, PinNo}, Pid, _, _}] ->
	    Pid
    end.

%%--------------------------------------------------------------------
%% @doc open device file.
%% @end
%%--------------------------------------------------------------------
-spec open(PinNo, Mode) -> ok | {error, Reason} when
      PinNo :: non_neg_integer(),
      Mode :: mode(),
      Reason :: term().
open(PinNo, Mode) ->
    OpenModes = case Mode of
		   out -> [read, write];
		   in  -> [read]
	       end,

    FileName = gpio_filename(PinNo, "value"),
    file:open(FileName, OpenModes).

%%--------------------------------------------------------------------
%% @private
%% @doc read row data from device file.
%% @end
%%--------------------------------------------------------------------
-spec read_row(FileIO) -> ok | {error, Reason} when
      FileIO :: file:device_io(),
      Reason :: term().
read_row(FileIO) ->
    {ok, 0} = file:position(FileIO, 0),
    file:read(FileIO, 1).

%%--------------------------------------------------------------------
%% @private
%% @doc set interrupt to gpio pin.
%% @end
%%--------------------------------------------------------------------
-spec set_interrupt(PinNo, Mode) -> ok | {error, Reason} when
      PinNo :: non_neg_integer(),
      Mode :: edge(),      
      Reason :: term().
set_interrupt(PinNo, Mode) ->
    ok = set_int_mode(PinNo, Mode),
    gpio_port:start_poll(PinNo, Mode).

%%--------------------------------------------------------------------
%% @private
%% @doc export gpio.
%% @end
%%--------------------------------------------------------------------
-spec set_int_mode(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: edge().
set_int_mode(PinNo, Mode) ->
    FileName = gpio_filename(PinNo, "edge"),
    {ok, FileIO} = file:open(FileName, [write]),
    ok = file:write(FileIO, atom_to_list(Mode)),
    ok = file:close(FileIO).

%%--------------------------------------------------------------------
%% @private
%% @doc read active low mode to device file.
%% @end
%%--------------------------------------------------------------------
-spec read_active_low(PinNo) -> 0 | 1 when
      PinNo :: non_neg_integer().
read_active_low(PinNo) ->
    FileName = gpio_filename(PinNo, "active_low"),
    {ok, FileIO} = file:open(FileName, [read]),
    {ok, ActiveLowStr} = file:read(FileIO, 1),
    ok = file:close(FileIO),
    case string:to_integer(ActiveLowStr) of
	{0, []} -> 0;
	{1, []} -> 1
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc write active low mode to device file.
%% @end
%%--------------------------------------------------------------------
-spec write_active_low(PinNo, Mode) -> ok when
      PinNo :: non_neg_integer(),
      Mode :: 0 | 1.
write_active_low(PinNo, Mode) ->
    FileName = gpio_filename(PinNo, "active_low"),
    {ok, FileIO} = file:open(FileName, [write]),
    ok = file:write(FileIO, integer_to_list(Mode)),
    ok = file:close(FileIO),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc API file for pullup pulldown string, on Galileo.
%% @end
%%--------------------------------------------------------------------
-spec drive_file(non_neg_integer()) -> string().
drive_file(PinNo) ->
    gpio_filename(PinNo, "drive").

%%--------------------------------------------------------------------
%% @private
%% @doc Return true if drive file exist (equal use Galileo).
%% @end
%%--------------------------------------------------------------------
-spec drive_file_exist(non_neg_integer()) -> boolean().
drive_file_exist(PinNo) ->
    file_exist(drive_file(PinNo)).
    
%%--------------------------------------------------------------------
%% @private
%% @doc Set pull mode to GPIO pin.
%% @end
%%--------------------------------------------------------------------
-spec set_pull(Mode, PinNo) -> ok when
      Mode :: pullup | pulldown | pullnone | strong,
      PinNo :: non_neg_integer().
set_pull(Mode, PinNo) ->
    case drive_file_exist(PinNo) of %% Galileo
	true ->
	    FileName = drive_file(PinNo),
	    {ok, FileIO} = file:open(FileName, [write]),
	    ok = file:write(FileIO, atom_to_list(Mode)),
	    ok = file:close(FileIO);
	false ->
	    case Mode of
		pullup   -> gpio_port:pullup(PinNo);
		pulldown -> gpio_port:pulldown(PinNo);
		pullnone -> gpio_port:pullnone(PinNo)
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Return true if file exist, return false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec file_exist(FilePath) -> boolean() when
      FilePath :: file:name_all().
file_exist(FilePath) ->
    filelib:is_regular(FilePath).
