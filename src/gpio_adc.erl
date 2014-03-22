%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2014, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 22 Mar 2014 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(gpio_adc).

-behaviour(gen_server).

%% API
-export([start_link/2,
	 read/1,
	 check_analog_value/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, { analog_no :: non_neg_integer(),
		 timer_ref :: timer:tref(),
		 file_io   :: file:io_device() }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(non_neg_integer(), non_neg_integer()) -> {ok, pid()}    |
							  ignore         |
							  {error, term()}.
      
start_link(AnalogNo, AnalogInterval) when is_integer(AnalogNo),
					  is_integer(AnalogInterval) ->
    gen_server:start_link(?MODULE, [AnalogNo, AnalogInterval], []).

%%--------------------------------------------------------------------
%% @doc read gpio value.
%% @end
%%--------------------------------------------------------------------
-spec read(AnalogNo) -> Val when
      AnalogNo :: non_neg_integer(),
      Val :: 0 | 1.
read(AnalogNo) when is_integer(AnalogNo) ->
    gen_server:call(get_child(AnalogNo), read).

%%--------------------------------------------------------------------
%% @doc call from timer:apply_interval/4.
%% @end
%%--------------------------------------------------------------------
check_analog_value(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, check_analog_value).

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
init([AnalogNo, AnalogInterval]) ->
    ok = init_galileo(AnalogNo),
    {ok, FileIO} = file:open(value_file(AnalogNo), [read]),
    {ok, TRef} = timer:apply_interval(AnalogInterval, ?MODULE, 
				      check_analog_value, [self()]),
    {ok, #state{analog_no = AnalogNo, timer_ref = TRef, file_io = FileIO}}.

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
handle_call(read, _From, #state{file_io = FileIO} = State) ->
    Str = read_row(FileIO),
    Val = list_to_integer(Str),
    {reply, Val, State}.

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
handle_cast(check_analog_value, #state{analog_no = AnalogNo,
				       file_io = FileIO} = State) ->
    Str = read_row(FileIO),
    Val = list_to_integer(Str),
    gen_event:notify(gpio_pin_event, {analog_recv, AnalogNo, Val}),
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
terminate(_Reason, _State) ->
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

    Fun = fun({{gpio_adc, No}, _, _, _}) ->
		  No =:= PinNo;
	     ({_, _, _, _}) ->
		  false
	  end,

    case lists:filter(Fun, List) of
	[] ->
	    {error, not_started};
	[ {{gpio_adc, PinNo}, Pid, _, _}] ->
	    Pid
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc initialize Galileo ADC.
%%
%% see http://www.malinov.com/Home/sergey-s-blog/intelgalileo-programminggpiofromlinux
%% @end
%%--------------------------------------------------------------------
-spec init_galileo(non_neg_integer()) -> ok.
init_galileo(AnalogNo) when AnalogNo =:= 4; AnalogNo =:= 5 ->
    set_selected(29, "1"),                         %% I2C_MUX(gpio29=1)
    set_selected(adc_selected_pin(AnalogNo), "0"), %% Selected by A(0-5)_MUX
    ok;

init_galileo(AnalogNo) ->
    set_selected(adc_selected_pin(AnalogNo), "0"), %% Selected by A(0-5)_MUX
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc ADC selected setting.
%% @end
%%--------------------------------------------------------------------
-spec set_selected(PinNo, Val) -> ok when
      PinNo :: non_neg_integer(),
      Val :: list().
set_selected(PinNo, Val) ->
    io:format("set_selected:~p -> ~p~n", [PinNo, Val]),
    gpio_pin:unexport(PinNo), timer:sleep(300),
    gpio_pin:export(PinNo),   timer:sleep(300),   %% waiting for file created...
    gpio_pin:set_mode(PinNo, out),
    FileName = gpio_pin:gpio_filename(PinNo, "value"),
    {ok, FileIO} = file:open(FileName, [write]),
    ok = file:write(FileIO, Val),
    ok = file:close(FileIO),
    gpio_pin:unexport(PinNo).

%%--------------------------------------------------------------------
%% @private
%% @doc ADC setting pin no.
%% @end
%%--------------------------------------------------------------------
-spec adc_selected_pin(non_neg_integer()) -> non_neg_integer().
adc_selected_pin(0) -> 37;
adc_selected_pin(1) -> 36;
adc_selected_pin(2) -> 23;
adc_selected_pin(3) -> 22;
adc_selected_pin(4) -> 21;
adc_selected_pin(5) -> 20.    

%%--------------------------------------------------------------------
%% @private
%% @doc analog value file path.
%% @end
%%--------------------------------------------------------------------
-spec value_file(non_neg_integer()) -> string().
value_file(AnalogNo) ->
    string:join(["/sys/bus/iio/devices/iio:device0/in_voltage",
		 integer_to_list(AnalogNo), "_raw"], "").

%%--------------------------------------------------------------------
%% @private
%% @doc read row data from device file.
%% @end
%%--------------------------------------------------------------------
-spec read_row(FileIO) -> string() when
      FileIO :: file:device_io().
read_row(FileIO) ->
    {ok, 0} = file:position(FileIO, 0),
    {ok, Str} = file:read_line(FileIO),
    string:strip(Str, right, $\n).
