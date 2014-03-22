%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 19 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(gpio_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/4, start_link/5]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
-spec start_link() -> {ok, Pid} | ignore | {error, Error} when
      Pid :: pid(),
      Error :: term().
start_link() ->
    {ok, IOList} = application:get_env(gpio, gpio),
    {ok, AnalogList} = application:get_env(gpio, analog_list),
    {ok, AnalogInterval} = application:get_env(gpio, analog_interval),
    EventHandlers = [ {gpio_sample_event_handler, []} ],
    start_link(IOList, AnalogList, AnalogInterval, EventHandlers).

-spec start_link(IOList, AnalogList,  AnalogInterval, EventHandlers) ->
			{ok, Pid}     |
			ignore        |
			{error, Error} when
      IOList :: [ {non_neg_integer(), in | out} ],
      AnalogList :: [non_neg_integer()],
      AnalogInterval :: pos_integer(),
      EventHandlers :: [atom()],
      Pid :: pid(),
      Error :: term().
start_link(IOList, AnalogList, AnalogInterval, EventHandlers) ->
    {ok, C_Node} = application:get_env(gpio, c_node),
    start_link(IOList, AnalogList, AnalogInterval, EventHandlers, C_Node).

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(IOList, AnalogList, AnalogInterval, EventHandlers, C_Node) -> 
			{ok, Pid}     |
			ignore        |
			{error, Error} when
      IOList :: [ {non_neg_integer(), in | out} ],
      AnalogList :: [non_neg_integer()],
      AnalogInterval :: pos_integer(),
      EventHandlers :: [atom()],
      C_Node :: atom(),
      Pid :: pid(),
      Error :: term().
start_link(IOList, AnalogList, AnalogInterval, EventHandlers, C_Node) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, 
			  [IOList, AnalogList, AnalogInterval, 
			   EventHandlers, C_Node]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([IOList, AnalogList, AnalogInterval, EventHandlers, C_Node]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    Childs = 
	[port_spec(C_Node)] ++ [db_child(IOList), event_spec(EventHandlers)] ++
	child_list(IOList) ++ gpio_adc_list(AnalogList, AnalogInterval),

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

gpio_adc_list(AnalogList, AnalogInterval) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    [ {{gpio_adc, AnalogNo}, {gpio_adc, start_link, [AnalogNo, AnalogInterval]},
       Restart, Shutdown, Type, [gpio_adc]}
      || AnalogNo <- AnalogList ].

event_spec(EventHandlers) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {gpio_pin_event, {gpio_pin_event, start_link, [EventHandlers]},
     Restart, Shutdown, Type, [gpio_port]}.

port_spec(C_Node) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {gpio_port, {gpio_port, start_link, [C_Node]},
     Restart, Shutdown, Type, [gpio_port]}.

db_child(IOList) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {gpio_pin_db, {gpio_pin_db, start_link, [IOList]},
     Restart, Shutdown, Type, [gpio_pin_db]}.

child_list(IOList) ->
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    L1 = [ {{gpio_pin, PinNo}, {gpio_pin, start_link, [{PinNo, Mode, Opts}]},
	    Restart, Shutdown, Type, [gpio_pin]}
	   || {PinNo, Mode, Opts} <- IOList ],

    L2 = [ {{gpio_pin, PinNo}, {gpio_pin, start_link, [{PinNo, Mode, []}]},
	    Restart, Shutdown, Type, [gpio_pin]}
	   || {PinNo, Mode} <- IOList ],

    L1 ++ L2.
    
