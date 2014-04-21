%%%-------------------------------------------------------------------
%%% @author HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%% @copyright (C) 2013, HIROE Shin
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2013 by HIROE Shin <shin@HIROE-no-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(gpio_port).

%% API
-export([start_link/0, stop/0, init/1]).
-export([start_poll/2,
	 pullup/1,
	 pulldown/1,
	 pullnone/1]).

-export([foo/1, bar/1]).

-define(SERVER, ?MODULE).
-define(GPIO_DRIVER, "gpio_drv").
-define(PULL_NONE, 0).
-define(PULL_DOWN, 1).
-define(PULL_UP, 2).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> term() | {error, Reason} when
      Reason :: term().
start_link() ->
    case erl_ddll:load_driver(priv_dir(), ?GPIO_DRIVER) of
	ok -> ok;
	{error, already_loaded} -> ok;
	_ -> exit({error, could_not_load_driver})
    end,
    proc_lib:start_link(?MODULE, init, [[self(), ?GPIO_DRIVER]]).

%%--------------------------------------------------------------------
%% @doc Initialize and goto receive loop.
%% @end
%%--------------------------------------------------------------------
-spec init(Args) -> no_return() when
      Args :: list().
init([Parent, SharedLib]) ->
    register(?SERVER, self()),
    Port = open_port({spawn, SharedLib}, [{packet, 2}]),
    proc_lib:init_ack(Parent, {ok, self()}),
    timer:send_interval(100, <<"tick">>), %% Reason writed at comment of loop/1.
    loop(Port).

%%--------------------------------------------------------------------
%% @doc Stop server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    ?SERVER ! stop.

%%--------------------------------------------------------------------
%% @doc Debug function
%% @end
%%--------------------------------------------------------------------
-spec foo(number()) -> number().
foo(X) ->
    call_port({foo, X}).

%%--------------------------------------------------------------------
%% @doc Debug function
%% @end
%%--------------------------------------------------------------------
-spec bar(number()) -> number().
bar(Y) ->
    call_port({bar, Y}).

%%--------------------------------------------------------------------
%% @doc Start Polling to pin (exec poll system call in port driver).
%% @end
%%--------------------------------------------------------------------
-spec start_poll(PinNo, Mode) -> ok | {error, start_poll_failed} when
      PinNo :: non_neg_integer(),
      Mode :: rising | falling | both.
start_poll(PinNo, Mode) when is_integer(PinNo), is_atom(Mode) ->
    case call_port({start_poll, PinNo, Mode}) of
	1 -> ok;
	_ -> {error, start_poll_failed}
    end.

%%--------------------------------------------------------------------
%% @doc Pull up pin.
%% @end
%%--------------------------------------------------------------------
-spec pullup(PinNo) -> ok when
      PinNo :: non_neg_integer().
pullup(PinNo) ->
    call_port({pullup_down, PinNo, pullup}).

%%--------------------------------------------------------------------
%% @doc Pull down pin.
%% @end
%%--------------------------------------------------------------------
-spec pulldown(PinNo) -> ok when
      PinNo :: non_neg_integer().
pulldown(PinNo) ->
    call_port({pullup_down, PinNo, pulldown}).

%%--------------------------------------------------------------------
%% @doc Disable pullup and pulldown.
%% @end
%%--------------------------------------------------------------------
-spec pullnone(PinNo) -> ok when
      PinNo :: non_neg_integer().
pullnone(PinNo) ->
    call_port({pullup_down, PinNo, none}).

%%--------------------------------------------------------------------
%% @private
%% @doc Disable pullup and pulldown.
%% @end
%%--------------------------------------------------------------------
-spec call_port(term()) -> term().
call_port(Msg) ->
    ?SERVER ! {call, self(), Msg},
    receive
	{?SERVER, Result} -> Result
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Receive loop.
%%
%% 理由はわかりませんが、通常のメッセージ受信方法ではポートドライバでpollシステム
%% コールを実行した後の、valueファイルが読み込み可能になった場合のメッセージを
%% 受信できませんでした。
%% ただし、オーナープロセスがなんらかのメッセージを受信すると同時にポートドライバ
%% からのメッセージも受信する為、現在はタイマーで意味の無いメッセージを自身に送る
%% ようにしています。
%% この所作は、原因が分かりこのような事をする必要が無くなったら消します。
%% @end
%%--------------------------------------------------------------------
-spec loop(port()) -> no_return().
loop(Port) ->
    receive
	<<"tick">> ->
	    loop(Port);	    

	{call, Caller, Msg} ->
	    Port ! {self(), {command, encode(Msg)}},
	    receive
		{Port, {data, Data}} -> Caller ! {?SERVER, decode(Data)}
	    end,
	    loop(Port);

	{gpio_changed, Port, Pin, _Edge} ->
	    gpio_pin:digital_change_notify(Pin),
	    loop(Port);

	{Port, {data, _Data}} ->
	    loop(Port);

	stop ->
	    Port ! {self(), close},
	    receive
		{Port, closed} -> exit(normal)
	    end;

	{'EXIT', Port, Reason} ->
	    io:format("~p ~n", [Reason]),
	    exit(port_terminated);

	Other ->
	    io:format("unknown message: ~p~n", [Other]),
	    loop(Port)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

priv_dir() ->
    case code:priv_dir(gpio) of
	{error, bad_name} ->
	    "./priv";
	D ->
	    D
    end.

encode({foo, X}) -> [1, X];
encode({bar, Y}) -> [2, Y];
encode({start_poll, Pin, rising})    -> [3, Pin, 1];
encode({start_poll, Pin, falling})   -> [3, Pin, 2];
encode({start_poll, Pin, both})      -> [3, Pin, 3];
encode({pullup_down, Pin, none})     -> [4, Pin, ?PULL_NONE];
encode({pullup_down, Pin, pulldown}) -> [4, Pin, ?PULL_DOWN];
encode({pullup_down, Pin, pullup})   -> [4, Pin, ?PULL_UP].

decode([Int]) -> Int.
