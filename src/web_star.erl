%%%-------------------------------------------------------------------
%%% File    : web_gamesyn.erl
%%% Author  : asceth <machinist@asceth.com>
%%% Description : Server process that handles query requests internally
%%%                and provides the fun for the mochiweb loop.
%%%
%%% Created :  9 Sep 2008 by asceth <machinist@asceth.com>
%%%-------------------------------------------------------------------
-module(web_star).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% External API
-export([setup/1]).
-export([start/1, stop/1]).
-export([loop/3]).

-record(state, {config={},
                web_exchange,
                web_exchange_name}).

-include("logger.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Options) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% External API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: loop(Req, DocRoot) -> void()
%% Description: Loop for the mochiweb http server
%%--------------------------------------------------------------------
loop(Config, WebExchange, Req) ->
  statistics(wall_clock),
  Path = Req:get(path),
  PathTokens = string:tokens(Path, "/"),
  Method = case Req:get(method) of
             'GET' ->
               get;
             'HEAD' ->
               get;
             'POST' ->
               PostParams = Req:parse_post(),
               case lists:keysearch("_method", 1, PostParams) of
                 {value, {"_method", "put"}} ->
                   put;
                 {value, {"_method", "delete"}} ->
                   delete;
                 _UnknownNone ->
                   post
               end;
             'PUT' ->
               put;
             'DELETE' ->
               delete;
             _Unknown ->
               error
           end,
  case Method of
    error ->
      Req:respond({501, [], []});
    RouteMethod ->
      Response = try web_star_cycle:do_request(Config, WebExchange, Method, PathTokens, Req)
                 catch
                   throw:{route_error, StatusCode, _Data} ->
                     ?ERROR_MSG("~p~nError: ~p~nTrace: ~p~n~n", [httpd_util:rfc1123_date(erlang:universaltime()), route_error, erlang:get_stacktrace()]),
                     web_star_cycle:do_status(WebExchange, request_error, StatusCode, []);
                   error:function_clause ->
                     ?ERROR_MSG("~p~nError: ~p~nTrace: ~p~n~n", [httpd_util:rfc1123_date(erlang:universaltime()), function_clause, erlang:get_stacktrace()]),
                     web_star_cycle:do_status(WebExchange, request_error, 403, []);
                   error:Error ->
                     ?ERROR_MSG("~p~nError: ~p~nTrace: ~p~n~n", [httpd_util:rfc1123_date(erlang:universaltime()), Error, erlang:get_stacktrace()]),
                     web_star_cycle:do_status(WebExchange, request_error, 500, [])
                 end,
      {_, Time1} = statistics(wall_clock),
      U1 = Time1 / 1000,
      ReqsASec = case U1 of
                   0.0 ->
                     0;
                   Other ->
                     1 / Other
                 end,
      ?INFO_MSG("==== SERVING ====~nPath: ~p~nRequest Time: ~p (~p reqs/s)~n",
                [Path, U1, ReqsASec]),
      {status, Status, headers, Headers, body, Body} = Response,
      Req:respond({Status, Headers, Body})
  end.

setup(Config) ->
  gen_server:call(?SERVER, {setup, Config}).

start(WebApplication) ->
  gen_server:call(?SERVER, {start, WebApplication}).

stop(WebApplication) ->
  gen_server:call(?SERVER, {stop, WebApplication}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% WebConfig = [{ip, "127.0.0.1"}, {port, 8000}, {docroot, "/srv/web/gamesyn.com/public"}, {web_router, web_gamesyn_router}].
%%--------------------------------------------------------------------
init([]) ->
  application:start(inets),
  application:start(mochiweb),
  application:start(mnesia),
  application:start(web_router),
  application:start(web_sessions),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({setup, Config}, _From, State) ->
  Reply = do_setup(Config),
  {reply, Reply, State#state{config=Config}};
handle_call({start, WebApplication}, From, State)  ->
  spawn(fun() -> start_application(WebApplication, From, State) end),
  {noreply, State};
handle_call({stop, WebApplication}, From, State)  ->
  spawn(fun() -> stop_application(WebApplication, From, State) end),
  {noreply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{config={}} = _State) ->
  ok;
terminate(_Reason, #state{config=Config, web_exchange_name=WebExchangeName} = _State) ->
  mochiweb_http:stop(?SERVER),
  web_router_exchange:delete(WebExchangeName, false),
  case proplists:get_value(eda, Config, none) of
    none ->
      ok;
    DatabaseConfig ->
      eda:disconnect(proplists:get_value(pool_name, DatabaseConfig))
  end,
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_setup(Config) ->
  do_setup(web_router, Config).
do_setup(web_router, Config) ->
  case proplists:get_value(web_exchange_name, Config) of
    undefined ->
      {error, no_exchange};
    WebExchangeName ->
      WebExchange = web_router_exchange:declare(WebExchangeName, topic, true, false),
      do_setup(web_sessions, [{web_exchange, WebExchange}|Config])
  end;
do_setup(web_sessions, Config) ->
  WebExchangeName = proplists:get_value(web_exchange_name, Config),
  web_sessions:setup(web_application:config(web_sessions, Config)),
  web_router:load_bindings(WebExchangeName, web_sessions:routes("")),
  do_setup(eda, Config);
do_setup(eda, Config) ->
  case proplists:get_value(eda, Config, none) of
    none ->
      do_setup(loop, Config);
    DatabaseConfig ->
      application:start(eda),
      PoolName = proplists:get_value(pool_name, DatabaseConfig),
      Driver = proplists:get_value(driver, DatabaseConfig),
      User = proplists:get_value(user, DatabaseConfig),
      Password = proplists:get_value(password, DatabaseConfig),
      Repository = proplists:get_value(repository, DatabaseConfig),
      Options = proplists:get_value(options, DatabaseConfig, []),
      PoolSize = proplists:get_value(pool_size, DatabaseConfig, 21),
      eda:connect(PoolName, Driver, User, Password, Repository, Options, PoolSize),
      do_setup(loop, Config)
  end;
do_setup(loop, Config) ->
  WebExchange = proplists:get_value(web_exchange, Config),
  ServerConfig = web_application:config(server, Config),
  % Start up listening loop
  {A1, A2, A3} = now(),
  random:seed(A1, A2, A3),
  Loop = fun(Req) ->
             ?SERVER:loop(Config, WebExchange, Req)
         end,
  mochiweb_http:start([{loop, Loop} | ServerConfig]),
  ok.


start_application(WebApplication, From, #state{config=Config, web_exchange_name=WebExchangeName} = _State) ->
  case code:lib_dir(WebApplication) of
    {error, bad_name} ->
      gen_server:reply(From, {error, bad_name});
    _LibDir ->
      application:start(WebApplication),
      WebApplication:setup(web_application:config(WebApplication, Config)),
      web_router:load_bindings(WebExchangeName, WebApplication:routes(web_application:prefix(WebApplication, Config))),
      gen_server:reply(From, ok)
  end.

stop_application(WebApplication, From, _State) ->
  case code:lib_dir(WebApplication) of
    {error, bad_name} ->
      gen_server:reply(From, {error, bad_name});
    _LibDir ->
      application:stop(WebApplication),
      gen_server:reply(From, ok)
  end.


