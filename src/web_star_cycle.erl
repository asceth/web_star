-module(web_star_cycle).

-include("logger.hrl").
-export([do_request/5, do_status/4, do_error/4]).

do_request(Config, WebExchange, Method, PathTokens, Req) ->
  Session = hook_session(Config, WebExchange, Method, PathTokens, Req),
  PreSession = hook_pre_request_global(WebExchange, Session),
  SessionFinal = hook_post_request(WebExchange, PreSession),
  {status, web_session:flash_lookup(SessionFinal, "status"), headers, web_session:flash_lookup(SessionFinal, "headers"), body, web_session:flash_lookup(SessionFinal, "body")}.

hook_session(Config, WebExchange, Method, PathTokens, Req) ->
  RoutingKey = web_router:key([session]),
  [Session] = web_router_exchange:route(WebExchange, RoutingKey,
                                        [[{"method", Method}, {"path_tokens", PathTokens}, {"request", Req}]]),
  web_session:flash_merge_now(Session, Config).

hook_pre_request_global(WebExchange, Session) ->
  Method = web_session:flash_lookup(Session, "method"),

  GlobalRoutingKey = web_router:key([Method, pre_request, global]),
  GlobalModifiers = web_router_exchange:route(WebExchange, GlobalRoutingKey, [Session]),
  Session1 = web_session:modifiers(Session, GlobalModifiers),

  continue(WebExchange, Session1, pre_request_global, fun hook_pre_request_path/2).

hook_pre_request_path(WebExchange, Session) ->
  Method = web_session:flash_lookup(Session, "method"),

  PathRoutingKey = web_router:key([Method, pre_request|web_session:flash_lookup(Session, "path_tokens")]),
  PathModifiers = web_router_exchange:route(WebExchange, PathRoutingKey, [Session]),
  Session1 = web_session:modifiers(Session, PathModifiers),

  continue(WebExchange, Session1, pre_request_path, fun hook_request_global/2).

hook_request_global(WebExchange, Session) ->
  GlobalRoutingKey = web_router:key([web_session:flash_lookup(Session, "method"), request, global]),
  GlobalModifiers = web_router_exchange:route(WebExchange, GlobalRoutingKey, [Session]),
  Session1 = web_session:modifiers(Session, GlobalModifiers),

  continue(WebExchange, Session1, request_global, fun hook_request_path/2).

hook_request_path(WebExchange, Session) ->
  PathRoutingKey = web_router:key([web_session:flash_lookup(Session, "method"), request|web_session:flash_lookup(Session, "path_tokens")]),
  PathModifiers = web_router_exchange:route(WebExchange, PathRoutingKey, [Session]),
  Session1 = web_session:modifiers(Session, PathModifiers),

  continue(WebExchange, Session1, request_path, fun hook_views/2).

hook_views(WebExchange, Session) ->
  Method = web_session:flash_lookup(Session, "method"),
  ViewTokens = case web_session:flash_lookup(Session, "view_tokens") of
                 {error, 404} ->
                   web_session:flash_lookup(Session, "path_tokens");
                 Tokens ->
                   Tokens
               end,

  Body0 = web_router_exchange:route(WebExchange, web_router:key([Method, pre_request_view, global]), [Session]),
  Body1 = web_router_exchange:route(WebExchange, web_router:key([Method, pre_request_view | ViewTokens]), [Session]),

  Body2 = web_router_exchange:route(WebExchange, web_router:key([Method, request_view, global]), [Session]),
  Body3 = web_router_exchange:route(WebExchange, web_router:key([Method, request_view | ViewTokens]), [Session]),

  Body4 = web_router_exchange:route(WebExchange, web_router:key([Method, post_request_view | ViewTokens]), [Session]),
  Body5 = web_router_exchange:route(WebExchange, web_router:key([Method, post_request_view, global]), [Session]),

  RenderedBody = [Body0, Body1, Body2, Body3, Body4, Body5],
  Session1 = web_session:flash_merge_now(Session, [{"YieldedContent", RenderedBody}]),
  BodyFinal = case web_router_exchange:route(WebExchange, web_router:key([Method, request_layout_view, global]), [Session1]) of
                [] ->
                  RenderedBody;
                BodyWithLayout ->
                  BodyWithLayout
              end,
  web_session:flash_merge_now(Session1, [{"body", BodyFinal}]).

hook_post_request(WebExchange, Session) ->
  Method = web_session:flash_lookup(Session, "method"),
  GlobalRoutingKey = web_router:key([Method, post_request, global]),
  Session1 = case web_router_exchange:route(WebExchange, GlobalRoutingKey, [Session]) of
               [] ->
                 Session;
               [Response1] ->
                 Response1
             end,
  RoutingKey = web_router:key([Method, post_request|web_session:flash_lookup(Session, "path_tokens")]),
  case web_router_exchange:route(WebExchange, RoutingKey, [Session]) of
    [] ->
      Session1;
    [Response2] ->
      Response2
  end.

continue(WebExchange, Session, CurrentStep, NextStep) ->
  case web_session:continue(Session) of
    {error, Error} ->
      do_error(WebExchange, CurrentStep, Error, Session);
    {redirect, Url} ->
      web_session:flash_merge_now(Session, [{"status", 301}, {"headers", [{"Location", Url}]}, {"body", <<>>}]);
    {redirect, StatusCode, Url} ->
      web_session:flash_merge_now(Session, [{"status", StatusCode}, {"headers", [{"Location", Url}]}, {"body", <<>>}]);
    ok ->
      apply(NextStep, [WebExchange, Session])
  end.

do_error(WebExchange, Hook, Error, Session) ->
  ?ERROR_MSG("~p~nRequest: ~p~nHook: ~p~nError: ~p~n~n", [httpd_util:rfc1123_date(erlang:universaltime()),
                                                          web_session:flash_lookup(Session, "request"), Hook, Error]),
  case web_router_exchange:route(WebExchange, web_router:key([request_error, 500]), [Session, Error]) of
    [] ->
      web_session:flash_merge_now(Session, [{"status", 500}, {"headers", []}, {"body", <<"500">>}]);
    [Response] ->
      {status, Status, headers, Headers, body, Body} = Response,
      web_session:flash_merge_now(Session, [{"status", Status}, {"headers", Headers}, {"body", Body}])
  end.

do_status(WebExchange, Hook, Status, []) ->
  ?WARNING_MSG("~p~nHook: ~p~nStatus: ~p~n~n", [httpd_util:rfc1123_date(erlang:universaltime()),
                                                Hook, Status]),
  case web_router_exchange:route(WebExchange, web_router:key([request_error, Status]), [undefined]) of
    [] ->
      {status, Status, headers, [], body, list_to_binary(integer_to_list(Status))};
    [Response] ->
      Response
  end;
do_status(WebExchange, Hook, Status, Session) ->
  ?WARNING_MSG("~p~nRequest: ~p~nHook: ~p~nStatus: ~p~n~n", [httpd_util:rfc1123_date(erlang:universaltime()),
                                                             web_session:flash_lookup(Session, "request"), Hook, Status]),
  case web_router_exchange:route(WebExchange, web_router:key([request_error, Status]), [Session]) of
    [] ->
      web_session:flash_merge_now(Session, [{"status", Status}, {"headers", []}, {"body", list_to_binary(integer_to_list(Status))}]);
    [Response] ->
      {status, Status1, headers, Headers, body, Body} = Response,
      web_session:flash_merge_now(Session, [{"status", Status1}, {"headers", Headers}, {"body", Body}])
  end.
