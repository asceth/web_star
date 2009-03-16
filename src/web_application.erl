-module(web_application).

-export([prefix/2, config/2]).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [{setup, 1},
   {routes, 1},
   {maintenance, 1},
   {teardown, 1}];

behaviour_info(_Other) ->
  undefined.

prefix(WebApplication, Config) ->
  case proplists:get_value(WebApplication, Config) of
    undefined ->
      "";
    Options ->
      case proplists:get_value(prefix, Options) of
        undefined ->
          "";
        Prefix ->
          Prefix
      end
  end.

config(WebApplication, Config) ->
  proplists:get_value(WebApplication, Config, []).



