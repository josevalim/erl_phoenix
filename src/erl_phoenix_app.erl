-module(erl_phoenix_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(supervisor, 'Elixir.Supervisor').

start(_Type, _Args) ->
  erlix_router:compile(erl_phoenix_router),
  erlix_view:compile(erl_phoenix_view, code:lib_dir(erl_phoenix), "priv/templates/*.html"),

  Children = [
    {'Elixir.Phoenix.PubSub', [{name, erl_phoenix_pubsub}]},
    'Elixir.ErlPhoenixEndpoint'
  ],

  Opts = [{strategy, one_for_one}, {name, erl_phoenix_supervisor}],
  ?supervisor:start_link(Children, Opts).

stop(_State) ->
  ok.
