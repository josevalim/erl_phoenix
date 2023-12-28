-module(erl_phoenix_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(supervisor, 'Elixir.Supervisor').

start(_Type, _Args) ->
  Children = [
    {'Elixir.Phoenix.PubSub', [{name, erl_phoenix_pubsub}]},
    'Elixir.ErlPhoenixEndpoint'
  ],

  Opts = [{strategy, one_for_one}, {name, erl_phoenix_supervisor}],
  ?supervisor:start_link(Children, Opts).

stop(_State) ->
  ok.
