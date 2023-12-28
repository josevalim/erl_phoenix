-module(erl_phoenix_page_controller).
-export([init/1, call/2]).
-behaviour('Elixir.Plug').
-define(conn, 'Elixir.Plug.Conn').

init(Action) ->
  Action.

call(Conn, index) ->
  ?conn:send_resp(Conn, 200, <<"Hello world!">>).
