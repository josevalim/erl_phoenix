-module(erl_phoenix_page_controller).
-export([init/1, call/2]).
-behaviour('Elixir.Plug').
-define(conn, 'Elixir.Plug.Conn').
-define(controller, 'Elixir.Phoenix.Controller').

init(Action) ->
  Action.

call(Conn, index) ->
  ?controller:render(?controller:put_view(Conn, erl_phoenix_view), index);
call(Conn, no_view) ->
  ?conn:send_resp(Conn, 200, <<"Hello world!">>).
