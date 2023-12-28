-module(erl_phoenix_live).
-behaviour('Elixir.Phoenix.LiveView').
-define(lv, 'Elixir.Phoenix.LiveView').
-define(component, 'Elixir.Phoenix.Component').
-export(['__live__'/0, render/1, mount/3, handle_event/3]).

render(Assigns) ->
  erl_phoenix_view:live(Assigns).

mount(_Params, _Session, Socket) ->
  {ok, ?component:assign(Socket, counter, 0)}.

handle_event(<<"bump">>, _Params, Socket) ->
  {noreply, ?component:update(Socket, counter, fun(Val) -> Val + 1 end)}.

%% This is the only metadata required by Phoenix.LiveView.
%% A parse transform could pre-process a -live_view([])
%% attribute to generate the behaviour and this metadata
%% at compile-time.
'__live__'() -> ?lv:'__live__'().