-module(erl_phoenix_live).
-define(component, 'Elixir.Phoenix.Component').
-export(['__live__'/0, render/1, mount/3, handle_event/3]).

render(Assigns) ->
  erl_phoenix_view:live(Assigns).

mount(_Params, _Session, Socket) ->
  {ok, ?component:assign(Socket, counter, 0)}.

handle_event(<<"bump">>, _Params, Socket) ->
  {noreply, ?component:update(Socket, counter, fun(Val) -> Val + 1 end)}.

%% This is the only metadata required by Phoenix.LiveView.
%% We can find ways to encapsulate this in LiveView itself.
'__live__'() ->
  #{
    kind => view,
    lifecycle => #{mount => [], handle_params => [], after_render => [], handle_event => []},
    container => {'div', []},
    layout => false,
    log => info
  }.