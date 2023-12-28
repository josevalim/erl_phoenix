-module(erl_phoenix_router).
-export([router/0, browser/2]).
-define(plug, 'Elixir.Plug').
-define(conn, 'Elixir.Plug.Conn').
-define(controller, 'Elixir.Phoenix.Controller').
-define(live_view_router, 'Elixir.Phoenix.LiveView.Router').
-include_lib("erl_phoenix/include/erlix_router.hrl").

%% live_dashboard "/dashboard"

router() ->
  #router{
    module = 'Elixir.ErlPhoenixRouter',
    imports = ['Elixir.Phoenix.LiveDashboard.Router'],
    routes = [
      #scope{path = "/", pipe_through = [browser], routes = [
        #route{method = get, path = "/", controller = erl_phoenix_page_controller, action = index},
        #custom{function=live_dashboard, args=[<<"/dashboard">>]}
      ]}
    ]
  }.

browser(Conn, _Opts) ->
  ?plug:run(Conn, [
    fun(Conn) -> ?controller:accepts(Conn, [<<"html">>]) end,
    fun ?conn:fetch_session/1,
    fun(Conn) -> ?live_view_router:fetch_live_flash(Conn, []) end,
    % fun(Conn) -> ?controller:put_root_layout(Conn, [{html, {erl_phoenix_layout_html, root}}]) end,
    fun ?controller:protect_from_forgery/1,
    fun ?controller:put_secure_browser_headers/1
  ]).
