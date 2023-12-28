-module(erl_phoenix_error_html).
-export([render/2]).

render(Template, _Assigns) ->
  'Elixir.Phoenix.Controller':status_message_from_template(Template).
