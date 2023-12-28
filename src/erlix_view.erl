-module(erlix_view).
-export([compile/3]).

compile(Module, Root, Path) ->
  AST = block([
    {import, [], ['Elixir.Phoenix.Template']},
    {embed_templates, [], [list_to_binary(Path), [{root, list_to_binary(Root)}]]}
  ]),

  File = <<"src/erlix_view.erl">>,
  'Elixir.Module':create(Module, AST, [{file, File}]).

block(List) when is_list(List) -> {'__block__', [], List}.
