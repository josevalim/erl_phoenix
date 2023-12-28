-module(erlix_view).
-export([compile/3]).

compile(Module, Root, Path) ->
  %% Imports could be made an option if desired!
  AST = block([
    {use, [], ['Elixir.Phoenix.Component']},
    {embed_templates, [], [list_to_binary(Path), [{root, list_to_binary(Root)}]]}
  ]),

  File = <<"src/erlix_view.erl">>,
  'Elixir.Module':create(Module, AST, [{file, File}]).

block(List) when is_list(List) -> {'__block__', [], List}.
