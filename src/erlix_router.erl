-module(erlix_router).
-include_lib("erl_phoenix/include/erlix_router.hrl").
-export([compile/1]).

compile(Module) ->
  #router{module=RouterModule} = Router = Module:router(),
  File = <<"src/", (atom_to_binary(Module))/binary, ".erl">>,
  'Elixir.Module':create(RouterModule, traverse_router(Router, Module), [{file, File}]).

traverse_router(#router{imports=Imports, routes=Routes}, Module) ->
  AllUses = [{use, [], ['Elixir.Phoenix.Router']}],
  AllImports = ['Elixir.Phoenix.LiveView.Router', Module | Imports],
  block(AllUses ++ traverse_imports(AllImports) ++ traverse_routes(Routes)).

traverse_imports(Imports) ->
  [{import, [], [Mod]} || Mod <- Imports].

traverse_routes(Routes) ->
  [traverse_route(Route) || Route <- Routes].

traverse_route(#scope{path=Path, pipe_through=Through, routes=Routes}) ->
  Inner = [{pipe_through, [], Through} | traverse_routes(Routes)],
  {scope, [], [list_to_binary(Path), [{do, block(Inner)}]]};
traverse_route(#route{method=Method, path=Path, controller=Controller, action=Action}) ->
  {Method, [], [list_to_binary(Path), Controller, Action, [{as, Controller}]]};
traverse_route(#live{path=Path, view=View, action=Action}) ->
  {live, [], [list_to_binary(Path), View, Action, [{as, View}]]};
traverse_route(#custom{function=Fun, args=Args}) ->
  {Fun, [], Args}.

block(List) when is_list(List) -> {'__block__', [], List}.
