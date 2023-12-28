# ErlPhoenix

This an example of how use Phoenix within Erlang (including code reloading, live dashboard, and live view). Mix is used as the build tool for convenience. To get started:

  * Run `mix setup` to install and setup dependencies
  * Start Phoenix endpoint with `mix phx.server`
  * Visit [`localhost:4000`](http://localhost:4000) from your browser

## Low down

The application entrypoint, router, controllers, and views are written in Erlang and placed in the `src/` directory. While Phoenix uses macros, the code generation is contained within modules, and Phoenix relies on a well-defined boundary across all layers. Examples:

  * A controller simply follows the Plug contract (which requires two functions, `init/1` and `call/2`)

  * Rendering a view simply calls a `render/2` function with the template name and the assigns

  * Channels and LiveViews define a `@behaviour` which we can adhere from any BEAM language that supports behaviours

This means it is easy to replace these layers by any other BEAM language. The only exception here is the `socket/2` macro in the endpoint, which does not currently have a direct translation to runtime APIs. However, this has recently improved with the addition of connetion upgrades to Plug and [`websock_adapter`](https://github.com/phoenixframework/websock_adapter/) and should now be feasible. For now, the endpoint is written in Elixir, but bontributions are certainly welcome to close this gap.

## Going further

Instead of simply relying on the low-level details and requiring users to implement their own router and views, this project also implements a thin binding layer, called `erlix`, which could be used if someone wants to provide high-lever bindings for using Phoenix from Erlang.

We show three different examples of integrations:

  * `erlix_router` - this translates a data-driven router, defined with Erlang records, into Elixir AST. The application router is defined at `src/erl_phoenix_router.erl` and effectively generated inside `src/erl_phoenix_app.erl`. This is, however, just one possible approach. You could mix functions, other data structures, and even your own abstractions (for example, you don't need to stick with Phoenix concept of pipelines)

  * `erlix_view` - a module that generates view modules given a template. Written in Elixir for convenience, it could be directly translated to Erlang (or other languages). Note the example app still use .heex templates, but additional templating languages can be added, as many have done in the Elixir community.

  * `erlix_live_view` - an Erlang behaviour for Erlang-written LiveViews

For self-contained modules, like the Phoenix Router, as long as a BEAM language has expressive power at compile/build time, it should be possible to hook into Elixir API and generate the relevant .beam files. For example, while we call `erlix_view` on `src/erl_phoenix_app.erl`, a Mix/Rebar3 plugin (or a parse transform) could move the call to compile-time.

## Summing up

I hope this gives more nuance to the notion that "Elixir's interop is hard" or that "Phoenix (and additional libs) are mostly macros".

  * Much of the Elixir tooling, including ExDoc, Hex, and the Elixir compiler, were written with interoperability in mind. The goal is always to contribute upstream whenever possible (instead of relying on non-BEAM languages)

  * Many Elixir projects are built on top of their runtime APIs: going from Plug and Broadway, to more recent projects such as Nx and Explorer. Livebook, for example, integrate Elixir and Erlang into a single execution

  * While macros add complexity, they can many times still be incorporated into other languages by adding a layer between the languages, similar to a FFI layer

In relation to the last point in particular, we show that:

  * While Phoenix uses macros, the huge majority of them are contained adhere to well-defined APIs, which any BEAM language can hook into, as shown in `Plug` and `erlix_live_view.erl`

  * While Elixir does have macros, Elixir macros work on a well-defined and documented Elixir AST, made by regular BEAM data structures. Any language can interact with Elixir macros by translating their data structures into Elixir AST via regular functions. This is what we have done in `src/erlix_router.erl`. You can convert the Elixir AST to .beam either at runtime or compile-time

  * A counter-example to these rules is `Ecto.Query`, which is hard to invoke granularly from other BEAM languages, given `Ecto.Query` was designed to write safe, performant, and composable SQL queries at compile-time. Moving `Ecto.Query` to runtime would negate many of its performance and security aspects

Happy holidays!
