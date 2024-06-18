# `erl_phoenix`

This an example of how use Phoenix within Erlang (including code reloading, live dashboard, and live view). Mix is used as the build tool for convenience. To get started:

  * Run `mix setup` to install and setup dependencies
  * Start Phoenix endpoint with `mix phx.server`
  * Visit [`localhost:4000`](http://localhost:4000) from your browser

## Low down

The application entrypoint, router, controllers, and views are written in Erlang and placed in the `src/` directory. While Phoenix uses macros, the code generation is contained within modules, and Phoenix relies on a well-defined boundary across all layers. Examples:

  * A controller simply follows the Plug contract (which requires two functions, `init/1` and `call/2`) (see `src/erl_phoenix_page_controller.erl`)

  * Rendering a view simply calls a `render/2` function with the template name and the assigns (see `src/erl_phoenix_error_html.erl`)

  * LiveViews define a `@behaviour` which we can adhere from any BEAM language that supports behaviours (see `src/erl_phoenix_live.erl`)

This means it is easy to replace these layers by any other BEAM language. The only exception here is the `socket/2` macro in the endpoint, which does not currently have a direct translation to runtime APIs. However, this has recently improved with the addition of connetion upgrades to Plug and [`websock_adapter`](https://github.com/phoenixframework/websock_adapter/). For now, the endpoint is written in Elixir for this reason, but contributions to close this gap within Phoenix are welcome.

## Going further

If you want to move beyond the contracts between the layers, we can also build abstractions on top of Phoenix, even from Erlang! This project implements a thin binding layer, called `erlix`, which could be used if someone wants to provide high-lever bindings for using Phoenix from Erlang. All leveraging public Phoenix APIs.

We show two different examples of integrations:

  * `erlix_router` - this translates a data-driven router, defined with Erlang records, into Elixir AST. You can find an example at `src/erl_phoenix_router.erl`. This is, however, just one possible approach. You could mix functions, other data structures, and even your own abstractions

  * `erlix_view` - a module that generates view modules given a template. Note the example app still use .heex templates, but additional templating languages can be added, as many have done in the Elixir community

In both cases, because Elixir macros work on Elixir AST, and Elixir AST is nothing more the well-defined data structures, any BEAM language can manipulate them! Here are some possible ways forward:

  * We currently call `erlix_view`/`erlix_router` on `src/erl_phoenix_app.erl`, but a Mix/Rebar3 plugin (or a parse transform) could move the call to compile-time if desired

  * LiveViews require both `render/1` and `__live__/0` functions. A macro or a parse transform could make this cleaner if desired. For example, a parse transform could read `-live_view([{view, my_app_view}, {container, ...}])` and set both functions up. The important bit is that the LiveView contract is well defined for anyone who wishes to improve it

Hopefully this repository moves the conversation forward with practical examples and places where the integration between Elixir and other BEAM languages can improve.

## Summing up

I hope this gives more nuance to the notion that "Elixir's interop is poor" or "Phoenix (and additional libs) are mostly macros". In particular:

  * Elixir, by design, shares most of its compiler and process abstractions with Erlang/OTP, instead of rolling its own. In many situations, the best way to speed up the compiler or to add a feature to Elixir's GenServer is by contributing it upstream
  
  * Much of the Elixir tooling, including ExDoc and Hex, were written with interoperability in mind. Packages from any BEAM language can be published to Hex. Erlang (and potentially other BEAM languages) can use ExDoc to generate documentation. The Erlang terminal can show Elixir docs (and vice-versa). Etc

  * Many Elixir projects are built on top of their runtime APIs: going from Plug and Broadway, to more recent projects such as Nx and Explorer. Livebook, for example, integrate Elixir and Erlang into a single execution

  * While macros add a layer on top of interoperability, they can many times still be incorporated into other languages with some additional transformation (similar to FFIs)

In relation to the last point in particular, we show that:

  * While Phoenix uses macros, the huge majority of them are contained to modules and adhere to well-defined APIs, which any BEAM language can hook into, as shown in plugs, controllers, views, and live views

  * Macros are nothing more than data manipulation at compile-time, which can be leveraged by other BEAM languages and tools. This is what we have done in `src/erlix_router.erl` and `src/elixir_view.erl`. At the end of the day, you can convert data structures into Elixir AST and then into `.beam` by using Elixir APIs, either at runtime or compile-time. It is yet unclear how useful this can be in practice though

  * A counter-example is `Ecto.Query`, which is hard to invoke granularly from other BEAM languages, given `Ecto.Query` was designed to write safe, performant, and composable SQL queries at compile-time. Moving `Ecto.Query` to runtime would negate many of its performance and security aspects

Happy holidays! 🎄
