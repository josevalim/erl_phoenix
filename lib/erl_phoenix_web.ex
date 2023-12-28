# defmodule ErlPhoenixWeb do
#   @moduledoc """
#   The entrypoint for defining your web interface, such
#   as controllers, components, channels, and so on.

#   This can be used in your application as:

#       use ErlPhoenixWeb, :controller
#       use ErlPhoenixWeb, :html

#   The definitions below will be executed for every controller,
#   component, etc, so keep them short and clean, focused
#   on imports, uses and aliases.

#   Do NOT define functions inside the quoted expressions
#   below. Instead, define additional modules and import
#   those modules here.
#   """

#   def static_paths, do: ~w(assets fonts images favicon.ico robots.txt)

#   def router do
#     quote do
#       use Phoenix.Router, helpers: false

#       # Import common connection and controller functions to use in pipelines
#       import Plug.Conn
#       import Phoenix.Controller
#       import Phoenix.LiveView.Router
#     end
#   end

#   def channel do
#     quote do
#       use Phoenix.Channel
#     end
#   end

#   def controller do
#     quote do
#       use Phoenix.Controller,
#         formats: [:html, :json],
#         layouts: [html: ErlPhoenixWeb.Layouts]

#       import Plug.Conn
#       import ErlPhoenixWeb.Gettext

#       unquote(verified_routes())
#     end
#   end

#   def live_view do
#     quote do
#       use Phoenix.LiveView,
#         layout: {ErlPhoenixWeb.Layouts, :app}

#       unquote(html_helpers())
#     end
#   end

#   def live_component do
#     quote do
#       use Phoenix.LiveComponent

#       unquote(html_helpers())
#     end
#   end

#   def html do
#     quote do
#       use Phoenix.Component

#       # Import convenience functions from controllers
#       import Phoenix.Controller,
#         only: [get_csrf_token: 0, view_module: 1, view_template: 1]

#       # Include general helpers for rendering HTML
#       unquote(html_helpers())
#     end
#   end

#   defp html_helpers do
#     quote do
#       # HTML escaping functionality
#       import Phoenix.HTML
#       # Core UI components and translation
#       import ErlPhoenixWeb.CoreComponents
#       import ErlPhoenixWeb.Gettext

#       # Shortcut for generating JS commands
#       alias Phoenix.LiveView.JS

#       # Routes generation with the ~p sigil
#       unquote(verified_routes())
#     end
#   end

#   def verified_routes do
#     quote do
#       use Phoenix.VerifiedRoutes,
#         endpoint: ErlPhoenixEndpoint,
#         router: ErlPhoenixWeb.Router,
#         statics: ErlPhoenixWeb.static_paths()
#     end
#   end

#   @doc """
#   When used, dispatch to the appropriate controller/view/etc.
#   """
#   defmacro __using__(which) when is_atom(which) do
#     apply(__MODULE__, which, [])
#   end
# end

# defmodule ErlPhoenixWeb.Router do
#   use ErlPhoenixWeb, :router

#   pipeline :browser do
#     plug :accepts, ["html"]
#     plug :fetch_session
#     plug :fetch_live_flash
#     plug :put_root_layout, html: {ErlPhoenixWeb.Layouts, :root}
#     plug :protect_from_forgery
#     plug :put_secure_browser_headers
#   end

#   pipeline :api do
#     plug :accepts, ["json"]
#   end

#   scope "/", ErlPhoenixWeb do
#     pipe_through :browser

#     get "/", PageController, :home
#   end

#   # Other scopes may use custom stacks.
#   # scope "/api", ErlPhoenixWeb do
#   #   pipe_through :api
#   # end

#   # Enable LiveDashboard and Swoosh mailbox preview in development
#   if Application.compile_env(:erl_phoenix, :dev_routes) do
#     # If you want to use the LiveDashboard in production, you should put
#     # it behind authentication and allow only admins to access it.
#     # If your application does not have an admins-only section yet,
#     # you can use Plug.BasicAuth to set up some basic authentication
#     # as long as you are also using SSL (which you should anyway).
#     import Phoenix.LiveDashboard.Router

#     scope "/dev" do
#       pipe_through :browser

#       live_dashboard "/dashboard", metrics: ErlPhoenixWeb.Telemetry
#       forward "/mailbox", Plug.Swoosh.MailboxPreview
#     end
#   end
# end
