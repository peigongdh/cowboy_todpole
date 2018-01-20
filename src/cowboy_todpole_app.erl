-module(cowboy_todpole_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
          {"/ws", ws_handler, []},
          {"/", cowboy_static, {priv_file, cowboy_todpole, "web-todpole/index.html"}},
          {"/[...]", cowboy_static, {priv_dir, cowboy_todpole, "web-todpole"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8002}],
        #{env => #{dispatch => Dispatch}}
    ),
    cowboy_todpole_sup:start_link().

stop(_State) ->
	ok.
