-module(sacudiendo_la_cabeza_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    sacudiendo_la_cabeza_sup:start_link().

stop(_State) ->
    ok.
