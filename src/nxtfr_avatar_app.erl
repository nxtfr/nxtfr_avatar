%%%-------------------------------------------------------------------
%% @doc nxtfr_avatar public API
%% @end
%%%-------------------------------------------------------------------

-module(nxtfr_avatar_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:start(nxtfr_event),
    application:start(nxtfr_autodiscovery),
    application:start(nxtfr_avatar).

start(_StartType, _StartArgs) ->
    nxtfr_avatar_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
