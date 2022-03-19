%%%-------------------------------------------------------------------
%% @doc nxtfr_avatar top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nxtfr_avatar_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 10,
                 period => 1},
    
    NxtfrAvatar = #{
        id => nxtfr_avatar,
        start => {nxtfr_avatar, start_link, []},
        type => worker},

    ChildSpecs = [NxtfrAvatar],
    {ok, {SupFlags, ChildSpecs}}.