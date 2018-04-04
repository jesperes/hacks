%%%-------------------------------------------------------------------
%% @doc erlbrot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbrot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    error_logger:info_msg("erlbrot supervisor init~n"),
    
    EbWx = {eb_wx, {eb_wx, start_link, []},
	    temporary, brutal_kill, worker, [eb_wx]},
    
    Children = [EbWx],
    RestartStrategy = {one_for_all, 0, 1},

    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
