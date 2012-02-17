
-module(eamid_start).

-behaviour(supervisor).
-include("http.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Config = {config_srv, {config_srv, start_link, []}, permanent, 5000, worker, [config_srv]},
    EAMID_SUP= {eamid_sup, {eamid_sup, start_link, []}, permanent, 5000, worker, [eamid_sup]},
    {ok, { {one_for_one, 5, 10}, [Config,EAMID_SUP]} }.


