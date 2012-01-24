
-module(eamid_sup).

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
	Argx=init:get_arguments(),
	io:format('Confid reading...',[]),
	Config=case lists:keyfind(conf,1,Argx) of
		false->default();
		{conf,Path}->
			{ok,BinF} = file:read_file(Path),
			F = binary_to_list(BinF),
			FNormal = (F--string:copies("\t",length(F)))--string:copies(" ",length(F)),
			FTokens = string:tokens(FNormal,"\n"),
			FullConfig=[ case string:tokens(X,"=") of [P,V]-> {list_to_atom(string:to_lower(P)),V}; _->[] end  ||X <- FTokens],
			validate_conf([ X ||{X,_} <- default()],FullConfig)
	       end,
	io:format(' Done.~n--->~n~w~n<---~n',[Config]),

    %%если в конфиге указаны параметры логирования, то формируем строку запуска логера.
	case lists:keyfind(logdir,1,Config) of
		{logdir,Logdir}-> 
			io:format('Starting logger. LogDir=~s~n',[Logdir]),
			start_logger(Logdir);
		_->none
	end,

    %%формируем параметры запуска хранилища очередей.
    %%формируем параметры запуска для потока активных команд.
    %%формируем параметры для запуска списка активных звонков.
    Qcalls = {qcalls, {qcalls, start_link, []}, permanent, 5000, worker, [qcalls]},
    %%формируем параметры для запуска парсера сообщений от астериска.
    {eamihost,H}=lists:keyfind(eamihost,1,Config),
    {eamiport,P}=lists:keyfind(eamiport,1,Config),Port=list_to_integer(P),
    {eamilogin,L}=lists:keyfind(eamilogin,1,Config),
    {eamipassword,Pass}=lists:keyfind(eamipassword,1,Config),
    Pooler = {pooler, {pooler, start_link, [H,Port,L,Pass]}, permanent, 5000, worker, [pooler]},
    ActiveAction = {active_action, {active_action, start_link, [H,Port,L,Pass]}, permanent, 5000, worker, [active_action]},

%%start cowboy:
	application:start(cowboy),
        Dispatch = [{'_', [{'_', cowboy_eamid, []}]}],
	cowboy:start_listener(http, 100,cowboy_tcp_transport, [{port, 8080}],cowboy_http_protocol, [{dispatch, Dispatch}]),
%%

    {ok, { {one_for_one, 5, 10}, [Qcalls,Pooler,ActiveAction]} }.

validate_conf([],List)->List;
validate_conf([H|Param],List)->
  case lists:member(H, [ X || {X,_} <- List]) of
	false ->validate_conf(Param,List++ [lists:keyfind(H,1,default())] );
  	_->validate_conf(Param,List)
  end
.

start_logger(Path)->
  Conf = log_mf_h:init(Path, 1024, 10),
  gen_event:add_handler(error_logger, log_mf_h, Conf),
  error_logger:info_msg({?MODULE, start_logger},"Start file logger").

default()->
 [
  {docroot,"."},
  {logdir,"/var/log/eamid"},
  {eamihost,"localhost"},
  {eamiport,"5038"},
  {eamilogin,""},
  {eamipassword,""}
 ]
.


