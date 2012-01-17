
-module(eamid_sup).

-behaviour(supervisor).

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
	case lists:keyfind(conf,1,Argx) of
		false->default();
		{conf,Path}->
			{ok,BinF} = file:read_file(Path),
			F = binary_to_list(BinF),
			FNormal = string:to_lower((F--string:copies("\t",length(F)))--string:copies(" ",length(F))),
			FTokens = string:tokens(FNormal,"\n"),
			FullConfig=[ case string:tokens(X,"=") of [P,V]-> {list_to_atom(P),V}; _->[] end  ||X <- FTokens1],
			%%осталось проверить конфиг на наличие всех переменных
			lists:member(6,FullConfig)
			
			
			
	end,

    %%если в конфиге указаны параметры логирования, то формируем строку запуска логера.

    %%формируем параметры запуска хранилища очередей.

    %%формируем параметры запуска для потока активных команд.

    %%формируем параметры для запуска вебсервера

    {ok, { {one_for_one, 5, 10}, []} }.

start_logger(Path)->
	Conf = log_mf_h:init(Path, 1024, 10).
	gen_event:add_handler(error_logger, log_mf_h, Conf).
	%%error_logger:info_msg({valx, parx},"Register PID").
.

default()->
 [{docroot,"."},
  {logdir,"/var/log/eamid"}

 ]
.
