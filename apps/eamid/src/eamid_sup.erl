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

    %% starting application

    %% start logger
	case {config_srv:get_config(logger),config_srv:get_config(logdir)} of
        {"on",Logdir}->
            case file:read_file_info(Logdir) of
                {ok,{file_info,_,directory,read_write,_,_,_,_,_,_,_,_,_,_}}-> 
                           io:format('Starting logger. LogDir=~s~n',[Logdir]),
                           Conf = log_mf_h:init(Logdir, 1024, 10),
                           gen_event:add_handler(error_logger, log_mf_h, Conf),
                           error_logger:info_msg({?MODULE, start_logger},"Start file logger");
                {error,_}->none;
                _->none
            end;
		_->none
	end,

    %% start tftpd:
	case {config_srv:get_config(tftpd),config_srv:get_config(tftproot)} of
    {"on",DR}->
            tftp:start([
			 {callback, {".cnf", tftp_eamid, [{root_dir, DR}]}},
			 {callback, {".cnf.xml", tftp_eamid, [{root_dir, DR}]}},
			 {callback, {".*[^cnf]", tftp_file, [{root_dir, DR}]}}
            ]);
    _->none
    end,

    %% start cowboy:
	case {config_srv:get_config(webserver),config_srv:get_config(wwwport)} of
    {"on", WWWPort}->
        application:start(cowboy),
        Dispatch = [{'_', [{'_', cowboy_eamid, []}]}],
        cowboy:start_listener(http, 100,cowboy_tcp_transport, [{port, list_to_integer(WWWPort)}],cowboy_http_protocol, [{dispatch, Dispatch}]);
    _->none
    end,

    %% change UID:
	case {config_srv:get_config(chroot),config_srv:get_config(uid)} of
    {"ok", UID}->
        setuid:start_link(),
        setuid:setuid(list_to_integer(UID)),
        error_logger:info_msg({?MODULE, init},"Change UID");
    _->none
    end,


    %%gen_server parameters: 
    GenSrv=lists:flatten(
    [
     {qcalls, {qcalls, start_link, []}, permanent, 5000, worker, [qcalls]},

     case config_srv:get_config(ami) of 
     "on"->
        H = config_srv:get_config(eamihost),
        Port = list_to_integer( config_srv:get_config(eamiport) ),
        L = config_srv:get_config(eamilogin),
        Pass = config_srv:get_config(eamipassword),
        [
         {pooler, {pooler, start_link, [H,Port,L,Pass]}, permanent, 5000, worker, [pooler]},
         {active_action, {active_action, start_link, [H,Port,L,Pass]}, permanent, 5000, worker, [active_action]}
        ];
     _-> []
     end
    ]),

    {ok, { {one_for_one, 5, 10}, GenSrv} }.

