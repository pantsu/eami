-module(config_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(pool,{pools=[]}).
-record(queues,{all=[]}).
-record(number,{number,password,fio}).
-record(queue,{name,number,agents=[]}). 

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_config/0, get_config/1, read_config/1, update_config/1, create_asterisk_config/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
  {ok,read_config(file)}.

handle_call(all, _From, Config) ->
  {reply, Config, Config};
handle_call({get,Val}, _From, Config) ->
  {reply,
   case lists:keyfind(Val,1,Config) of
        false-> false;
        {Val,R}-> R;
        Other-> Other
   end,
  Config};
handle_call({update_config,Val},_From,Config)->
   io:format('[UPDATE]: ~w~n~w~n---~n',[Config,Val]),
   NewConfig= validate_conf([ X ||{X,_} <- Config],Val),
   io:format('[UPDATE]: ~w~n',[NewConfig]),
   {reply,ok, NewConfig}
.

handle_cast(_Msg, Config) ->
  {noreply, Config}.

handle_info(_Info, Config) ->
  {noreply, Config}.

terminate(_Reason, _Config) ->
  ok.

code_change(_OldVsn, Config, _Extra) ->
  {ok, Config}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

create_asterisk_config()->
   {ok,SIP}=sip_dtl:render([{numbers,get_config(numbers)}]),
   SIP
.

read_config(file)->
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
  Config
;
read_config(http)->ok;
read_config(json)->ok.

get_config()->
   gen_server:call(?MODULE,all).
get_config(Val)->
   gen_server:call(?MODULE,{get,Val}).

%% Val={"http",_Link} | {params, Val}
update_config(Val)->
   gen_server:call(?MODULE,{update_config,Val})
.

default()->
 [
  {docroot,"."},
  {tftproot,"/usr/local/unison/eamid/tftp"},
  {asteriskconfig,"/usr/local/unison/etc/asterisk"},
  {logdir,"/var/log/eamid"},
  {eamihost,"localhost"},
  {eamiport,"5038"},
  {eamilogin,""},
  {eamipassword,""},
  {wwwport,"8080"},
  {uid,"33"},
  %%parameter for start application:
  {tftpd,"off"},
  {webserver,"off"},
  {chroot,"off"},
  {logger,"off"},
  {ami,"off"},

  %%asterisk parameters:
  %%    {"action":"update_config","params":[
  %%    	{"incoming_lines",[
  %%    		{1192,["voice1","voice2","queue1","queue2","queue3","voice3"]},
  %%    		{1193,[...]}]},
  %%    	{"queues",[
  %%    		{"queue1",[001,002,...]},
  %%    		{...}
  %%    	]},
  %%            {numbers,[{001,"passwd1","user 001"}, {002,"passwd2","user 002"}]},
  %%    	{"pools":[{"192.168.0.111",5060,"login","password"},{"192.168.0.112",5075,"",""},...]}
  %%    ]}

  {queues,[{"queue1",[001,002,003]},{"queue2",[004,005,006]}]},
  {numbers,[{001,"passwd1","user 001"}, {002,"passwd2","user 002"}]},
  {"incoming_lines",[]},
  {pools,[]} 
 ]
.

validate_conf([],List)->List;
validate_conf([H|Param],List)->
  case lists:member(H, [ X || {X,_} <- List]) of
	false ->validate_conf(Param,List++ [lists:keyfind(H,1,default())] );
  	_->validate_conf(Param,List)
  end
.

