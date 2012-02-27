-module(config_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%-record(pool,{pools=[]}).
%%-record(queues,{all=[]}).
%%-record(number,{number,password,fio}).
%%-record(queue,{name,number,agents=[]}). 

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_config/0, get_config/1, read_config/1, update_config/1, update_config_reload/1, create_asterisk_config/0, is_lnumber/1, get_nqueues/1, write_config/0, get_ipclients/1]).

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
  {reply, pp(Val,Config), Config}
;
handle_call({update_config,Val},_From,Config)->
   NewConfig= validate_conf([ X ||{X,_} <- Config],Val,Config),
   {reply,ok, NewConfig}
;
handle_call({update_config_reload,Val},_From,Config)->
   NewConfig= validate_conf([ X ||{X,_} <- Config],Val,Config),
   OldQueue=pp(queues,Config),
   NewQueue=pp(queues,NewConfig),
   case OldQueue=:=NewQueue of
    true->true;
    _->
     %TODO: add new lines in queue.conf


     %%callback
       [ [ active_action:queueremove(Queue,Number) || Number <- Numbers] || {Queue,_,_,Numbers} <- OldQueue],
       [ [ active_action:queueadd(Queue,Number) || Number <- Numbers] || {Queue,_,_,Numbers} <- NewQueue]
   end,
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
write_config()->
	{ok, SIP} =sip_dtl:render([{pools,config_srv:get_config(pools)},{numbers,config_srv:get_config(numbers)}]),
	{ok,QUEUE}=queue_dtl:render([{queues,config_srv:get_config(queues)}]),
	{ok,EXTEN}=extensions_dtl:render([
				{numbers,config_srv:get_config(numbers)},
				{lines,[ Name || {Name,_,_,_} <-config_srv:get_config(queues)]}
				   ]),
	file:write_file(config_srv:get_config(asteriskconfig)++"/sip.conf",SIP),
	file:write_file(config_srv:get_config(asteriskconfig)++"/queue.conf",QUEUE),
	file:write_file(config_srv:get_config(asteriskconfig)++"/extensions.conf",EXTEN),
	%%TODO: add asterisk reload
	ok
.

pp(Val,Config)->
   case lists:keyfind(Val,1,Config) of
        false-> false;
        {Val,R}-> R;
        Other-> Other
   end
.

get_ipclients(Mac)->
    [ {IP,Model}  || {IP, M, Model} <- get_config(ip_clients), M=:=Mac  ]
.

get_nqueues(Num)->
	%%{"queue1","all",11,[001,002,003]}
	[ case lists:members(Num, Numbers) of 
            false-> [] ;
            true-> Queue
          end ||{Queue,_,_,Numbers}<- get_config(queues)]
.

is_lnumber(Num) when is_number(Num)->
	case [ ok || {Number, _Password, _Fio} <-  get_config(numbers), Number=:=Num] of
	[]-> false;
	_-> true
	end
;
is_lnumber(Num) when is_list(Num)->
	is_lnumber(list_to_integer(Num))
.

create_asterisk_config()->
   {ok,SIP}=sip_dtl:render([{numbers,get_config(numbers)}]),
   SIP
.

ip_clients(IPClient)->
    case string:tokens(IPClient, "/") of
        [IP,Mac,Model]-> {IP,string:to_upper(Mac--":::::::"),Model};
        _->[]
    end
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
			AllConfig=[ case string:tokens(X,"=") of [P,V]-> {list_to_atom(string:to_lower(P)),V}; _->[] end  ||X <- FTokens],

            IPClient= [{ip_clients, lists:flatten([ ip_clients(Y)  || {X,Y}<-AllConfig, X=:= ip_clients ])}], 
            NOIPClient=[ {X,Y} || {X,Y}<-AllConfig, X /= ip_clients ],
            FullConfig= NOIPClient++IPClient,
			validate_conf([ X ||{X,_} <- default()],FullConfig,default())
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

update_config_reload(Val)->
   gen_server:call(?MODULE,{update_config_reload,Val})
.

default()->
 [
  {docroot,"."},
  {sipproxy,"127.0.0.1"},
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
  %%    	{"pools":[{"pool1","192.168.0.111",5060,"login","password"},{"pool2","192.168.0.112",5075,"",""},...]}
  %%    ]}

  {queues,[{"line1","all",11,[]}]},
  {numbers,[{001,"passwd1","user 001"}, {002,"passwd2","user 002"}]},
  {incoming_lines,[]},
  {pools,[]},
  {ip_clients,[]} 
  %% {ip_clients,[{"192.168.0.2","04C5A4B1C802","Cisco 7940"}]} 
 ]
.

validate_conf([],List,_)->List;
validate_conf([H|Param],List,Template)->
  case lists:member(H, [ X || {X,_} <- List]) of
	false ->validate_conf(Param,List++ [lists:keyfind(H,1,Template)],Template );
  	_->validate_conf(Param,List,Template)
  end
.

