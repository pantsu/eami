-module(active_action).
-behaviour(gen_server).
-define(TIMEOUT,5000).
-define(EOL,"\r\n").

-export([start_link/0,start_link/4,redirect/2,callback/2,queueremove/2,queueadd/2,queuereload/0]).
-export([code_change/3,handle_cast/2,handle_info/2,terminate/2,init/1,handle_call/3]).

-record(newchannel,{privilege, channel, channelstate, channelstatedesc, calleridnum, calleridname, accountcode, application, applicationdata, exten, context, uniqueid,link=none,date,history}).

%%gen_server callback function
start_link()->       start_link("localhost",5038,"","").
start_link(Host,Port,Login,Pass)-> gen_server:start_link({local, ?MODULE}, ?MODULE, [Host,Port,Login,Pass], []).

init([Host,Port,Login,Pass])->  
	Message="Action: login\r\nUsername: "++Login++"\r\nSecret: "++Pass++"\r\nEvents: off\r\n\r\n",
	case gen_tcp:connect(Host,Port,[list, {packet, line}]) of
	{ok,Socket}->
		[{asterisk_eami,"version 1.1"}]=parser([],Socket),
		gen_tcp:send(Socket,Message),
		[{asterisk_eami,access_auth}]=parser([],Socket),
		{ok,Socket}
	;
	_->
		error_logger:error_msg({?MODULE,connect},"Could't connect into EAMI")
	end
.

handle_cast({redirect,Channel,NumberTo}, Socket) ->
	Message=lists:flatten(
	io_lib:format(
		'Action: Redirect\r\nChannel: ~s\r\nExten: ~s\r\nContext: komm\r\nPriority: 1\r\n\r\n'
		,[Channel,NumberTo])),
	gen_tcp:send(Socket,Message),
	parser([],Socket),
	{noreply, Socket}.


handle_call({queuereload}, _From, Socket) ->
	Message=lists:flatten(
	io_lib:format(
		'Action: QueueReload\r\n\r\n'
		,[Queue,Number])),
	gen_tcp:send(Socket,Message),
	parser([],Socket),
	{reply, ok, Socket};
handle_call({queueadd,Queue,Number}, _From, Socket) ->
	Message=lists:flatten(
	io_lib:format(
		'Action: QueueAdd\r\nQueue: ~s\r\nInterface: SIP/~s\r\n\r\n'
		,[Queue,Number])),
	gen_tcp:send(Socket,Message),
	parser([],Socket),
	{reply, ok, Socket};

handle_call({queueremove,Queue,Number}, _From, Socket) ->
	Message=lists:flatten(
	io_lib:format(
		'Action: QueueRemove\r\nQueue: ~s\r\nInterface: SIP/~s\r\n\r\n'
		,[Queue,Number])),
	gen_tcp:send(Socket,Message),
	parser([],Socket),
	{reply, ok, Socket};
handle_call(_,_,_)-> ok.

code_change(_,_,_)-> ok.
handle_info(_,_)->   ok.
terminate(_,_)->     ok.

%%internal function.
newtime()-> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600.
pp(P,List)-> case [X||{Y,X}<-List,Y=:=P] of [Answer]->Answer; _->error end.

%%TODO ADD ERLYDTL AS TEMPLATE
parser(Body,Socket)->
	receive 
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Success"},{message,"Authentication accepted"}]) -> 
			error_logger:info_msg({?MODULE,parser},"AMI protocol: auth access"),
			[{asterisk_eami,access_auth}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Success"},{message,"Added interface to queue"}]) -> 
			error_logger:info_msg({?MODULE,parser},"AMI protocol: Added interface to queue"),
			[{asterisk_eami,interface_add}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Success"},{message,"Removed interface from queue"}]) -> 
			error_logger:info_msg({?MODULE,parser},"AMI protocol: Removed interface from queue"),
			[{asterisk_eami,interface_del}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Success"},{message,"Queue reloaded successfully"}]) -> 
			error_logger:info_msg({?MODULE,parser},"AMI protocol: Queue reloaded successfully"),
			[{asterisk_eami,queue_reload}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Error"},{message,"Authentication failed"}]) -> 
			error_logger:error_msg({?MODULE,parser},"AMI protocol: bad auth"),
			[{asterisk_eami,bad_auth}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Error"},{message,"Permission denied"}]) -> 
			error_logger:error_msg({?MODULE,parser},"AMI protocol: permision denied"),
			[{asterisk_eami,perm_denied}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Error"},{message,"Message: Missing action in request"}])->
			error_logger:error_msg({?MODULE,parser},"AMI protocol: error action"),
			[{asterisk_eami,error_action}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Error"},{message,"Message: Unable to add interface: Already there"}])->
			error_logger:error_msg({?MODULE,parser},"AMI protocol: unknown error"),
			[{asterisk_eami,error_action}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Error"},{message,"Message: Unable to remove interface: Not there"}])->
			error_logger:error_msg({?MODULE,parser},"AMI protocol: unknown error"),
			[{asterisk_eami,error_action}];
		{tcp,Socket,Line} when Line=:= ?EOL -> Body;
		{tcp,Socket,Line} when Line=:= "Asterisk Call Manager/1.1\r\n" -> 
			error_logger:info_msg({?MODULE,parser},"AMI protocol detected"), [{asterisk_eami,"version 1.1"}];
		{tcp,Socket,Line} -> parser(  Body++[to_structure(Line--?EOL)], Socket  );
		{tcp_closed,_}->{error,tcp_closed};
		Other -> {error,Other}
		after ?TIMEOUT -> {timeout,Body}
	end
.

to_structure(List)->
	case string:tokens(List,":") of
		[X]->{list_to_atom(string:to_lower(X)),[]};
		[X,Y]->	{list_to_atom(string:to_lower(X)),Y--" "};
		Other->
			[Head|Tail]=Other,
			{list_to_atom(string:to_lower(Head)),string:join(Tail,":")--" "}
	end
.

redirect(Channel,NumberTo)->
    error_logger:info_msg({?MODULE,redirect},"Active action REDIRECT. Channel: "++Channel++" . Number_to: "++NumberTo),
	gen_server:cast(?MODULE,{redirect,Channel,NumberTo})
.

callback(From,To)->
    %%TODO: delete absolute path
	{ok,CText} =callback_dtl:render([{from,From},{to,To}]),
	Rnd="call_"++integer_to_list(random:uniform(9999999999)),
	Filename="/tmp/"++ Rnd,
	SpoolOut="/usr/local/asterisk-1.8.3.2/var/spool/asterisk/outgoing/" ++ Rnd,
	file:write_file(Filename,CText),
	file:rename(Filename,SpoolOut)
.


queueadd(all, Number)->
    error_logger:info_msg({?MODULE,queueadd},"Active action QUEUEADD. Interface: "++Number),
    [ queueadd(Queue, Number) || {Queue,_,_,Numbers} <- config_srv:get_config(queues), lists:member(Number,Numbers)]
;
queueadd(Queue, Number)->
    error_logger:info_msg({?MODULE,queueadd},"Active action QUEUEADD. Channel: "++Queue++" . Interface: "++Number),
	gen_server:call(?MODULE,{queueadd,Queue,Number})
.

queueremove(all, Number)->
    error_logger:info_msg({?MODULE,queueremove},"Active action QUEUEREMOVE. Interface: "++Number),
    [ queueremove(Queue,Number) || {Queue,_,_,Numbers} <- config_srv:get_config(queues), lists:member(Number,Numbers)]
;
queueremove(Queue, Number)->
    error_logger:info_msg({?MODULE,queueremove},"Active action QUEUEREMOVE. Channel: "++Queue++" . Interface: "++Number),
	gen_server:call(?MODULE,{queueremove,Queue,Number})
.

queuereload()->
    error_logger:info_msg({?MODULE,queuereload},"Active action QUEUERELOAD."),
	gen_server:call(?MODULE,{queuereload})
.

%%chan_spy(Channel,NumberTo)->
%%	MessageOrigin=lists:flatten(
%%	io_lib:format(
%%		'Action: Originate\r\nChannel: SIP/~s\r\nData: ~s,qg(suppq)\r\nApplication: Chanspy\r\n\r\n'
%%		,[NumberTo,Channel])),
%%	io:format('[ChanSpy]: ~s~n',[MessageOrigin]),
%%	Socket=connect("localhost",5038,off),
%%	gen_tcp:send(Socket,MessageOrigin),
%%	gen_tcp:close(Socket),
%%	ok
%%.
%%
%%activeaction(Number,[])->
%%	io:format('[Number]: ~s~n',[Number]),
%%	Socket=connect("localhost",5038,off),
%%	Ch1=origin(Number,Socket),
%%	gen_tcp:close(Socket),
%%	Ch1
%%;
%%activeaction(Number,Channel1)->
%%	io:format('[Number]: ~s~n',[Number]),
%%	io:format('[Channel1]: ~s~n',[Channel1]),
%%	Socket=connect("localhost",5038,off),
%%	Channel2=origin(Number,Socket),
%%	bridge(Channel1,Channel2,Socket),
%%	gen_tcp:close(Socket),
%%	Channel2
%%.
%%
%%%%create new channel from asterisk to operator
%%origin(Number,Socket)->
%%	ActionId=integer_to_list(random:uniform(999999999)),
%%	MessageOrigin=lists:flatten(
%%		       io_lib:format(
%%			 'Action: Originate\r\nChannel: SIP/callman61/~s\r\nExten: ~s\r\nContext: komm\r\nPriority: 1\r\nActionID: ~s\r\n\r\n',
%%		         [Number,Number,ActionId]
%%                      )),
%%	MessageStatus="Action: CoreShowChannels\r\n\r\n",
%%
%%	gen_tcp:send(Socket,MessageOrigin),
%%	OriginAnswer=parser([],Socket),	
%%%%	[{response,"Success"}, {actionid,"92300893"}, {message,"Originate successfully queued"}]
%%	case {catch pp(actionid,OriginAnswer),catch pp(message,OriginAnswer)} of
%%		{ActionId,"Originate successfully queued"}->
%%			 gen_tcp:send(Socket,MessageStatus),
%%			 Channels=coreshowchannel([],Socket),
%%			 case Channels of
%%				{timeout,_}->timeout;
%%				Chs->lists:flatten([ case {pp(channel,X),pp(extension,X)} of {Ch,Number}->Ch; _->[] end ||X<-Chs])
%%			end;
%%
%%		_-> gen_tcp:close(Socket), {error,close}
%%	end
%%.
%%
%%%%Parser for action CoreShowChannels
%%coreshowchannel(Body,Socket)->
%%	case parser([],Socket) of
%%		[{timeout,_}]->io:format('timeout~n',[]),{timeout,Body};
%%		Other->
%%			case pp(event,Other) of
%%				"CoreShowChannelsComplete"->Body;
%%				"CoreShowChannel"->coreshowchannel(Body++[Other],Socket);
%%				_->coreshowchannel(Body,Socket)
%%			end
%%	end
%%.
%%
%%%%Create new bridge 
%%bridge(Channel1,Channel2,Socket)->
%%	Message=io_lib:format('Action: Bridge\r\nChannel1: ~s\r\nChannel2: ~s\r\n\r\n',[Channel1,Channel2]),
%%	io:format('[Bridge]: ~s~n~n',[Message]),
%%	gen_tcp:send(Socket,Message)
%%.
