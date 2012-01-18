-module(pooler).
-behaviour(gen_server).
-define(TIMEOUT,5000).
-define(EOL,"\r\n").

-export([main/4,start_link/0,start_link/4,activeaction/2,redirect/2,chan_spy/2]).
-export([code_change/3,handle_cast/2,handle_info/2,terminate/2,init/1,handle_call/3]).

-record(chanparams,{calleridnum,calleridname,uniqueid,link}).
-record(channel,{channel,param,time,history=[]}).

%%gen_server callback function
start_link()->       start_link("localhost",5038,"","").
start_link(Host,Port,Login,Pass)-> gen_server:start_link({local, ?MODULE}, ?MODULE, [Host,Port,Login,Pass], []).

init([Host,Port,Login,Pass])->  spawn_link(?MODULE,main,[Host,Port,Login,Pass]), {ok,[]}.
code_change(_,_,_)-> ok.
handle_cast(_,_)->   ok.
handle_call(_,_,_)-> ok.
handle_info(_,_)->   ok.
terminate(_,_)->     ok.

%%internal function.
main(Host,Port,Login,Pass)->
	error_logger:info_msg({?MODULE,main},"Start AMI parser"),
	Socket=connect(Host,Port,{on,Login,Pass}),
	pooler(Socket),
	error_logger:error_msg({?MODULE,main},"Shutdown AMI parser")
.
%%TODO ADD ERLYDTL AS TEMPLATE
connect(Host,Port,{on,Login,Pass})->
	Message="Action: login\r\nUsername: "++Login++"\r\nSecret: "++Pass++"\r\n\r\n",
	connect(Host,Port,Message);
connect(Host,Port,{off,Login,Pass})->
	Message="Action: login\r\nUsername: "++Login++"\r\nSecret: "++Pass++"\r\nEvents: off\r\n\r\n",
	connect(Host,Port,Message);
connect(Host,Port,Message)->
	case gen_tcp:connect(Host,Port,[list, {packet, line}]) of
	{ok,Socket}->
		io:format('!~n',[]),
		[{asterisk_eami,"version 1.1"}]=parser([],Socket),
		io:format('!!~n',[]),
		gen_tcp:send(Socket,Message),
		io:format('!!!~n',[]),
		%%[{asterisk_eami,access_auth}]=parser([],Socket),
		ZZZ=parser([],Socket),
		io:format('!!!!~w~n',[ZZZ]),
		Socket
	;
	_->
		error_logger:error_msg({?MODULE,connect},"Could't connect into EAMI"),
		exit(error_socket),
		%%И тут можно уже запустить астериск или остановить поток для рестарта его gen_server'ом
		io:format('error socket~n',[]),
		receive after ?TIMEOUT -> ok end,
		connect(Host,Port,Message)
	end
.

parser(Body,Socket)->
	receive 
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Success"},{message,"Authentication accepted"}]) -> 
			error_logger:info_msg({?MODULE,parser},"AMI protocol: auth access"),
			[{asterisk_eami,access_auth}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Error"},{message,"Authentication failed"}]) -> 
			error_logger:error_msg({?MODULE,parser},"AMI protocol: bad auth"),
			[{asterisk_eami,bad_auth}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Error"},{message,"Permission denied"}]) -> 
			error_logger:error_msg({?MODULE,parser},"AMI protocol: permision denied"),
			[{asterisk_eami,perm_denied}];
		{tcp,Socket,Line} when (Line=:= ?EOL)and(Body =:= [{response,"Error"},{message,"Message: Missing action in request"}])->
			error_logger:error_msg({?MODULE,parser},"AMI protocol: error action"),
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

pp(P,List)->
        case [X||{Y,X}<-List,Y=:=P] of
                [Answer]->Answer;
                _->error
        end
.
to_structure(List)->
	case string:tokens(List,":") of
		[X]->{list_to_atom(string:to_lower(X)),[]};
		[X,Y]->	{list_to_atom(string:to_lower(X)),Y--" "};
		Other->
			[Head|Tail]=Other,
			{list_to_atom(string:to_lower(Head)),string:join(Tail,":")--" "}
	end.

chan_spy(Channel,NumberTo)->
	MessageOrigin=lists:flatten(
	io_lib:format(
		'Action: Originate\r\nChannel: SIP/~s\r\nData: ~s,qg(suppq)\r\nApplication: Chanspy\r\n\r\n'
		,[NumberTo,Channel])),
	io:format('[ChanSpy]: ~s~n',[MessageOrigin]),
	Socket=connect("localhost",5038,off),
	gen_tcp:send(Socket,MessageOrigin),
	gen_tcp:close(Socket),
	ok
.

redirect(Channel1,NumberTo)->
	MessageOrigin=lists:flatten(
	io_lib:format(
		'Action: Redirect\r\nChannel: ~s\r\nExten: ~s\r\nContext: komm\r\nPriority: 1\r\n\r\n'
		,[Channel1,NumberTo])),
	io:format('[Redirect]: ~s~n',[MessageOrigin]),
	Socket=connect("localhost",5038,off),
	gen_tcp:send(Socket,MessageOrigin),
	gen_tcp:close(Socket),
	ok
.

activeaction(Number,[])->
	io:format('[Number]: ~s~n',[Number]),
	Socket=connect("localhost",5038,off),
	Ch1=origin(Number,Socket),
	gen_tcp:close(Socket),
	Ch1
;
activeaction(Number,Channel1)->
	io:format('[Number]: ~s~n',[Number]),
	io:format('[Channel1]: ~s~n',[Channel1]),
	Socket=connect("localhost",5038,off),
	Channel2=origin(Number,Socket),
	bridge(Channel1,Channel2,Socket),
	gen_tcp:close(Socket),
	Channel2
.

%%create new channel from asterisk to operator
origin(Number,Socket)->
	ActionId=integer_to_list(random:uniform(999999999)),
	MessageOrigin=lists:flatten(
		       io_lib:format(
			 'Action: Originate\r\nChannel: SIP/callman61/~s\r\nExten: ~s\r\nContext: komm\r\nPriority: 1\r\nActionID: ~s\r\n\r\n',
		         [Number,Number,ActionId]
                      )),
	MessageStatus="Action: CoreShowChannels\r\n\r\n",

	gen_tcp:send(Socket,MessageOrigin),
	OriginAnswer=parser([],Socket),	
%%	[{response,"Success"}, {actionid,"92300893"}, {message,"Originate successfully queued"}]
	case {catch pp(actionid,OriginAnswer),catch pp(message,OriginAnswer)} of
		{ActionId,"Originate successfully queued"}->
			 gen_tcp:send(Socket,MessageStatus),
			 Channels=coreshowchannel([],Socket),
			 case Channels of
				{timeout,_}->timeout;
				Chs->lists:flatten([ case {pp(channel,X),pp(extension,X)} of {Ch,Number}->Ch; _->[] end ||X<-Chs])
			end;

		_-> gen_tcp:close(Socket), {error,close}
	end
.

%%Parser for action CoreShowChannels
coreshowchannel(Body,Socket)->
	case parser([],Socket) of
		[{timeout,_}]->io:format('timeout~n',[]),{timeout,Body};
		Other->
			case pp(event,Other) of
				"CoreShowChannelsComplete"->Body;
				"CoreShowChannel"->coreshowchannel(Body++[Other],Socket);
				_->coreshowchannel(Body,Socket)
			end
	end
.

%%Create new bridge 
bridge(Channel1,Channel2,Socket)->
	Message=io_lib:format('Action: Bridge\r\nChannel1: ~s\r\nChannel2: ~s\r\n\r\n',[Channel1,Channel2]),
	io:format('[Bridge]: ~s~n~n',[Message]),
	gen_tcp:send(Socket,Message)
.
pooler(Socket)->pooler([],Socket).
pooler(Newchannel,Socket)->
	case parser([],Socket) of
		[{timeout,_}]->io:format('timeout~n',[]);
		{timeout,_}->io:format('timeout~n',[]);
		{error,tcp_closed}-> exit(error);
		{error,_}->pooler(Newchannel,Socket);
		Other->
			io:format('.',[]),
			case pp(event,Other) of
				"Newstate"->
					io:format('~nNewstate: ~w',[Other]),
					case pp(calleridnum,Other) of
						[]->ok;
						CalleridNum when is_list(CalleridNum)->
						%%Добавляем номер в канал из пришедшей информации.
							[{SS,DATA}]=[{_S,[CalleridNum,_B,_C,[]]}||{_S,[_,_B,_C,_],_}<-qcalls:get(),_S=:=pp(channel,Other)],
							qcalls:del(pp(channel,Other)),
							qcalls:add(SS,DATA)
						;
						_->ok
					end
				;
				%%Как только в очереди появился новый свободный Location, на всякий случай подчищаем qcalls:...
				"QueueMemberStatus"->
						io:format('~nQueueMemberStatus~w',[Other]),
						Location=pp(location,Other)--"SIP/callman61/",
						io:format('~n__Location: ~s~n',[Location]),
						case Newchannel of
							[]->ok;
							_->
								AllCalls=qcalls:get(),
								qcalls:del(Newchannel),
								[qcalls:add(_Ch,[Location,"",_Uniq,[]]) || {_Ch,[_,_,_Uniq,[]],_Date} <-AllCalls,_Ch=:=Newchannel]
						end
				;
				"Newchannel"->
						io:format('~nNewchannel: ~w',[Other]),
						qcalls:add(pp(channel,Other),[	
								pp(calleridnum,Other),
								pp(calleridname,Other),
								pp(uniqueid,Other),
								[] ]),
						pooler(pp(channel,Other),Socket)
				;  
				"Unlink"->io:format('~nUnlink: ~w',[Other]);
				"Hangup"->
					io:format('~nHangup: ~w',[Other]),
					qcalls:del(pp(channel,Other))
				;
				"Bridge"->
					%%TODO добавить рекорт для параметров. -record(chanparams,{calleridnum,calleridname,uniqueid,link})
					AllCalls=qcalls:get(),
					qcalls:del(pp(channel1,Other)),
					qcalls:del(pp(channel2,Other)),
				[qcalls:add(_Ch0,[N1,N2,N3,pp(channel2,Other)]) || {_Ch0,[N1,N2,N3,_],_} <-AllCalls,_Ch0=:=pp(channel1,Other)],
				[qcalls:add(_Ch1,[M1,M2,M3,pp(channel1,Other)]) || {_Ch1,[M1,M2,M3,_],_} <-AllCalls,_Ch1=:=pp(channel2,Other)],
					io:format('~nBridge: ~w',[Other])
				;
				_->
					pooler(Newchannel,Socket)
			end
	end,
	pooler(Socket)
	
.

