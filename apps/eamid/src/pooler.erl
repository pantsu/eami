-module(pooler).
-behaviour(gen_server).
-define(TIMEOUT,5000).
-define(EOL,"\r\n").

-export([main/4,start_link/0,start_link/4,activeaction/2,redirect/2,chan_spy/2]).
-export([code_change/3,handle_cast/2,handle_info/2,terminate/2,init/1,handle_call/3]).

-record(newchannel,{privilege, channel, channelstate, channelstatedesc, calleridnum, calleridname, accountcode, application, applicationdata, exten, context, uniqueid,link=none,date,history}).
-record(newstate,{privilege,channel,channelstate,channelstatedesc,calleridnum,calleridname,uniqueid}).
-record(newexten,{privilege,channel,context,extension,priority,application,appdata,uniqueid}).

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
newtime()-> calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time( now()))-719528*24*3600.

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
		[{asterisk_eami,"version 1.1"}]=parser([],Socket),
		gen_tcp:send(Socket,Message),
		[{asterisk_eami,access_auth}]=parser([],Socket),
		Socket
	;
	_->
		error_logger:error_msg({?MODULE,connect},"Could't connect into EAMI"),
		exit(error_socket),
		%%И тут можно уже запустить астериск или остановить поток для рестарта его gen_server'ом
		%%io:format('error socket~n',[]),
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
		[{timeout,_}]->timeout;
		{timeout,_}->timeout;
		{error,tcp_closed}-> exit(error);
		{error,_}->pooler(Newchannel,Socket);
		Other->
			case pp(event,Other) of
				"Newstate"->
					error_logger:info_msg({?MODULE,pooler},"Channel new state:"++pp(channel,Other)),
					qcalls:update(
						#newchannel{
							channel=         pp(channel,Other),
							channelstate=    pp(channelstate,Other),
							channelstatedesc=pp(channelstatedesc,Other),
							calleridnum=     pp(calleridnum,Other),
							calleridname=    pp(calleridname,Other),
							uniqueid=        pp(uniqueid,Other)
						}
					)
				;
				"Newexten"->
					error_logger:info_msg({?MODULE,pooler},"Channel newexten:"++pp(channel,Other)),
					qcalls:update(
						#newchannel{
							channel=         pp(channel,Other),
							exten=		 pp(extension,Other),
							application =	 pp(application,Other),
							applicationdata =pp(appdata,Other),
							uniqueid=        pp(uniqueid,Other)
						}
					)
				;

				%%Как только в очереди появился новый свободный Location, на всякий случай подчищаем qcalls:...
				"QueueMemberStatus"->
%%						io:format('~nQueueMemberStatus~w',[Other]),
%%						Location=pp(location,Other)--"SIP/callman61/",
%%						io:format('~n__Location: ~s~n',[Location]),
%%						case Newchannel of
%%							[]->ok;
%%							_->
%%								AllCalls=qcalls:get(),
%%								qcalls:del(Newchannel),
%%								[qcalls:add(_Ch,[Location,"",_Uniq,[]]) || {_Ch,[_,_,_Uniq,[]],_Date} <-AllCalls,_Ch=:=Newchannel]
%%						end
				ok
				;
				"Newchannel"->
					error_logger:info_msg({?MODULE,pooler},"Create new channel:"++pp(channel,Other)),
					qcalls:add(
					#newchannel{
						channel         =pp(channel,  Other),
						channelstate    =pp(channelstate,    Other),
						channelstatedesc=pp(channelstatedesc,Other),
						calleridnum     =pp(calleridnum,     Other),
						calleridname    =pp(calleridname,    Other),
						accountcode     =pp(accountcode,Other),
						exten           =pp(exten,    Other),
						context         =pp(context,  Other),
						uniqueid        =pp(uniqueid, Other),
						link            =none,
						date            =newtime(),
						history		=[]
					})
				;  
				"Unlink"->
					error_logger:info_msg({?MODULE,pooler},"Unlink channel "++pp(channel1,Other)++" and channel "++pp(channel2,Other)),
					qcalls:update(
					#newchannel{
						channel		=pp(channel1,Other),
						link		=none
					}),
					qcalls:update(
					#newchannel{
						channel		=pp(channel2,Other),
						link		=none
					})

				;
				"Hangup"->
					error_logger:info_msg({?MODULE,pooler},"Delete(Hangup) channel:"++pp(channel,Other)),
					qcalls:del(pp(channel,Other))
				;
				"Bridge"->
					error_logger:info_msg({?MODULE,pooler},"Bridge channel "++pp(channel1,Other)++" and channel "++pp(channel2,Other)),
					qcalls:update(
					#newchannel{
						channel		=pp(channel1,Other),
						link		=pp(channel2,Other)
					}),
					qcalls:update(
					#newchannel{
						channel		=pp(channel2,Other),
						link		=pp(channel1,Other)
					})
				;
				"Rename"->
					error_logger:info_msg({?MODULE,pooler},"Rename channel. Old name "++pp(oldname,Other)++" , new name: "++pp(newname,Other)),
					Old=qcalls:get(pp(oldname,Other)),
					New=Old#newchannel{channel=pp(newname,Other)},
					qcalls:add(New),
					qcalls:del(pp(oldname,Other))
%%Event: Rename
%%Oldname: SIP/1234-6378
%%Newname: SIP/1234-6378
%%Uniqueid: 1124982513.19184
				;
				_->
					pooler(Newchannel,Socket)
			end
	end,
	pooler(Socket)
	
.

%%
%%Event: PeerStatus
%%Privilege: system,all
%%ChannelType: SIP
%%Peer: SIP/8243
%%PeerStatus: Registered
%%Address: 109.235.162.254:5060
%%
%%Event: RTCPSent
%%Privilege: reporting,all
%%To 109.235.162.250:20953
%%OurSSRC: 1236171297
%%SentNTP: 1327055340.2380857344
%%SentRTP: 1189600
%%SentPackets: 4251
%%SentOctets: 680160
%%ReportBlock:
%%FractionLost: 0
%%CumulativeLoss: 0
%%IAJitter: 0.0002
%%TheirLastSR: 0
%%DLSR: 16876.5810 (sec)




