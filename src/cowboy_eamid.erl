-module(cowboy_eamid).

-export([init/3, handle/2, terminate/2]).
-record(newchannel,{privilege, channel, channelstate, channelstatedesc, calleridnum, calleridname, accountcode, application, applicationdata, exten, context, uniqueid,link=none,date,history}).

pp(P,List)->
        case [X||{Y,X}<-List,Y=:=P] of
                [Answer]->Answer;
                _->error
        end
.

union([])->[];
union([H|Tail])->
	case [ X || X <-Tail,is_record(newchannel,X),X#newchannel.link=:=H#newchannel.channel ] of
		[X]->
			[[
			  {'Channel',H#newchannel.channel},
			  {'CallerIDNum',H#newchannel.calleridnum},
			  {'CallerIDNum2',X#newchannel.calleridnum},
			  {'Uniqueid',H#newchannel.uniqueid},
			  {'TimeStart',H#newchannel.date}
			]]  ++union([Y || Y<-Tail, Y#newchannel.channel/=X#newchannel.channel ])
		;
		_-> union(Tail)
	end
%%	{Ch1,[Num1,Name1,Uniq1,Link1],Date1}=Head,
%%	%%Ищем пару к звонку	
%%%%	[{Channel2,Nummer2}]=[{Ch2,Num2}||{Ch2,[Num2,Name2,Uniq2,Link2],Date2}<-Tail, Ch2=:=Link1],
%%	
%%	case [{Ch2,Num2}||{Ch2,[Num2,Name2,Uniq2,Link2],Date2}<-Tail, Ch2=:=Link1] of
%%		[{Channel2,Nummer2}]->
%%			%%Удаляем из хвоста списка найденную пару
%%			TailWth=[catch {Ch3,[Num3,Name3,Uniq3,Link3],Date3}||{Ch3,[Num3,Name3,Uniq3,Link3],Date3}<-Tail, Ch3/=Link1],
%%			[[{'Channel',Ch1},{'CallerIDNum',Num1},{'CallerIDNum2',Nummer2},{'Uniqueid',Uniq1},{'TimeStart',Date1}]]++union(TailWth)
%%		;
%%		%% А вообще тут надо поправить unlink.
%%		_->union(Tail)
%%	end
.
freecall()->
	[ [{'Channel',X#newchannel.channel},{'CallerIDNum',X#newchannel.calleridnum},{'Uniqueid',X#newchannel.uniqueid},{'Link',"None"},{'TimeStart',X#newchannel.date}] 
		|| X <- qcalls:get(), X#newchannel.link=:=none]
.


init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Reply=
    case cowboy_http_req:method(Req) of
	{'GET',_}-> <<"Unknown method">>;
	{'POST',_}->
		Body=
		case cowboy_http_req:body(Req) of
			{error,badarg}->error_body;
			{ok,B,_}-> binary_to_list(B);
			_->unknown_body
		end,
		case (catch json2:decode_string(lists:flatten(Body))) of
			{ok,{struct,P}}->
				io:format('~n~w~n',[P]),
				case pp("action",P) of
					"status"-> 
						%%Queue=freecall() ,
						Queue=union()++freecall([]) ,
						io:format('[DEBUG]: [status]: ~w~n',[Queue]),
						list_to_binary(json2:encode({struct,[{status,{array,[{struct,X}||X<-Queue ]}}]}))
					
					;
					"call"->
						case pp(channelid,P) of
							[]->catch spawn(pooler,activeaction,[integer_to_list(pp(number,P)),[]]);
							_->catch spawn(pooler,activeaction,[integer_to_list(pp(number,P)),pp(channelid,P)])
						end,
						{html,"{\"action\":\"ok\"}"}
					;
					"redirect"->
						%%{"action":"redirect","channel":"_CHANEL","number":"66666"}
						catch spawn(pooler,redirect,[pp(channel,P),pp(number,P)]),
						{html,"{\"action\":\"ok\"}"}
					;
					"chan_spy"->
						catch spawn(pooler,chan_spy,[pp(channel,P),pp(number,P)]),
						{html,"{\"action\":\"ok\"}"}
						
					;
					_-> <<"action unknown">>
				end
			;
			_-> <<"error_json">>
		end
	;
	_->unknown_method, {ok,Req}
    end,
    {ok, Req2} = cowboy_http_req:reply(200, [], Reply, Req),
    {ok, Req2, State}.

terminate(Req, State) ->
    ok.

