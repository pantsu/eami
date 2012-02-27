-module(cowboy_eamid).

-export([init/3, handle/2, terminate/2,set_device/1]).
-record(newchannel,{privilege, channel, channelstate, channelstatedesc, calleridnum, calleridname, accountcode, application, applicationdata, exten, context, uniqueid,link=none,date,history}).

pp(P,List)->
        case [X||{Y,X}<-List,Y=:=P] of
                [Answer]->Answer;
                _->error
        end
.

union([])->[];
union([H|Tail])->
	case [ X || X <-Tail,X#newchannel.link=:=H#newchannel.channel ] of
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
.
freecall()->
	[ [{'Channel',X#newchannel.channel},{'CallerIDNum',X#newchannel.calleridnum},{'Uniqueid',X#newchannel.uniqueid},{'Link',"None"},{'TimeStart',X#newchannel.date}] 
		|| X <- qcalls:get(), X#newchannel.link=:=none, config_srv:is_lnumber(X#newchannel.calleridnum) =:= false]
.

call(error,_)->ok;
call(_,error)->ok;
call(From,To)-> active_action:callback(From,To).

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
				case pp("action",P) of
					"status"-> 
						Queue=union(qcalls:get())++freecall() ,
						list_to_binary(json2:encode({struct,[{status,{array,[{struct,X}||X<-Queue ]}}]}))
					;
					"call"->
						call(pp("from",P),pp("to",P)),
                                                list_to_binary("{\"action\":\"ok\"}")
					;
					"redirect"->
						%%{"action":"redirect","channel":"_CHANEL","number":"66666"}
                                                active_action:redirect(pp("channel",P),pp("number",P)),
                                                list_to_binary("{\"action\":\"ok\"}")
                                        ;
					"set_device"->
						set_device(P),
						list_to_binary("{\"action\":\"ok\"}")
					;     
					"chan_spy"->
						catch spawn(pooler,chan_spy,[pp(channel,P),pp(number,P)]),
						list_to_binary("{\"action\":\"ok\"}")
						
					;
					"update_config"->
						case pp("params",P) of
						error->	list_to_binary("{\"action\":\"error\"}");
						{struct,Val}-> 
							%%incoming_lines:
							P1=lists:flatten([X||{"incoming_lines",{array,X}}<-Val]),
							INCOMING=[{INCOMING_LINE,X}||{_,[{INCOMING_LINE,{_,X}}]}<-P1],
							config_srv:update_config([{incoming_lines,INCOMING}]),
							%%queues:
							Q1=lists:flatten([X||{"queues",{array,X}}<-Val]),
							Queues=[{Line,Strategy,Timeout,Num} ||  {_,[{Line,{_,[Strategy,Timeout,{_,Num}]}}]} <-Q1],
							config_srv:update_config([{queues,Queues}]),
							%%pools:
							
							%%sip:

							%%write_config(),
							list_to_binary("{\"action\":\"ok\"}")
						end
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


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
set_device(P)->
	Mac=string:to_upper(pp("macaddr",P)--":::::::"),
	[{Ip,Model}]=config_srv:get_ipclients(Mac),
	{ok,Cisco79xx}=
	case Model of
		"Cisco7940"-> cisco7940_dtl:render([{number,pp("number",P)}, {sipproxy, config_srv:get_config(sipproxy)}]);
		"Cisco7942"-> cisco7942_dtl:render([{number,pp("number",P)}, {sipproxy, config_srv:get_config(sipproxy)}]);
		_->{ok,nodevice}
	end,
	Filename=
	case Model of
		"Cisco7940"-> config_srv:get_config(tftproot)++"/SIP"++Mac++".cnf";
		"Cisco7942"-> config_srv:get_config(tftproot)++"/SEP"++Mac++".cnf.xml"
	end,
	io:format('[FILENAME]: ~s~n',[Filename]),
	file:write_file(Filename,lists:flatten(Cisco79xx)),

	case Model of
		"Cisco7940"-> 
			catch spawn(os,cmd,["/usr/local/unison/eamid/template/cisco7940.expect "++Ip])
		;
		"Cisco7942"-> 
			catch spawn(os,cmd,["/usr/local/unison/eamid/template/cisco7942.expect "++Ip])
		;
		_->{ok,nodevice}
	end
.
