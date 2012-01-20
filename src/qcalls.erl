-module(qcalls).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3]).
-export([add/1,del/1,get/0,get/1,update/1]).
-export([code_change/3,handle_cast/2,handle_info/2,terminate/2]).

-record(newchannel,{privilege, channel, channelstate, channelstatedesc, calleridnum, calleridname, accountcode, application, applicationdata, exten, context, uniqueid,link=none,date,history}).
-record(newstate,{privilege,channel,channelstate,channelstatedesc,calleridnum,calleridname,uniqueid}).
-record(newexten,{privilege,channel,context,extension,priority,application,appdata,uniqueid}).


%%internal function.
%%undefined(Value,NewValue,A)
vvv(undefined,undefined)->undefined;
vvv(Val,undefined)->Val;
vvv(_,New)->New.


%%input #newchannel on add(Channel)
add(Channel)-> gen_server:call(qcalls,{add,Channel}).
%%input value #newchannel.channel on del(Channel)
del(Channel)-> gen_server:call(qcalls,{del,Channel}).
%%input value #newchannel.channel on get(Channel)
get(Channel)-> gen_server:call(qcalls,{get,Channel}).
get()->       gen_server:call(qcalls,{get}).
%%TODO: доделать update
update(Channel)-> gen_server:call(qcalls,{update,Channel}) .

%%gen_server callback function
init(_Args)->        {ok,[]} .
start_link() ->      gen_server:start_link({local, qcalls}, qcalls, [], []).
code_change(_,_,_)-> ok.
handle_cast(_,_)->   ok.
handle_info(_,_)->   ok.
terminate(_,_)->     ok.

handle_call({add,Channel}, _From, Session)-> {reply, ok, Session++[Channel]};

handle_call({del,Channel}, _From, Session)->
	   	     [error_logger:error_msg({?MODULE,handle_call_del},
			"Can't delete channel: "++X#newchannel.channel) || 
			X <-Session,is_record(X,newchannel),X#newchannel.channel=:=Channel],
		     NewSession=[X || X <- Session, is_record(X,newchannel),X#newchannel.channel /= Channel ],	
		     {reply, ok, NewSession};

handle_call({get},_From, Session)->
	             {reply,Session,Session};

handle_call({get,Channel},_From, Session)->
		     Answer=
		     case [X || X <-Session,is_record(X,newchannel),X#newchannel.channel=:=Channel] of
			[O]->O;
			[]->[]
		     end,
	             {reply,Answer,Session};

handle_call({update,Channel},_From, Session)->
%%TODO Исправить. Понять что передаем при обновлении.
		     NewSession=[ case X of 
				  R when R#newchannel.channel=:=Channel#newchannel.channel-> 
					#newchannel{
					channel      =R#newchannel.channel,
   					privilege    =vvv(R#newchannel.privilege,Channel#newchannel.privilege),
   					channelstate =vvv(R#newchannel.channelstate,Channel#newchannel.channelstate),
   					channelstatedesc=vvv(R#newchannel.channelstatedesc,Channel#newchannel.channelstatedesc),
   					calleridnum  =vvv(R#newchannel.calleridnum,Channel#newchannel.calleridnum ),
   					calleridname =vvv(R#newchannel.calleridname,Channel#newchannel.calleridname),
   					accountcode  =vvv(R#newchannel.accountcode,Channel#newchannel.accountcode ),
					application  =vvv(R#newchannel.application,Channel#newchannel.application),
					applicationdata =vvv(R#newchannel.applicationdata,Channel#newchannel.applicationdata),
   					exten        =vvv(R#newchannel.exten,Channel#newchannel.exten ),
   					context      =vvv(R#newchannel.context,Channel#newchannel.context),
   					uniqueid     =vvv(R#newchannel.uniqueid,Channel#newchannel.uniqueid ),
   					link         =vvv(R#newchannel.link,Channel#newchannel.link ),
   					date         =vvv(R#newchannel.date,Channel#newchannel.date ),
   					history      =R#newchannel.history++[Channel]
					}
				  ;
				  R -> R
				  end || X <-Session],
	             {reply,ok,NewSession}
.


