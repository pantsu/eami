-module(cowboy_eamid).

-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    Reply=
    case cowboy_http_req:method(Req) of
	{'GET',_}->cowboy_http_req:reply(404, [], <<"Unknown method">>, Req);
	{'POST',_}->
		Body=
		case cowboy_http_req:body(Req) of
			{error,badarg}->error_body;
			{ok,B,_}->
				binary_to_list(B)

			;
			_->unknown_body
		end,
		io:format('Body: ~w',[Body]),
		case (catch json2:decode_string(lists:flatten(Body))) of
			{ok,{struct,P}}->
				<<"HHH">>
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

