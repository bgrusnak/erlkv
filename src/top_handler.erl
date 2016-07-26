-module(top_handler).
-include("erlkv.hrl").
-export([init/3]).
-export([content_types_provided/2]).
-export([top_text/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
%% Custom callbacks.
-export([create_res/2]).
init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[ <<"GET">>, <<"PUT">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, top_text}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_res}],
		Req, State}.


top_text(Req, State) ->
	Ret=case db:items_list() of
		{error, Val} -> list_to_binary(io_lib:format("~p",[{error, Val}]));
		Items -> lists:foldl(fun(X,A) -> A ++ [X#erlkv_item.key,<<"\n">>] end, [], Items)
	end,
	{Ret, Req, State}.

create_res(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	{ok, Req3} = maybe_create(Method, HasBody, Req2),
	{ok, Req3, State}.


maybe_create(<<"PUT">>, HasBody, Req) ->
	maybe_create(<<"POST">>, HasBody, Req)
;

maybe_create(<<"POST">>, true, Req) ->
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
	Key = proplists:get_value(<<"key">>, PostVals),
	db:add_item(Key, PostVals),
	{ok, Req2};
	
maybe_create(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_create(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).
