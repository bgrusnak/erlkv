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
	{[ <<"GET">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, top_text},
		{<<"text/plain">>, top_text}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_res}],
		Req, State}.


top_text(Req, State) ->
	Ret=case db:items_list() of
		{error, Val} -> list_to_binary(io_lib:format("~p",[{error, Val}]));
		{ok, []} -> <<"">>;
		{ok, [First|Items]} -> lists:foldl(fun(X,A) -> A ++  [<<"\n">>,X] end, [First], Items)
	end,
	{Ret, Req, State}.

create_res(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req),
	Rt= case {Method, HasBody} of
		{<<"POST">>, true} ->
			{ok, PostVals, Req3} = cowboy_req:body_qs(Req2),
			Key = proplists:get_value(<<"key">>, PostVals),
			Created=db:add_item(Key, PostVals),
			true;
		_ -> false
	end,
	{Rt, Req, State}.
