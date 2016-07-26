-module(resource_handler).
-include("erlkv.hrl").
%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).

%% Custom callbacks.
-export([create_res/2]).
-export([pass_rest/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[ <<"GET">>, <<"PUT">>, <<"DELETE">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, []}, pass_rest},
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_res}],
		Req, State}.

delete_resource(Req, State) ->
	case cowboy_req:binding(res_id, Req) of
		{undefined, Req2} ->
			{true, Req2, index};
		{ReqId, Req2} ->
			db:delete_item(ReqId),
			{true, Req2, ReqId}
	end.

resource_exists(Req, _State) ->
	case cowboy_req:binding(res_id, Req) of
		{undefined, Req2} ->
			{false, Req2, index};
		{ReqId, Req2} ->
			{db:is_item_exists(ReqId), Req2, ReqId}
	end.

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


pass_rest(Req, _State) ->
	case cowboy_req:binding(res_id, Req) of
		{undefined, Req2} ->
			{false, Req2, index};
		{ReqId, Req2} ->
			Rt=case db:get_item(ReqId) of
				{error, not_found} -> false;
				{ok, Res} -> Res
			end,
			{Rt, Req2, ReqId}
	end.
