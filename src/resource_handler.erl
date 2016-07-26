-module(resource_handler).
-include("erlkv.hrl").
%% Standard callbacks.
-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).




init(_Transport, Req, []) ->
	case cowboy_req:method(Req) of
		{<<"DELETE">>, _} ->
			handle_delete(Req1);
		{<<"GET">>, Req1} ->
			handle_get(Req1)
	end.
     
allowed_methods(Req, State) ->
	{[ <<"GET">>, <<"DELETE">>], Req, State}.

handle_delete(Req) ->
	case cowboy_req:binding(res_id, Req) of
		{undefined, Req2} ->
			cowboy_req:reply(404, [{<<"content-type">>, <<"text/plain">>}], "not found", Req);
		{ReqId, Req2} ->
			Rt=case db:delete_item(ReqId) of
				ok -> cowboy_req:reply(410, [{<<"content-type">>, <<"text/plain">>}], "deleted", Req);
				_ -> cowboy_req:reply(404, [{<<"content-type">>, <<"text/plain">>}], "error", Req)
			end,
			{Rt, Req2, null}
	end.

handle_get(Req) ->
	case cowboy_req:binding(res_id, Req) of
		{undefined, Req2} ->
			{false, Req2, index};
		{ReqId, Req2} ->
			Rt=case db:get_item(ReqId) of
				{error, not_found} -> cowboy_req:reply(404, [{<<"content-type">>, <<"text/plain">>}], "not found", Req);
				{ok, Res} -> cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Res#erlkv_item.value, Req) 
			end,
			{Rt, Req2, null}
	end.
