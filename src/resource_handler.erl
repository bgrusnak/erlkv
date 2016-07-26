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
-export([pass_rest/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[ <<"GET">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{{<<"text">>, <<"plain">>, []}, pass_rest}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, create_res}],
		Req, State}.

delete_resource(Req, State) ->
	case cowboy_req:binding(res_id, Req) of
		{undefined, Req2} ->
			{false, Req2, State};
		{ReqId, Req2} ->
			db:delete_item(ReqId),
			{true, Req2, State}
	end.

resource_exists(Req, _State) ->
	case cowboy_req:binding(res_id, Req) of
		{undefined, Req2} ->
			{false, Req2, index};
		{ReqId, Req2} ->
			{db:is_item_exists(ReqId), Req2, ReqId}
	end.


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
