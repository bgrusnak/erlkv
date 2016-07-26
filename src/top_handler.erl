-module(top_handler).
-include("erlkv.hrl").
-export([init/3]).
-export([content_types_provided/2]).
-export([top_text/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, top_text}
	], Req, State}.



top_text(Req, State) ->
	Ret=case db:items_list() of
		{error, Val} -> Val;
		Items -> lists:foldl(fun(X,A) -> A ++ [X#erlkv_item.key,<<"\n">>] end, [], Items)
	end,
	{Ret, Req, State}.
