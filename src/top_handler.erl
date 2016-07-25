-module(top_handler).

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
	{<<"">>, Req, State}.
