%% Feel free to use, reuse and abuse the code in this file.

-module(erlkv).
-include("erlkv.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API.
-export([start/0, install/0, stop/0]).

%% API.

start() ->
	mnesia:create_schema([node()]),
	ok = application:start(compiler),
	ok = application:start(syntax_tools),
	ok = application:start(sync),
	ok = application:start(crypto),
	ok = application:start(cowlib),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(goldrush),
	ok = application:start(lager),
	ok = application:start(gproc),
	ok = application:start(uuid),
	ok = application:start(mnesia),
	ok = application:start(erlkv),
	db:start_link(),
	db:create_schema()
.

stop() ->
	ok
.
