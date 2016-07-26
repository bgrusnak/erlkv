-module(db).
-include("erlkv.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([items_list/0, is_item_exists/1, add_item/2, get_item/1, delete_item/1, create_schema/0, delete_outdated/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
     gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


init(_Args) ->
    {ok, []}.
    
items_list(Item) ->
  gen_server:call({global, ?MODULE}, {items_list}).

get_item(Item) ->
  gen_server:call({global, ?MODULE}, {get_item, Item}).
  
is_item_exists(Item) ->
  gen_server:call({global, ?MODULE}, {is_item_exists, Item}).
  
delete_item(Item) ->
  gen_server:call({global, ?MODULE}, {delete_item, Item}).
  
delete_outdated(Count) ->
  gen_server:call({global, ?MODULE}, {delete_outdated, Count}).
  
add_item(Item, Value) ->
  gen_server:call({global, ?MODULE}, {add_item, Item, Value}).
  

handle_call({ items_list }, _From, State) ->
	Reply=try
		case mnesia_utile:all(erlkv_item) of
			no_rows -> {error, not_found};
			not_found -> {error, not_found};
			Res -> {ok, lists:map(fun(X) -> X#erlkv.key end, Res) }
		end
	catch _:_ ->
		{error, bad_db}
	end,
	{ reply, Reply, State };

handle_call({ get_item, Item }, _From, State) ->
	Reply=try
		case mnesia_utile:find_by_id(erlkv_item, Item) of
			no_rows -> {error, not_found};
			not_found -> {error, not_found};
			Res -> {ok, Res}
		end
	catch _:_ ->
		{error, bad_db}
	end,
	{ reply, Reply, State };


handle_call({ is_item_exists, Item }, _From, State) ->
	Reply=try
		case mnesia_utile:find_by_id(erlkv_item, Item) of
			no_rows -> false;
			not_found -> false;
			_ -> true
		end
	catch _:_ ->
		true
	end,
	{ reply, Reply, State };
	

handle_call({ add_item, Item, Values }, _From, State) ->
	Value=proplists:get_value(<<"value">>, Values, null),
	TTL=proplists:get_value(<<"ttl">>, Values, null),
	Trans = fun() ->      
       case TTL of
			null -> mnesia_utile:store(#erlkv_item{key= Item, value=Value});
			_ -> mnesia_utile:store(#erlkv_item{key= Item, value=Value}),
				Timeout=calendar:datetime_to_gregorian_seconds( calendar:universal_time())+ binary_to_integer(TTL),
				mnesia_utile:store(#erlkv_ttl{key= Item, ttl=Timeout});
		end
    end,
    mnesia:transaction(Trans),
	{ reply, ok, State };

handle_call({ delete_item, Item }, _From, State) ->
	Trans = fun() ->      
		mnesia_utile:remove(erlkv_item, Item),
		mnesia_utile:remove(erlkv_ttl, Item)
	end,
	Reply=try
		 mnesia:transaction(Trans),
		ok
	catch _:_ ->
		{error, bad_data}
	end,
	{ reply, Reply, State };
	
handle_call({ delete_outdated, Count }, _From, State) ->
	Trans = fun() ->      
		Now=calendar:datetime_to_gregorian_seconds( calendar:universal_time()),
		Match = ets:fun2ms(
			fun(#erlkv_ttl{key=Key, ttl=TTL})
				when TTL < Now ->
					#erlkv_ttl{key=Key, ttl=TTL}
			end
		),
		case mnesia_utile:match(erlkv_ttl, Match, Count) of
			not_found -> ok;
			Outdated ->
				lists:foreach(fun(X) -> 
					mnesia_utile:remove(erlkv_item, X#erlkv_ttl.key),
					mnesia_utile:remove(erlkv_ttl, X#erlkv_ttl.key)
				end, Outdated)
		end
	end,
	Reply=try
		 mnesia:transaction(Trans),
		ok
	catch _:_ ->
		{error, bad_data}
	end,
	{ reply, Reply, State };
	


handle_call({create_schema }, _From, State) ->
	Trans = fun() ->      
		mnesia:create_table(erlkv_item, [{attributes, record_info(fields, erlkv_item)},{disc_copies,[node()]}]),
		mnesia:create_table(erlkv_ttl, [{attributes, record_info(fields, erlkv_ttl)},{disc_copies,[node()]}])
	end,
	mnesia:transaction(Trans),
	{ reply, ok, State };

handle_cast(_Message, State) -> { noreply, State }.
handle_info(_Message, State) -> { noreply, State }.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> { ok, State }.
