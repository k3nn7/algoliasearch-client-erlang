-module(test).
-author("lukasz").

-export([start/0]).

start() ->
  ibrowse:start(),
  ssl:start(),
  Client = algolia:make_client("SKKVU7GTPP", "8c1bed50b70ea46f0aa08a540645531d"),
  io:format("~p~n", [algolia:list_indices(Client)]).
