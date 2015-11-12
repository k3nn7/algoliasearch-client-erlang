# Algolia Search API Client for Erlang

Quick Start
-----------

```erlang
Object = {},
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
algolia:add_object(Index, Object).
```
