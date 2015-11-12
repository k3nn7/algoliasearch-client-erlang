# Algolia Search API Client for Erlang

Setup
-----

Add to `deps` section in your `rebar.config`:
```erlang
{algolia, ".*", {git, "git://github.com/k3nn7/algoliasearch-client-erlang.git", "7891cb5f0ed0011bb510b376e862d323fb4759bd"}}
```

And run:
```bash
$ rebar get-deps
```

Quick Start
-----------
```erlang
Object = {},
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
algolia:add_object(Index, Object).
```
