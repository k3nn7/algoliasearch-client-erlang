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
Object = {[{<<"objectID">>, <<"1234">>j}]},
Client = algolia:make_client("YourApplicationID", "YourAPIKey"),
Index = algolia:init_index(Client, "IndexName"),
algolia_index:add_object(Index, Object).

```

Testing
-------
```bash
$ rebar eu
```

To do (iteration #1)
----
- [x] Initializing client
- [x] Initializing request
- [ ] Building request
- [ ] Making request over HTTP
- [ ] Logic for querying index
- [ ] Logic fot fetching index
