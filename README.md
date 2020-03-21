# kvs

`kvs` is a `proplists` replacement with a more ergonomic interface.
The selling points are:

- A convenient interface mimicking `maps`.
  Lookups take a `Default` or crash, instead of returning the dreaded `undefined`.
  Conversions between `maps` and `kvs` should be trivial.

- Compatibility with Elixir keyword lists.

- Implementation with functions from `lists`, which in turn are usually
  backed with BIFs.


## Why not `proplists`


### `maps` interop

If an example speaks to you more directly,
then I was perplexed by the following code fumbling:

```erlang
-spec options_with_defaults(proplists:proplist()) -> proplists:proplist().
options_with_defaults(Options) ->
    Defaults = #{def1 => val1,
                 def2 => val2},
    maps:to_list(maps:merge(Defaults, maps:from_list(Options))).
```

Apparently, a `proplist()` might contain not just pairs:

```erlang
property() = atom() | tuple()
```

Therefore, the code above should actually look more like:

```erlang
    maps:to_list(maps:merge(Defaults, maps:from_list(proplists:unfold(Options)))).
```

However, this stops to be a one-liner I'd like to retype in every project
and starts to become worth of its own small package.


### Fail-fast on lookup failure




## Build

    $ rebar3 compile
