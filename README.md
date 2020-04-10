# kvs

`kvs` is a fail-fast interface to key-value lists.
The selling points are:

- A convenient interface mimicking `maps`.
  Lookups take a `Default` or crash, instead of returning the dreaded `undefined`.

- Conversions between `maps` and `kvs` are trivial.

- Compatibility with Elixir keyword lists.

- Implementation with functions from `lists`, which in turn are backed with BIFs.


## TL;DR

Use `kvs` is you prefer:

```
> kvs:get(z, [{a, 3}]).
** exception error: {badkey,z}
     in function  kvs:get/2
        called as kvs:get(z,[{a,3}])
```

Over:

```
> proplists:get_value(z, [{a, 3}]).
undefined
```


## Use

Just add the dependency for Rebar3:

```erlang
{deps,
 [
  {kvs, "0.0.1"}
 ]}.
```


## Why `kvs`?

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

However, this stops to be a one-liner easy to retype in every project
and starts to become worth of its own small package.

### Fail-fast on lookup failure

The other reason is exception handling.
`kvs` mimics the `maps` interface, which is the most convenient one with
regard to guarantees it provides.

`kvs:get/2` guarantees to return a value we can work with or it throws
an exception:

```erlang
MyVal = kvs:get(my_key, Opts)
```

`MyVal` will either be defined and ready to use if found in `Opts`.
If not found an error will be thrown - no need to handle `undefined`.
Even better, `undefined` will not propagate through our code to cause a problem
deeper with no trace of origin.

`kvs:get/3` does not crash on lookup failure, but returns a `Default` value:

```erlang
PoolSize = kvs:get(pool_size, Opts, 10)
```

This variant does not risk crashing the process as it does not throw in any case.
This is the one to use if we can provide a reasonable default value or
want to avoid exception handling at all costs.

### Typeability and _let it crash!_

Let's consider the following code:

```erlang
-spec get_int(atom(), proplists:proplist()) -> integer() | undefined.
get_int(K, Opts) ->
    proplists:get_value(K, Opts).
```

And the following error reported by [Gradualizer](https://github.com/josefs/Gradualizer):

```
The function call on line 8 at column 16 is expected to have type integer() but it has type integer() | undefined

-spec test() -> ok.
test() ->
    accept_int(get_int(i, [{i, 3}])).
               ^^^^^^^^^^^^^^^^^^^^
```

A lookup returning the value itself OR `undefined` on lookup failure is
inconvenient to handle in code - it requires defensive programming,
i.e. enclosing each call site in `case get_int(...) of undefined -> ...`.

The Erlang philosophy is to _let it crash_ in such cases.
Make sure the code leading to `get_int(i, ...)` passes a list that has `i` defined.
However, if due to an error it's not there - fail fast and let it crash!
The closest `try ... catch` block will let us recover if the problem is
recoverable or if it has to be reported back to the user.

Crashing on lookup failure leads us to a cleaner spec, no warnings of arguable usefulness,
and no need for defensive `case ... of undefined` at each call site:

```erlang
-spec get_int(atom(), proplists:proplist()) -> integer().
get_int(K, Opts) ->
    case proplists:get_value(K, Opts) of
        undefined -> erlang:error(not_found, [K, Opts]);
        V -> V
    end.
```

That's the approach `kvs` takes in `get/2` and `get/3` implementation
for maximum convenience of use.
