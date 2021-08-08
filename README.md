efuse_filter
====

A dependency free Binary Fuse Filter library.

They're faster and smaller than Bloom, Cuckoo, and Xor filters.

This library is API compatible with the [exor_filter](https://github.com/mpope9/exor_filter) library, and can be used as a replacement without too much hassle.

## Table Of Contents
* [Benchmarks](#benchmarks)
* [Installation](#installation)
* [Example Usage](#example-usage)
   * [Basic Usage](#basic-usage)
   * [Elixir Example](#elixir-example)
   * [Incremental Initialization](#incremental-initialization)
* [Custom Return Values](#custom-return-values)
* [Custom Hashing](#custom-hashing)
* [Serialization](#serialization)

## Benchmarks
![Benchmark Graph](/images/results.png)

This was benchmarked with the [exor_benchmark](https://github.com/mpope9/exor_bechmark) suite that compares several Erlang and Elixir bloom / xor / fuse filter implementations. The full benchmark and results can be found there.

## Installation

This library requires Erlang version 24+.

### rebar3
```erlang
%% rebar.config

{deps, [
  %% hex.pm
  {efuse_filter, "0.1.0"},

  %% git
  {efuse_filter, {git, "git://github.com/mpope9/efuse_filter.git", {tag, "0.1.0"}}}
]}.
```

### mix
```elixir
## mix.exs

defp deps do
  [
    {:efuse_filter, "~> 0.1.0"}
  ]
end
```

## Example Usage

### Basic Usage
```erlang
Filter = fuse8:new(["cat", "dog", "mouse"]),
true = fuse8:contain(Filter, "cat"),
false = fuse8:contain(Filter, "goose").
```

### Elixir Example
```Elixir
alias :fuse8 as: Fuse8

# ...

true =
    [1, 2, 3, 4]
    |> Fuse8.new()
    |> Fuse8.contain(1)
```

### Incremental Initialization

This builds a filter over multiple calls. Filters are immutable. Once finalized, more elements cannot be added.

The `exor_filter` stores elements using a compressed bitmap. This library uses the optimized Erlang `sets` version 2 module, and keeps them in Erlang memory. This library will have a higher memory footprint when initializing a filter incrementally compared to the `exor_filter`.

```erlang
Filter0 = fuse8:new_empty(),

%% Add multiple elements.
Filter1 = fuse8:add(Filter0, [1, 2]),
Filter2 = fuse8:add(Filter1, [3, 4]),

%% Add single element.
Filter3 = fuse8:add(Filter2, 5),

Filter4 = fuse8:finalize(Filter3),

true = fuse8:contain(Filter4, 1),
false = fuse8:contain(Filter4, 6).
```

## Custom Return Values
`fuse8:contain/3` can return a custom value instead of `false` if the required item isn't present in the filter:

```erlang
Filter = fuse8:new(["Ricky Bobby", "Cal Naughton Jr."]),
true = fuse8:contain(Filter, "Ricky Bobby", {error, not_found}),
{error, not_found} = fuse8:contain(Filter, "Reese Bobby", {error, not_found}).
```

## Custom Hashing
By default this library uses the [`erlang:phash2/1`](https://erlang.org/doc/man/erlang.html#phash2-1) function. If you want to use your own custom hashing, pass `none` to the `fuse8:new/2` function. Values passed to `fuse8:/new` and `fuse8:contain` need to be pre-hashed, and those functions will return a `{error, pre_hashed_values_should_be_ints}` error of non-integer values are passed.

Example usage:
```erlang
PreHashedList = [...],
Filter = fuse8:new(PreHashedList, none),
true = fuse8:contain(Filter, hd(PreHashedList)).
```

Using your own hashing method has benefits. If the filter is sent between services other platforms might not have `erlang:phash2/1` available. Also for larger filters, a hashing function with a larger key space might be desired. Keys in the C code are mapped to a `uint64_t`. Keep that in mind when choosing the hashing function.

The `exor_filter` supports passing a function to do the hashing. This library does not.

## Serialization
Functions are provided to the filter in binary form, instead of a nif reference. This can be useful to interop with other platforms / systems. The bin returned can be used with `fuse:contain` for ease of use. Example usage:

```erlang
Filter     = fuse8:new(["test1", "test2", "test3"]),
BinFilter  = fuse8:to_bin(Filter),
true       = fuse8:contain(BinFilter, "test1").
```

Build
====

```bash
$ rebar3 compile
```

Test
====

```bash
$ rebar3 eunit
$ rebar3 cover
```

Docs
====

```
$ rebar3 edoc
```
