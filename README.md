efuse_filter
====

They're faster and smaller than Bloom, Cuckoo, and Xor filters.

This library is API compatible with the [exor_filter](https://github.com/mpope9/exor_filter) library, and can be used as a replacement without too much hassle.

Table Of Contents
====

Benchmarks
====
![Benchmark Graph](/images/results.png)

This was benchmarked with the [exor_benchmark](https://github.com/mpope9/exor_bechmark) suite that compares several Erlang and Elixir bloom / xor / fuse filter implementations.

Breif benchmark snippet:
```
Operating System: macOS
CPU Information: Intel(R) Core(TM) i5-7267U CPU @ 3.10GHz
Number of Available Cores: 4
Available memory: 8 GB
Elixir 1.12.2
Erlang 24.0.5

##### With input 10,000 Inputs #####
Name                   ips        average  deviation         median         99th %
fuse8 1k            749.31        1.33 ms     ±3.17%        1.33 ms        1.49 ms
fuse8 100k          746.83        1.34 ms     ±2.79%        1.33 ms        1.48 ms
fuse8 8 1M          700.22        1.43 ms     ±4.68%        1.42 ms        1.74 ms
xor8 100k           684.07        1.46 ms     ±3.21%        1.45 ms        1.66 ms
xor8 1k             674.74        1.48 ms     ±3.15%        1.47 ms        1.63 ms
fuse8 8 10M         636.23        1.57 ms    ±13.44%        1.52 ms        2.74 ms
xor8 8 1M           598.47        1.67 ms    ±45.72%        1.54 ms        4.95 ms
xor8 8 10M          591.64        1.69 ms     ±8.51%        1.64 ms        2.44 ms
Bloomex 1k          362.02        2.76 ms     ±2.78%        2.75 ms        3.09 ms
xor16 1M            325.59        3.07 ms     ±2.58%        3.06 ms        3.44 ms

Comparison:
fuse8 1k            749.31
fuse8 100k          746.83 - 1.00x slower +0.00443 ms
fuse8 8 1M          700.22 - 1.07x slower +0.0936 ms
xor8 100k           684.07 - 1.10x slower +0.127 ms
xor8 1k             674.74 - 1.11x slower +0.147 ms
fuse8 8 10M         636.23 - 1.18x slower +0.24 ms
xor8 8 1M           598.47 - 1.25x slower +0.34 ms
xor8 8 10M          591.64 - 1.27x slower +0.36 ms
Bloomex 1k          362.02 - 2.07x slower +1.43 ms
xor16 1M            325.59 - 2.30x slower +1.74 ms
```

Installation
====

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
```
## mix.exs

defp deps do
  [
    {:efuse_filter, "~> 0.1.0"}
  ]
end
```

Example Usage
====

### Basic Usage
```erlang
Filter = fuse8:new(["cat", "dog", "mouse"]),
true = fuse8:contain(Filter, "cat"),
false = fuse8:contain(Filter, "goose").
```

### Incremental Initialization
This builds a filter over multiple calls. Filters are immutable. Once filled, more elements cannot be added.
```erlang
Filter0 = fuse8:new_empty(),
Filter1 = fuse8:add([1, 2]),
Filter2 = fuse8:add([3, 4]),
Filter3 = fuse8:finalize(Filter2),

true = fuse8:contain(Filter3, 1),
false = fuse8:contain(Filter3, 5).
```

Custom Return Values
====
`fuse8:contain/3` can return a custom value instead of `false` if the required item isn't present in the filter:

```erlang
Filter = fuse8:new(["Ricky Bobby", "Cal Naughton Jr."]),
true = fuse8:contain(Filter, "Ricky Bobby", {error, not_found}),
{error, not_found} = fuse8:contain(Filter, "Reese Bobby", {error, not_found}).
```

Serialization
====
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
