# erlang-xorf <!-- omit in toc -->

This library is a NIF wrapper for the [xorf](https://crates.io/crates/xorf).

Both binary fuses and xor filters are suported in 8 16, and 32 bits per entry.

**NOTE**: The initialization list to a filter must be a unique list of 64 bit
integers. Use a good hashing function over your data and make sure to dedupe it
before passing it into the `xorf:new/2` functions. Lists with duplicates may
cause undefined behavior.

## Table of Contents

- [Table of Contents](#table-of-contents)
- [Installation](#installation)
- [Example Usage](#example-usage)
- [Serialization](#serialization)

## Installation

[Available on hex.pm!](https://hex.pm/packages/xorf).

For rebar3:

```erlang
%% rebar.config

{deps, [
  {xorf, "1.0.0"}
]}.
```

For Mix:

```elixir
## mix.exs

defp deps do
  [
    {:xorf, "~> 1.0.0"}
  ]
end
```

## Example Usage

**NOTE** The list of integers given to `xorf:new/2` must be deduped to avoid
undefined behavior.

For an xor filter:

```erlang
{ok, Filter} = xorf:new({exor, 8}, [1, 2, 3]),
true   = xorf:contain(Filter, 1),
false  = xorf:contain(Filter, 5),
```

Note that the number of bits per entry implies the likelihood of false positives
in the resulting filter.

For a binary fuse:

```erlang
{ok, Filter} = xorf:new({binary_fuse, 8}, [1, 2, 3]),
true   = xorf:contain(Filter, 1),
false  = xorf:contain(Filter, 5),
```

## Serialization

Binary serialization and deserialization is achieved using the [bincode](https://github.com/bincode-org/bincode).

For example, given a binary for an 8 bit binary fuse:

```erlang
{ok, Bin} = xorf:to_bin(Filter),
{ok, DFilter} = xorf:from_bin({binary_fuse, 8}, Bin),
true   = xorf:contain(DFilter, 1),
false  = xorf:contain(DFilter, 5),
```