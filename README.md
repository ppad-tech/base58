# ppad-base58

A pure Haskell implementation of base58 and base58check encoding &
decoding on strict ByteStrings.

[![](https://img.shields.io/hackage/v/ppad-base58?color=blue)](https://hackage.haskell.org/package/ppad-base58)
![](https://img.shields.io/badge/license-MIT-brightgreen)

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > -- import qualified
  > import qualified Data.ByteString.Base58 as B58
  > import qualified Data.ByteString.Base58Check as B58Check
  >
  > -- simple base58 encoding and decoding
  > let b58 = B58.encode "hello world"
  > b58
  "StV1DL6CwTryKyV"
  >
  > B58.decode b58
  Just "hello world"
  >
  > -- base58check is a versioned, checksummed format
  > let b58check = B58Check.encode "\NULhello world" -- 0x00 version byte
  > b58check
  "13vQB7B6MrGQZaxCqW9KER"
  >
  > B58Check.decode b58check
  Just "\NULhello world"
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/base58](https://docs.ppad.tech/base58).

## Performance

The aim is best-in-class performance for pure, highly-auditable Haskell
code.

Current benchmark figures on my mid-2020 MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-base32/base58/encode/hello world
  time                 667.4 ns   (661.2 ns .. 674.5 ns)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 673.5 ns   (669.5 ns .. 678.8 ns)
  std dev              15.91 ns   (13.00 ns .. 21.13 ns)

  benchmarking ppad-base32/base58/decode/StV1DL6CwTryKyV
  time                 741.2 ns   (731.5 ns .. 752.5 ns)
                       0.999 R²   (0.998 R² .. 0.999 R²)
  mean                 748.3 ns   (741.8 ns .. 757.2 ns)
  std dev              24.98 ns   (20.60 ns .. 31.28 ns)

  benchmarking ppad-base32/base58check/encode/0x00, hello world
  time                 5.411 μs   (5.349 μs .. 5.472 μs)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 5.389 μs   (5.352 μs .. 5.439 μs)
  std dev              142.5 ns   (119.5 ns .. 184.4 ns)

  benchmarking ppad-base32/base58check/decode/13vQB7B6MrGQZaxCqW9KER
  time                 5.825 μs   (5.722 μs .. 5.930 μs)
                       0.998 R²   (0.997 R² .. 0.999 R²)
  mean                 5.718 μs   (5.655 μs .. 5.792 μs)
  std dev              228.0 ns   (190.7 ns .. 275.4 ns)
```

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC, in
which strict constant-timeness can be challenging to achieve.

If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-base58
```

to get a REPL for the main library.

## Attribution

The vectors used in the test suite for both base58
and base58check are verbatim from paulmillr's
[scure-base](https://github.com/paulmillr/scure-base) library.

[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
