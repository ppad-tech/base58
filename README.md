# base58

[![](https://img.shields.io/hackage/v/ppad-base58?color=blue)](https://hackage.haskell.org/package/ppad-base58)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-base58-lightblue)](https://docs.ppad.tech/base58)

A pure Haskell implementation of base58 and base58check encoding &
decoding on strict ByteStrings.

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

Current benchmark figures on a M4 Silicon MacBook Air look like (use
`cabal bench` to run the benchmark suite):

```
  benchmarking ppad-base58/base58/encode/hello world
  time                 350.3 ns   (349.2 ns .. 352.0 ns)
                       0.999 R²   (0.997 R² .. 1.000 R²)
  mean                 353.0 ns   (351.9 ns .. 354.9 ns)
  std dev              4.705 ns   (3.411 ns .. 7.449 ns)
  variance introduced by outliers: 13% (moderately inflated)

  benchmarking ppad-base58/base58/decode/StV1DL6CwTryKyV
  time                 377.7 ns   (373.7 ns .. 380.7 ns)
                       0.999 R²   (0.999 R² .. 1.000 R²)
  mean                 379.4 ns   (376.8 ns .. 381.8 ns)
  std dev              9.020 ns   (7.823 ns .. 10.76 ns)
  variance introduced by outliers: 32% (moderately inflated)

  benchmarking ppad-base58/base58check/encode/0x00, hello world
  time                 1.197 μs   (1.195 μs .. 1.200 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.200 μs   (1.198 μs .. 1.202 μs)
  std dev              7.256 ns   (6.187 ns .. 8.819 ns)

  benchmarking ppad-base58/base58check/decode/13vQB7B6MrGQZaxCqW9KER
  time                 1.222 μs   (1.218 μs .. 1.225 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.220 μs   (1.217 μs .. 1.223 μs)
  std dev              9.566 ns   (8.250 ns .. 11.28 ns)
```

You should build with the 'llvm' flag for maximum performance.

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
