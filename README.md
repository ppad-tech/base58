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
  time                 356.9 ns   (355.7 ns .. 359.2 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 357.4 ns   (356.7 ns .. 359.4 ns)
  std dev              3.439 ns   (1.609 ns .. 6.911 ns)

  benchmarking ppad-base58/base58/decode/StV1DL6CwTryKyV
  time                 397.7 ns   (397.2 ns .. 398.1 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 397.3 ns   (397.1 ns .. 397.6 ns)
  std dev              942.3 ps   (752.8 ps .. 1.232 ns)

  benchmarking ppad-base58/base58check/encode/0x00, hello world
  time                 2.430 μs   (2.428 μs .. 2.431 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.433 μs   (2.432 μs .. 2.434 μs)
  std dev              4.715 ns   (3.868 ns .. 5.971 ns)

  benchmarking ppad-base58/base58check/decode/13vQB7B6MrGQZaxCqW9KER
  time                 2.500 μs   (2.497 μs .. 2.504 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 2.506 μs   (2.503 μs .. 2.510 μs)
  std dev              9.919 ns   (7.444 ns .. 14.66 ns)
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
