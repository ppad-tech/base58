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
  time                 364.8 ns   (362.4 ns .. 366.2 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 360.4 ns   (359.3 ns .. 361.8 ns)
  std dev              4.193 ns   (3.434 ns .. 5.082 ns)
  variance introduced by outliers: 10% (moderately inflated)

  benchmarking ppad-base58/base58/decode/StV1DL6CwTryKyV
  time                 355.0 ns   (352.8 ns .. 358.9 ns)
                       1.000 R²   (0.999 R² .. 1.000 R²)
  mean                 360.6 ns   (358.6 ns .. 362.2 ns)
  std dev              6.168 ns   (5.045 ns .. 7.390 ns)
  variance introduced by outliers: 20% (moderately inflated)

  benchmarking ppad-base58/base58check/encode/0x00, hello world
  time                 998.1 ns   (996.7 ns .. 999.5 ns)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 999.1 ns   (996.2 ns .. 1.002 μs)
  std dev              9.393 ns   (6.903 ns .. 11.96 ns)

  benchmarking ppad-base58/base58check/decode/13vQB7B6MrGQZaxCqW9KER
  time                 1.037 μs   (1.033 μs .. 1.040 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 1.039 μs   (1.038 μs .. 1.041 μs)
  std dev              4.926 ns   (3.584 ns .. 6.859 ns)
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
