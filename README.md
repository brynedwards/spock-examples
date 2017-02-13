Isolated [Spock](https://github.com/agrafix/Spock) examples

## Stack

To run an example using Stack, first run:

`stack setup && stack build`

Then run with `stack runghc examples/<file.hs>`. You can also load them in GHCi using `stack ghci`.

## Cabal

To run using a Cabal sandbox, first run:

 `cabal sandbox init && cabal install --only-dependencies`

Then run with `cabal exec runghc examples/<file.hs>`. You can also load them in GHCi using `cabal repl`.
