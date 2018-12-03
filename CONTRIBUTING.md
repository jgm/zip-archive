Contributing
============

# Testing Gotchas

A collection of common pitfalls

## Hackage

Don't include zip files with symbolic links as these don't work on Hackage. Create the directory structure manually as shown in the test.

## Stackage

Don't use packages newever than the ones in the current LTS as Stackage will fail building even if used as `extra-deps` in `stack.yaml`

## Nix

Don't rely on external binaries. If you absolutely need to, declare them in the cabal in the `Build-tools` section. 

