Contributing
============

# Testing

Before submitting a PR please run the test by executing

```
$ stack test --flag 'zip-archive:enable_extra_tools'
```

# Gotchas

A collection of common pitfalls

## Hackage

Don't include zip files with symbolic links as these don't work on Hackage. Create
the directory structure manually as shown in the test.

## Stackage

Don't use packages newever than the ones in the current LTS as Stackage will fail building
even if used as `extra-deps`

## Nix

Don't rely on external binaries in test. If you need to, surround the code with

```Haskell

#ifdef _ENABLE_EXTRA_TOOLS

-- Code here
#endif

```

