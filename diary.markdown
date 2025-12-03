# Prep day: Get haskell and javascript running

## Stack

[My global configuration](https://discourse.haskell.org/t/solved-stack-new-and-stack-init-failing-missing-aws-asset-nosuchbucket/9902) was out of date so I modified ~/.stack/config.yaml with this configuration:

```
urls:
  latest-snapshot: https://stackage-haddock.haskell.org/snapshots.json
```

- stack new haskelloni48 new-template
- stack init
- stack run

`hello world` finally runs!

with Stack, we'll have an easy way to add dependencies

## Kaplay

Goal: build a small platformer to understand how these games are made.

## Scheme in 48 Hours

Goal: try and implement this quickly with a variant that compiles Scheme to JavaScript (with bindings to Kaplay?).

## Purescript

The codebase is fairly concise so it might serve as a good reference point for our transpiler.

Example: the [Printer.hs](https://github.com/purescript/purescript/blob/v0.15.15/src/Language/PureScript/CodeGen/JS/Printer.hs) file converts the Purescript AST into pretty-printed Javascript source code.
