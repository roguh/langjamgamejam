# Prep day 1: Get haskell and javascript running

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

This tutorial on Youtube by [JSLegendDev](https://www.youtube.com/watch?v=Dkxwe_Gv7q4) shows that a lot of the functionality is builtin, entities have a `.jump()` method for example.

## Scheme in 48 Hours

Goal: try and implement this quickly with a variant that compiles Scheme to JavaScript (with bindings to Kaplay?).

## Purescript

The codebase is fairly concise so it might serve as a good reference point for our transpiler.

Example: the [Printer.hs](https://github.com/purescript/purescript/blob/v0.15.15/src/Language/PureScript/CodeGen/JS/Printer.hs) file converts the Purescript AST into pretty-printed Javascript source code.

# Prep day 2: Parser Generator

I found Write Yourself A Scheme, Version 2 by Adam Wespiser and open source contributors.
The GitHub has some updates missing from the blog, so I read both to get a parser running.

https://wespiser.com/writings/wyas/home.html
https://github.com/write-you-a-scheme-v2/scheme/blob/master/src/Parser.hs

Now I can parse and print S-Expressions:

```
$ stack run "(#t #f #b111 '() '(a b c))" 2>/dev/null
(#t #f 7 Nil (quote (a b c)))
```

Parsec is a monadic parser generator. I had to refresh my memory on some of the monad combinator functions like `<$>` and `<|>`. [Hoogle](https://hoogle.haskell.org) was quite useful for this.

I would've prefered building my own parser combinator, but Parsec got me going quickly.
I'll want to convert this Parsec code to a Happy parser.

I want to add line and column numbers to my LispVal.

I wonder how I'll generate JavaScript code and how I'll write the Scheme standard library in JavaScript. There must be a neat way to do this.

Finally, I finished the day with art practice.
I watched bits of a longplay of [Metroid Prime: Hunters](https://www.youtube.com/watch?v=todmHotoxfA), the 2006 videogame I grew up with, as well as a longplay of the original Metroid (1986).
Pixel art is its own art speciality, so I've been sketching some ideas.
The main character will roll into a ball when jumping and "sprinting".
I want the first boss to be a dragon, like the logo of langjamgamejam:

![Langjam Gamejam logo: black and white pixel art of a fierce dragon with an outstretched claw and a mouth ready to breathe fire down on the peons.](./langjamgamejam.png)
