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

![Langjam Gamejam logo: black and white pixel art of a fierce dragon with an outstretched claw and a mouth ready to breathe fire down on the peons.](./img/langjamgamejam.png)

# Prep day 3 and 4: Evaluation

This is day 3 and 4 of preparing for Langjam Gamejam, a fun new competition where participants will build a programming language and a videogame using that language.


My goal is to write a Haskell compiler for a Scheme-like language (Lisp) targeting JavaScript. I'm not sure what the videogame will be, but because I am generating JavaScript code, I have access to easy-to-use game libraries like Kaplay. The Kaplay game library is highly declarative, so it will likely make more sense in a functional language like Scheme.


Evaluation and compilation are now possible.


For example, I can go from this Scheme-like code:


(console.log '((Math.cos 0) 2 3 "4"))


To:


console.log([Math.cos(0), 2, 3, "4"])


I implemented several Scheme features such as let-bindings and lambda functions. First, I followed the tutorial Write Yourself A Scheme V2 written by Adam Wespiser and many open source contributors. After I felt confident with a basic Lisp interpreter, the real work began.


I changed the Haskell code so it could generate JavaScript code instead of immediately interpreting the Scheme code. Then I ran the JavaScript through NodeJS to run it locally. There's some special considerations you need to take when converting Scheme into JavaScript, for example how would you convert a Scheme variable like "list-empty?" into a JavaScript name? It has a dash and a question mark, so we can't just use a JavaScript variable with the same name directly.


The Haskell code is a mess. There is much refactoring and research in my future :) Haskell's powerful type system lets me refactor the code easily to try out several solutions without breaking existing features. I'm also implementing a thorough test suite, including larger Scheme programs like an FFT function. I hate having to fix old bugs or creating new bugs when building new features.


Scheme's letrec was interesting to implement. I referred to Scheme's specification (R5RS) and it described how to implement it: define all variables and set them to `undefined` and then run each declaration's initializer in any order and assign them to the variables. This syntax allows you to refer to any variable in the letrec's scope recursively. This is useful for defining a parser or other recursive functions. For example, recursive descent parsers and parser combinators are often written using mutually recursive functions.


I also rewrote a short Kaplay demo from Javascript into Scheme.

Kaplay avoids for-loops and other imperative control structures, so it is quite easy to translate it into a functional programming language like Scheme.



