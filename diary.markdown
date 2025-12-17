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




# Prep day 5 and 6: More evaluation and JS vs. Scheme

This is day 5 and day 6 of preparing for Langjam Gamejam, a fun new competition where participants will build a programming language and a videogame using their new language.

(I'm posting this late, this happened a few days ago.)

My goal is to write a Haskell compiler for a Scheme-like language (Lisp) targeting JavaScript.

Added features:
- Double syntax, and both explicit and implicit bigint
- Dicts
- Bigint, fft in scheme
- Tons of new stdlib required, list construction and iteration, no for-loops! heavily using JS immutable features like array cloning.
- Should I use scheme or js or both? scheme is wordier but js is harder.
- Clojure is good inspiration.

Dreams: LSP, good repl with autocomplete, python-style docstrings, numpy/pandas inspiration for easy numeric programming, formatter, AI-friendly AGENTS.md for working with this lang.

Live FFT (spectrogram) of mic data. Make the generated code JUST AS FAST as well-written JavaScript.

What videogame will we write with this lang? stick around to find out!

# Prep day 7: GLEAM!

This is day 7 of preparing for Langjam Gamejam, a fun new competition where participants will build a programming language and a videogame using their new language.

I've reflected on which languages I want to dedicate my time to, and I've changed my mind on the plan for the hackathon, which starts tomorrow Saturday at 10AM Mountain Time (UTC-7).

My goal is to write a compiler in Gleam targeting the web browser.
The source language will be Python-like. Maybe a Python with curly braces to make parsing easier.
I don't want to waste time on parsing when I only have 7 days.
The target language will be JavaScript.

In just a few hours, I built an HTML/CSS webapp using Gleam's Lustre framework.
It is pleasant to work with and very productive if you've used languages like Haskell and Ocaml before.
The webapp seems fairly efficient.
Gleam was easy to configure, it even has hot code-reloading to make it easier to test and debug while I'm developing my compiler.

Because langjamgamejam is a language and game competition, I built a simple code editor in Gleam today to prepare.
My webapp allows the user to load a predefined program, edit its source code, and it compiles and runs the source code directly in the web browser.
I'm only parsing a JSON-like subset of JavaScript, not my new Python-like language!

I'm quite happy with Gleam.

I'm also collaborating with a friend, who's a published writer and fond of science fiction.
We're working on a pretty cool story.
Hopefully I can express enough of my friend's writing in the videogame next week :)

I'll also draw as much as possible, especially organic elements like overgrown trees and mossy drones.

Wish us luck! And good luck to all lovers of programming languages, linguistics, and videogames!

# Diary: Day 1 and 2

Animation, platformer movement similar to Hollow Knight

# Diary: Day 3

Yarn Spinner dialog compiler to Kaplay.
Make it easy to compile Yarn scripts to other game frameworks as well.
Substantial tests.

```
# arch linux
sudo pacman -Syu rebar3
trizen -Syu aur/gleam-bin
sudo ufw allow 9999

# install gleam
gleam new project-name
gleam add lustre atto gleam_json
gleam add --dev lustre_dev_tools
gleam run -m lustre/dev start
```

Python-like syntax?
Animation language?

# Diary: Day 3 and 4

This is day 4 of the Langjam Gamejam, a fun new 7-day hackathon where participants are building a programming language and a videogame using their new language.

I spent this weekend drawing, animating, and building good controls for a platformer in the Kaplay game engine for web browsers. This is my first time animating a human figure and I'm quite happy with the character's motion, though your opinion may vary.

Still, I couldn't think of an interesting programming language to implement as part of the langjamgamejam, a 7 day hackathon where participants are building a videogame and a programming language. Monday evening, I looked into languages for implementing videogame dialogue and storytelling systems. Yarn Spinner is writer-friendly, yet it is a complete programming language. By late Monday night, I had a basic Yarn parser running in Gleam on my browser.

I am now implementing a nearly-complete Yarn parser and VM in Gleam for the browser. I'm omitting indent-sensitive grammar and Yarn's cool new Saliency features. I recall the gamejam discord had a good discussion on implementing whitespace-sensitive parses, like Python's and Haskell's, but I'll save this work for later.

I will integrate the Yarn VM into my platform videogame built using Javascript Kaplay. I will continue to support live code editing, but only for Yarn files so that users can modify in-game conversations and observe their changes live!

Yarn is a Turing complete language and I'd like to practice my VM implementation skills. As a stretch goal, I'll also add for-loops, lists, and mathematical functions to my Yarn VM. I'll run some benchmarking compared to general purpose languages. A more practical Yarn extensions would be using generative AI for more dynamic speech patterns, or integrating with multiplayer games.

In terms of languages, I believe JavaScript and HTML strike a good balance between performance and readability, especially for newcomers and people not interested in learning programming.
This is why I'm liking Yarn.
It has a lightweight syntax that lets writers use as much or as little of Yarn's programming features as they need.

My friend, a published writer, is contributing her talent to write a good story for the game. We've both dreamed of building videogames. The game (and editor) will be available on the nearest web browser near you. Stay tuned!


title: YarnInKaplayInGleamIn7Days
---
Ready to continue?
-> No?
-> Yes!
-> What an interesting Yarn example.
  Indeed
  <<jump>>
===

# Day 5: Rewrite in Phaser

```
npx @phaserjs/create-game@latest  # pick vite with typescript
```

phaser extension
console and typescript checker ESSENTIAL for viewing errors
FPS extension is cool

