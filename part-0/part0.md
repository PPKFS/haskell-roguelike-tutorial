---
author: ["Avery"]
title: "Haskell Roguelike Tutorial Part 0 - Introduction and Setting Up"
date: "2025-03-30"
description: |
  The Python+libtcod roguelike tutorial series and all of its spinoffs for other languages and frameworks have been super popular in the roguelike gamedev space for over a decade now. So, this is me attempting to do the same but for Haskell.

summary: "An introduction to the planned blog posts, setting up a project, and getting a window open."
tags: ["roguelike", "tutorial", "projects", "haskell"]
categories: ["haskell"]
series: ["roguelike-tutorial"]
ShowToc: true
modified: "2025-06-15"
TocOpen: true
weight: 2
social:
  bluesky: "ppkfs@bsky.social"
---

# Introduction

## Motivation

Welcome! This is the zeroth part (the prologue, as it were) of what I hope will be a complete series on how to make a roguelike game in Haskell. The [original roguelike tutorial](https://www.roguebasin.com/index.php?title=Complete_Roguelike_Tutorial,_using_python%2Blibtcod) has been endlessly used (I remember going through it all with Python in...god knows when. Probably early 2010) and loved and adapted to many other languages and frameworks for Rust with `rltk`, Javascript with `rot.js`, C# with `RogueSharp`; for Godot, for Unity, and a ton more I'm missing. "r/roguelikedev does the roguelike tutorial" has
become a yearly event. There's a great roguelike fan Discord server with an active dev chat. Making roguelikes is great fun, and it's easier than people think.

There's a kind of mental jump going between "I can solve all these problems in a given language" and "I can write an application (possibly a game)". Maybe the reason these tutorial series are so popular, despite being very clear that nothing they are introducing to the reader is new to them, is because they provide a welcoming helping hand. They take you from being able to write a bunch of classes or functions to an actual *game* with a randomly generated dungeon with *monsters* and *combat* and *items* and all that cool stuff.

This is a bit of a challenge to bring to Haskell. There's a reputation about how purity and functional programming isn't suited to gamedev. This is ~~completely~~ mostly false, of course - it's far more doable than you think! A lot of the flak Haskell gets is that it's complicated (not unless you make it so), you need to know all about monads and algebraic effects and functional lenses (you don't), or that it's not suited to most sorts of application (again, not true). I want to dispel these fictions.

This tutorial **won't be an introduction to Haskell for completely new people to the language**, but the intended level is at the advanced beginner. Perhaps having taken most of a functional programming course, or having read Learn You A Haskell or Real World Haskell. If you are coming from another functional language (F#, Scala, OCaml) then *most* of the concepts should be familiar to you and it is mostly syntax that isn't.

If the following list doesn't make you run away screaming, then fingers are crossed that this should be understandable:
- The Haskell language
  - Datatypes, records, ADTs, polymorphism, typeclasses, constraints, functions;
  - Polymorphism (type parameters, polymorphic functions);
  - Data structures - lists/vectors/arrays, maps, sets;
  - `map`/`fmap`/`mapM`, `filter`, `fold`;
- Monads
  - do-notation;
  - Basic monads - at the very least `State` and `IO`;
  - Simple monad stacks, in whatever form (`StateT s IO a` or `MonadState s m => m a` or `State s :> es => Eff es a`) - or at least the concept of putting two monads together;
- modules;
- `cabal` - adding build-depends, modules, building and running projects

It'd be *very useful* to know about, but these will be covered in a way that doesn't require prior knowledge:

- Language extensions (e.g. `OverloadedLabels`, `OverloadedStrings`, `NoImplicitPrelude`, `TypeApplications`, etc)
- `mtl`
- Basic roguelike concepts like generating tile-based dungeons and an obsession of rendering things in ASCII characters, but that's why you're here - right?

## Planned tutorial structure

- [Part 0 (this one): Introduction, setting up the project, opening a window](https://ppkfs.github.io/posts/roguelike-tutorial/part0/)
- [Part 1: Drawing a @ and moving it around](https://ppkfs.github.io/posts/roguelike-tutorial/part1/)
- Part 2: Generating a couple of maps
- Part 3: Field of view
- Part 4: Monsters, AI
- Part 5: Combat
- Part 6: UI
- Part 7: Items and inventory
- ???

## Some style notes

My primary objective for this tutorial is to write *easily understandable* Haskell, not necessarily the most performant or the way I would do it personally.

> [!EXPERIMENT]
> I'll use Experiment bubbles to briefly describe possible alternative ways to implement something in a more complex way. These are completely optional!
> They'll often have a convenient link to an expanded discussion at the bottom of the page, like this: [Other useful tools, libraries, and techniques for Haskell gamedev.](#other-tools-libraries-and-techniques)


# Project structure setup

The rest of this part is setting up an empty cabal project, installing the relevant libraries, and verifying that indeed the hell of library paths has been solved.

I'm going to assume you have a working Haskell setup already. `ghcup` is almost certainly your best bet for doing it all painlessly. I've tested the code with GHC 9.8.2 and with 9.12.2, so it should be fine with 9.10 and probably 9.4/9.6.

## Installing `bearlibterminal`

First, we need to download [bearlibterminal](https://github.com/cfyzium/bearlibterminal), which is the graphics library that `roguefunctor` is built on. There are prebuilt binaries available at the bottom of the readme on GitHub. If you are on a platform that doesn't have a prebuilt binary (for example, an M1 Macbook with Apple Silicon) then you can build it yourself:

```bash

git clone https://github.com/cfyzium/bearlibterminal.git
mkdir bearlibterminal/Build
cd bearlibterminal/Build
cmake ../
make all
```

With the library file (`.so`,`.dll`, `.dylib`) you have two options:

- you can either install this yourself into `/usr/lib` or equivalent.
- you can copy the library file into some directory of your project and pass the library option to cabal with `--extra-lib-dirs=/path/to/the/library`. The downside of this method is that you do *also* need to set `LD_LIBRARY_PATH`(Linux) or `DYLD_LIBRARY_PATH` (Mac) or something else (Windows) to actually do `cabal run`, because it doesn't copy the library into the cabal build directory. If anyone knows cabal better than I, please let me know how to set this up! PRs also incredibly welcome.

> [!EXPERIMENT]
> There is a WIP branch of `bearlibterminal-hs` that tries to avoid this by having Cabal build the C library. It's available in my repository on a branch here: https://github.com/ppkfs/bearlibterminal-hs.git.

## Making a new `cabal` project

Let's begin by making a new project directory and initialising a blank `cabal` project. We'll go for the unexciting name `hs-rogue`.

```bash
mkdir hs-rogue
cd hs-rogue
cabal init
```

This will start the interactive wizard for setting up a new cabal project.

> [!WARNING]
> If you don't get an interactive command line prompt, you're probably using an old version of `cabal`. It's very useful to have at least version 3.4 for `GHC2021`. If not, there will be a full cabal file available at the end of the post.

The options of importance:

- we want just an executable project. In the future we might want to extract some things into a library if we wanted to build off our framework, but for now we just want to make an app.
- we want **cabal version 3.4 minimum**, because `default-language: GHC2021` saves a lot of time with writing out language extensions!
- we want to use `GHC2021` as the language for our executable, as this enables a bunch of language extensions for us.

The reason I suggest `GHC2021` is because of the following language extensions - there's probably more that are enabled and used, but these are the major ones:

- `TupleSections` - Another quick timesaver - `( , a)` rather than `\x -> (x, a)`.
- `DisambiguateRecordFields` - I'm not a fan of prefacing record field names with `typename`, so this saves the headache of the compiler complaining that it cannot identify which `width` is meant in the record update `f { width = (width f) + 1 }`.
- `DeriveFunctor` - Being able to derive functor instances for things that look like wrappers around an `a` with a bunch of additional data is super handy to have.

> [!EXPERIMENT]
> `DeriveGeneric` - Deriving Generic where possible is great primarily for the 'free' `LabelOptic` instances it gives you when using `optics` (which we are not in this tutorial) but also for making it simple to derive `FromJSON/ToJSON` when it comes to saving and loading our game.

This gives us `hs-rogue.cabal` pre-populated. Currently (as of June 2025), one of the libraries we need - `roguefunctor` - is not on Hackage, so cabal cannot automatically download it. We need a `cabal.project` file that specifies where the repositories for this library can be found as well as the packages in our project. We only have one package, but otherwise `cabal run` will complain that the project file lists no packages to run. Create `cabal.project` in the root directory of the project and add the following:

```cabal
source-repository-package
  type: git
  location: https://github.com/PPKFS/roguefunctor.git
  tag: main
packages:
  hs-rogue.cabal
```

We specify that for the `roguefunctor` library, it's available as a git repo at the above URL and the version of the repository we want is the `main` branch.

Whilst we're at it, it's safe to assume we would like `haskell-language-server` to work with this project. Whilst the language server shouldn't have a problem working without it, it can sometimes be a bit...temperamental. We'll add a `hie.yaml` to be safe, which is a config file that tells HLS where to find the `hs-rogue` executable component of our project:

```yaml
cradle:
  cabal:
    - path: "hs-rogue/app"
      component: "executable:hs-rogue"
```

So now we will have tooltips and code actions and hints. Nice.

### Initial dependencies
We've got one final setup step, and that's adding some dependencies and extensions to the `cabal` file. These are the ones we'll need for the first few parts of the tutorial. I'll make sure in future parts to put all the modifications to the `.cabal` file at the start of the post. Under `build-depends`, add:

```cabal
  build-depends:
    , base
    , bearlibterminal
    , containers
    , mtl
    , random
    , roguefunctor
    , text
```

From the top:

- `base` - I'd suggest this tutorial might be a bit hard to follow if you're unsure why we need this. :)

- `bearlibterminal` and `roguefunctor` are the two libraries we are using for this tutorial. You *could* very easily do this tutorial without `roguefunctor` and just use `bearlibterminal`. However, it introduces some useful abstractions (like viewports, event handling, colours, field of view algorithm[s], and so on) so it means we can spend more time making a game and less time writing engine infrastructure.

- `containers` - as we're basically doing everything as a stateful `Map` of IDs to various *things*, this is kind of key to the whole thing.
- `mtl` - Originally this series was going to avoid bringing in `mtl` and `optics` but I quickly discovered it was quite awful to avoid them. For all we need of it (constraints rather than writing our own typeclasses), it's very simple. `optics` won't come in until the second half of part 3, which I plan to include a decent enough explanation of the idea of lenses so fear not.
- `text` - `String` is horribly slow and memory hungry. The libraries are based around using `Text` and we will use `Text` as we should.

Now if you run `cabal build`, it should download and build the dependencies. You'll know your setup for installing `bearlibterminal` was correct if it successfully builds, as otherwise it will give errors about missing C libraries.

> [!WARNING]
> If you get an error at this stage about cabal being unable to resolve conflicts, it could be one of two things: 1) you've not run `cabal update`, implied by messages like "could not find package 'random'" or 2) your currently installed GHC version is too low. Whilst I am writing this using 9.8, it should work on 9.4 upwards.

> [!WARNING]
> If you cannot *build* the project because of missing libraries, make sure you are supplying `--extra-lib-dirs` with wherever your bearlibterminal library is!

### Default extensions
Finally we can add a couple of default extensions. `GHC2021` automatically contains most of the "key" extensions for writing comfy modern Haskell, but there's at least three more I suggest:

```cabal
  default-extensions:
    LambdaCase
    NoImplicitPrelude
    OverloadedStrings
    default-language: GHC2021 -- if it's not already added
```

- `LambdaCase` saves an awful lot of time to just write `\case` rather than `\x -> case x of`. `NoImplicitPrelude` removes the implicit `import Prelude` from every module, thus allowing you to use your own Prelude. This works better than cabal mixins, which aren't great when it comes to integrating with HLS. In this tutorial, we'll write our own `HsRogue.Prelude` - a very thin wrapper around `Prelude` but with some extra re-exports that are still quite strangely not exported by the regular one - `Text`, `forM_`, `(&)`, `void`, and so on - as well as some imports we want to use everywhere in our project, like `Rogue.Geometry.V2`./????????????
- `OverloadedStrings` allows string literals to be interpreted as any instance of `IsString` rather than just as `String`. This is because we want to use `Text` over `String` for efficiency, as you should do in Haskell.

I also typically use `TemplateHaskell` and `RecordWildCards` on a per-module basis.

> [!EXPERIMENT]
> In `roguefunctor` there is also a custom prelude - `Rogue.Prelude` - that is a thin wrapper around [relude](https://hackage.haskell.org/package/relude). Whilst I do like to use `relude` myself - `Text` over `String` by default, no partial functions, polymorphic `show`, and many other nice-to-haves - `Rogue.Prelude` is geared towards using `effectful` and `optics` rather than `mtl`.

# Opening a window

To verify that everything has been set up correctly, you can open a window by copying this into `Main.hs` - a full explanation will be in Part 1, along with drawing some things to the screen. In short, we use `withWindow` to open a window (with some provided options) and run our main event loop. The main loop refreshes the window, blocks until some input event is received (either a window event or a keypress) and loops if this is anything other than a window close event or an `Escape` key press.

```haskell
module Main where

import Prelude -- we'll make our own prelude in part 1

import BearLibTerminal ( terminalRefresh )
-- note that pattern synonyms - such as TkResized, TkClose, and TkEscape -
-- have a different import syntax.
import BearLibTerminal.Keycodes

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )

import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents )
import Rogue.Geometry.V2 ( V2(..) )
import Rogue.Window ( withWindow )

screenSize :: V2
screenSize = V2 100 50

main :: IO ()
main =
  withWindow
  defaultWindowOptions { size = Just screenSize, title = Just "HsRogue - Part 0" }
  (return ()) -- no init logic
  (const $ liftIO runLoop)
  (return ()) -- no shutdown logic

runLoop :: IO ()
runLoop = do
  terminalRefresh
  -- event handling
  shouldContinue <- handleEvents Blocking $ \case
    TkClose -> return False
    TkEscape -> return False
    _ -> return True
  when (and shouldContinue) runLoop
```

> [!NOTE]
> I'll be mostly using explicit import lists for external libraries to make it clearer where everything is coming from. Whilst it is a good practice to have, it's not something I normally do!

You can run your program with `cabal run hs-rogue`. If everything goes to plan, you should now have a black window open!

> [!WARNING]
> If you can build but not *run* the project because of missing libraries, make sure you've copied `libbearterminal.so/dylib/dll` to a location on your PATH or use `export LD_LIBRARY_PATH/DYLIB_LIBRARY_PATH`.

Maybe there will be more things, which can be added later.

# Wrapping up

Well, that's it for part 0. Not exactly much coding, but we can now hit the ground running for [Part 1](https://ppkfs.github.io/posts/roguelike-tutorial/part1/)!

The code - well, a blank project structure - is available at the accompaying github repo [here](https://github.com/PPKFS/haskell-roguelike-tutorial-parts/).

Hopefully this was understandable - any feedback is greatly appreciated. You can find me on the various functional programming/roguelike Discords as ppkfs and on Bluesky as @ppkfs.bsky.social.

Hope to see you for Part 1 soon!

# Postscript

## Full cabal file

```cabal
cabal-version: 3.6
name:          hs-rogue-part0
version:       0.0.0.0
synopsis:      Roguelike tutorial in Haskell.
description:   Part 0 of the roguelike tutorial in Haskell.
homepage:
  https://github.com/PPKFS/roguelike-tutorial-parts/tree/part-0

bug-reports:   https://github.com/PPKFS/roguelike-tutorial-parts/issues
license:       MIT
author:        Avery
maintainer:    Avery <ppkfs@outlook.com>
copyright:     2024-2025 Avery
category:      Game
build-type:    Simple
tested-with:   GHC ==9.12.2 || ==9.8.2

executable hs-rogue
  main-is:            Main.hs
  default-extensions:
    LambdaCase
    NoImplicitPrelude
    OverloadedStrings

  build-depends:
    , base
    , bearlibterminal
    , containers
    , random
    , roguefunctor

  hs-source-dirs:     app
  default-language:   GHC2021
  ghc-options:        -Wall
```
