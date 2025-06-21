---
author: ["Avery"]
title: "Haskell Roguelike Tutorial Part 1 - Drawing & Moving"
date: "2025-04-18"
modified: "2025-06-19"
description: "This is the first 'proper' tutorial post; there's a walkthrough of the window opening example before jumping in with moving an @ around the screen."
summary: "Walking through the window opening code and drawing and moving a character."
tags: ["roguelike", "tutorial", "projects", "haskell"]
categories: ["haskell"]
series: ["roguelike-tutorial"]
ShowToc: true
TocOpen: true
draft: false
weight: 1
social:
  bluesky: "ppkfs@bsky.social"
---

# Introduction

Welcome back for the first proper part of the Haskell roguelike tutorial! In this part, we're going to:
- first briefly go over the window opening code from Part 0,
- setup our own custom prelude,
- draw a player character to the screen,
- add input handling to move around.

# Going over the window opening code

In the first part, we opened a (blank) window just to verify that we had setup the project correctly and had the libraries working. Whilst the code is relatively straightforward, it's worth covering it to make sure we can hit the ground running.

```haskell
module Main where

import Prelude

import BearLibTerminal ( terminalRefresh )
import BearLibTerminal.Keycodes

import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )

import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents )
import Rogue.Geometry.V2 ( V2(..) )
import Rogue.Window ( withWindow )

screenSize :: V2
screenSize = V2 100 50
```

As we have `NoImplicitPrelude` enabled (very shortly for an actual reason) we need to explicitly import `Prelude` here. We have a small smattering of other imports for getting a basic window up:
- `Control.Monad` for `when` (soon we won't need to!)
- `BearLibTerminal` (for keycode/other events and terminal functions)
- `Rogue.Config` gives us `defaultWindowOptions`
- `Rogue.Events` gives us an event handling loop
- `Rogue.Geometry.V2` gives us a nice 2D vector type (this one is also soon to disappear)
- `Rogue.Window` has functionality for handling initialising and closing a window.

For now, we just want to hard-code the screen size (and ignore resizing or having it in a configuration file). We also want to, at least initially, tie the map size to the screen in some form - so we define it at the top level so it can be passed around later to our map generation functions.

```haskell

main :: IO ()
main =
  withWindow
  defaultWindowOptions { size = Just screenSize, title = Just "HsRogue - Part 0" }
  (return ()) -- no init logic
  (const $ liftIO runLoop)
  (return ()) -- no shutdown logic
```

Our `main` function delegates everything to `withWindow`, a convenient wrapper around `bracket`.

> [!INFO]
> For those who haven't seen `bracket/bracket_` before, it allows us to wrap some `IO a` action with an initialisation or acquisition logic and a finalising or releasing logic. Even if the internal computation hits a runtime exception or exits unexpectedly, the resource will be safely released. From the [Haskell wiki](https://wiki.haskell.org/Bracket_pattern), 'In general, throughout the libraries many functions having names beginning with `with` are defined to manage various resources in this fashion. One passes in a function of the allocated resource that says what to do, and the with-function handles both the allocation and deallocation.'


`withWindow` isn't *exactly* the same as `bracket` - `bracket`'s type signature looks like `IO a -> (a -> IO b) -> (a -> IO c) -> IO c`, where the order of parameters goes acquire-release-action and the resource acquired is passed as a parameter to both the main action and releasing action. In contrast, `withWindow` is:

```haskell
withWindow ::
  MonadIO m
  => WindowOptions -- to configure the window
  -> m a -- the initialisation logic to run *after* the window has been initialised
  -> a -> m b -- everything happens here
  -> m c -- the final logic to run *before* the window has been closed
  -> m b
```

Note that the order is shifted: because we typically want to run the middle action as a loop (i.e. we need it to recurse) rather than as a one-off, we don't need the benefits of writing `bracket init close $ \resource -> do ...`. Writing it in this order feels more sensible.

As we don't need to do any resource acquisition or releasing - `withWindow` handles the window for us, obviously - we do nothing with `return ()` both times. For us, it's less about the guarantee that we free the resource (this is much more critical in e.g. IO-sensitive operations) but it does make it a lot easier to abstract out "doing window setup" from "actual game stuff".

We'll use the initialisation part of `withWindow` in the next part when we start building a map and have a complex gamestate that we need to assemble.

> [!INFO]
> `bearlibterminal` as a library is based around strings. Window configuration options, config file loading, font alignment and glyph mapping - it almost all can be done with strings passed to `terminalSet`. You can do this too in Haskell land if you want! However for the most part, this is obviously Not Good in terms of type safety! `roguefunctor` instead allows you to set configuration options with normal Haskell records and deals with the stringification. This will come up more in Part 2 when we want to change the font, but it's all in `Rogue.Config` and the `BearLibConfigString` class if you're interested. The upside is that there's nothing special needed to use the more niche configuration options here - http://foo.wyrd.name/en:bearlibterminal:reference:configuration.

```haskell
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

The first thing we need to do in our game loop is to refresh the screen. This is required by `BearLibTerminal` to show the window after it's been initialised. `withWindow` doesn't do this step for us (in case e.g. you want to render the scene before showing the window).

For event handling we use `handleEvents` to iterate over all the input events that have been registered since the last time it was checked. Usually this will just be one event, but it can be multiple! An event here is a keypress, a mouseclick, or a window event (such as someone closing or resizing it). The event queue is polled once per time, and any events that have occured in the meantime are added to the queue.

We can either do this with `Blocking` enabled (where it will wait if no events are pending) or `NonBlocking` (where, if there are no events, it'll return `[]` and continue execution). For now, because we want to just wait for a keypress we use the blocking mode. If we get a window close event (the user pressed the X) or an `Escape` keypress, we return a `False` and if we have any other event we return `True`. As this is a *list* of booleans (if we get multiple events at the same time), we want to loop - via recursion - if **none** of the booleans are `False` (that we did not get a close or escape event). When you think about it, if none of them are False, then all of them are True. This means we simply loop if **all** the booleans are `True`! If this is the case we go back into the loop, and if not then the function ends. This is the functional equivalent of a `while` loop with a `break` condition.

Nothing currently happens, but the window should open. It only renders once per event it receives, but as it's only rendering a black screen it doesn't matter. In the (far) future when we get to animations we will want to decouple the rendering from the event loop, but for now we are good to only render once per turn.

So hopefully this was all fine to follow! Once again none of the things in `roguefunctor` are necessary, but they sure save time. One more small thing before we start actually drawing to the screen to save us some time later..

# Making our own Prelude

There's an awful lot of problems with the standard prelude, as you've probably experienced in your Haskell usage. Many functions aren't exposed (like `when`, for instance). Everything is `String`. There's a lot of partial functions that will crash if you look at them wrong (looking at you, `head`).  Custom preludes exist and are quite good, but for this project we want to keep it relatively simple.

Most of the stuff in the Prelude is fine for us, but there's a bunch of things we are going to make a lot of use of that would be easier if we had them bundled. So that's what we're going to do - make a module that simply re-exports the entire Prelude module and another half dozen modules, and then import that in each file.

Let's make a new folder for our game logic modules and call it `HsRogue`, and then make a new `HsRogue.Prelude` module in there:

```haskell
module HsRogue.Prelude
  ( module Prelude
  , module Data.Maybe
  , module Control.Monad
  , module Control.Monad.State.Strict
  , module Rogue.Geometry.V2
  , module GHC.Generics
  , Text
  ) where

import Prelude

import Control.Monad
import Control.Monad.State.Strict

import Data.Maybe
import Data.Text (Text)

import GHC.Generics

import Rogue.Geometry.V2
```

This module will mostly be the re-exporting of entire modules rather than containing our own code. We certainly will write a few functions that would normally go into a `Utils` file or module in the future.

Here, we just re-export the entire modules we will be using the most:
- `Data.Maybe` is very handy for inline pattern matching on `Maybe` (e.g. `fromMaybe :: a -> Maybe a -> a`).
-`Control.Monad` gives us convenient control flow options like `when` and `forM` (arguably the more useful cousin to `mapM` for us).
- As games are inherently stateful, and we're going to be working almost exclusively in `StateT s IO`, having this always imported helps.
-`GHC.Generics` is handy for when we want to derive `Generic` for our datatypes. A secret tool to help us later.
- `Rogue.Geometry.V2` is just ubiquitous.
- Finally, we only want to export the `Text` type from `Data.Text`. If we want the rest of the functions we'll explicitly import it qualified to avoid clashing with `Text.map` and `Text.fold` and similar.

> [!WARNING]
> Make sure to add `HsRogue.Prelude` to your `other-modules` in your `hs-rogue.cabal` file! I won't be mentioning it consistently but when you make a new module, remember to add it to the cabal file to avoid linker issues later on.

> [!EXPERIMENT]
> Another option would be to use the bundled `Rogue.Prelude` module, which is based off `relude`. It does basically the same thing we've done, except it includes things like `Bifunctor`.

Let's amend our current `Main` module to use the new prelude:

```haskell
import HsRogue.Prelude

import BearLibTerminal ( terminalClear, terminalRefresh )
import BearLibTerminal.Keycodes

import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents_ )
import Rogue.Rendering.Print  ( printText_ )
import Rogue.Window ( withWindow )
```

Now with a more convenient prelude out of the way, let's **finally** start on drawing to the screen.

# Drawing the @

We'll start by adding some state to our game. A game is an inherently stateful program; whilst we could abstract away the underlying state and produce a domain-specific model (and probably *should*), there would still need to be some sort of state attached. We've not got a map yet, so the only parts of our state are the player's current position and whether we should quit at the next moment.

```haskell

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

data WorldState = WorldState
  { playerPosition :: V2
  , pendingQuit :: Bool
  }
```

## A very brief aside on monad transformers and `mtl`

This is a very brief look into the problems with using specific combinations of monads and how `mtl` can be used to make it much easier. For a much deeper dive, check out the section on the [Haskell Wiki about Monad Transformers](https://wiki.haskell.org/index.php?title=All_About_Monads#Monad_transformers). Here, I'll instead briefly explain enough so you'll be fine using `mtl`-style constraints.

We want to put this `WorldState` into a `State` monad, so we can modify it as the game progresses (the player moves around). But, we also want to use `IO`. No worries, we can use monad transformers:

```haskell
type Game a = StateT WorldState IO a
```

This defines a type synonym for a monad called `Game` that consists of a `State` monad **on top of** (or containing, or wrapping) the `IO` monad. If we have a monadic computation of type `Game`, then we have both the behaviour of a `State` monad and also the behaviour of an `IO` monad. Unfortunately, this is slightly annoying: functions like `putStrLn :: String -> IO ()` are of type `IO`, not `StateT WorldState IO`. The compiler will complain that it cannot match these. This is where the concept of *lifting* comes in. `lift` is a member of the `MonadTrans` (rights) typeclass:

```haskell

class MonadTrans t where
    lift :: (Monad m) => m a -> t m a
```

That is, it takes some monadic function and allows it to be executed "inside" another thing. If we let `t` be our `StateT WorldState` and `m` be `IO`, we specialise lift to:

```haskell

lift :: Monad IO => IO a -> StateT WorldState IO a
-- or
lift :: Monad IO => IO a -> Game a
```
As to how this is done is different for every outer monad, but the intuition is that of unwrapping the outer monad to get to the inner one, performing some action, and then adding a no-op of the outer monad back on top. If you'd like a full breakdown, the wiki link above is worth a readthrough with some tea.

Back to our `Game` monad, we can now do operations of the `State` monad normally (`get` and `set`) and we can *also* do things of the `IO` monad with `lift` (such as `lift $ putStrLn "hello!"`). If we had multiple monads on top of each other, we'd have to start writing `lift $ lift $ lift f`! That's not ideal. Another downside is that we cannot add or remove monads. If we want to, for example, use a `ReaderT` transformer (for example, if we want to introduce a config setting that we can only read from and not write to) on top then all of our functions with type `Game a` now need to be lifted!

There's a more specific thing for us in particular: `roguefunctor` also has its own internal state. If you check the type of `withWindow`, you'll notice it does not take an `IO` action but instead something of the type `RogueT`. This is an additional layer of state that holds, among other things, the random number generator and some entity tracking information. What we *want* is to work with any monad `m` such that it supports `IO` actions and `State` actions. That way, we don't need to `lift` (we don't need to know if something is 10 layers deep or the top monad), we can have extra things in our monad stack that we simply don't need to bring into scope, and we can extend it without a problem. This is where `mtl` comes in. Instead of a `Game` monad **type**, we'll make a set of **monad constraints**:

```haskell
type GameMonad m = (MonadIO m, MonadState WorldState m)

-- which we can use like this

runLoop :: GameMonad m => m ()

-- which is just shorthand for this
runLoop :: (MonadIO m, MonadState WorldState m) => m ()
```

This says that in `m`, there is `IO` (somewhere) and there is `State WorldState` (somewhere). Most modern functions will be polymorphic - that is, they will be of the kind `MonadIO m => m a` rather than just `IO a`, but we can use `liftIO` if they aren't.

It feels like we've gone on a somewhat complicated journey to say that "`mtl` allows you to say that this monad `m` has some behaviour in it". That's it! `m` has `IO` abilities in it! `m` has `State WorldState` abilities in it! And now we can move on!

## A new main

Whilst monad constraints are great for using monads, they unfortunately do need you to interpret the monads still. We are currently only needing to add (and interpret) a `StateT` monad. As we don't care for the eventual return value or the eventual state of the `WorldState`, it doesn't matter which interpreter we use - `evalStateT`, `execStateT`, or `runStateT`:

```haskell
main :: IO ()
main =
  withWindow
    defaultWindowOptions { size = Just screenSize, title = Just "HsRogue - Part 1" }
    (return ()) -- no init logic
    -- equivalent to \() -> evalStateT runLoop (WorldState initialPlayerPosition False) >> return ()
    (const $ void $ evalStateT runLoop (WorldState initialPlayerPosition False))
    (return ()) -- no shutdown logic
```

As our initialisation function (`return ()`) doesn't provide anything useful, we ignore it with `const`.

To migrate our event loop to the new monad, we need to:
  - obtain the player's position from the state with `gets`.
  - draw a `@` at the position.
  - make a `pendQuit` function that updates the `pendingQuit` field of our world state.
  - now we don't need the return value of `handleEvents` we can use `handleEvents_` to discard it


```haskell
pendQuit :: GameMonad m => m ()
pendQuit = modify (\worldState -> worldState { pendingQuit = True})

runLoop :: GameMonad m => m ()
runLoop = do
  terminalClear
  playerPos <- gets playerPosition
  printText_ playerPos "@"
  terminalRefresh
  handleEvents_ Blocking $ \case
    TkClose -> pendQuit
    TkEscape -> pendQuit
    _ -> return ()
  shouldQuit <- gets pendingQuit
  unless shouldQuit runLoop
```

If you rebuild it now, you should see a white `@`! It doesn't do much yet, except quit. Let's add some keyboard handling.

# Moving around

For now, we aren't going to do some sort of `Action` hierarchy for different kinds of actions (this will be a bit later) or setting up game objects (we'll do that when we add a map in the next part).

We want to separate the actual keypresses from the directions we want to move, and both of those from the moving logic itself. We start with a type for directions (diagonal movement will come later, or it can be left as an exercise to the reader!):

```haskell
data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Eq, Ord, Show, Generic, Read, Enum, Bounded)
```

Most of the deriving classes are only there for the sake of being complete, though having `Enum` and `Bounded` instances will make it easier to generate random directions.

Next, we need a mapping from the keypresses to the directions. We can support both WASD and the arrow keys for this fairly easily.

```haskell
import qualified Data.Map as M

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  , (TkUp, UpDir)
  , (TkDown, DownDir)
  , (TkLeft, LeftDir)
  , (TkRight, RightDir)
  ]

asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys
```

Not every keypress is necessarily a movement action, which is why we return `Maybe Direction` - `Just direction` in the good case, and a `Nothing` otherwise. Eventually, we will a) expand this from a mapping of keys-directions to a mapping of keys-*actions* and b) envelop it within the game state to allow us to remap keys.

Given a direction, we want to turn it into a function that modifies a position (by moving 1 space in that direction):

```haskell

calculateNewLocation :: Direction -> V2 -> V2
calculateNewLocation dir (V2 x y) = case dir of
  LeftDir -> V2 (x-1) y
  RightDir -> V2 (x+1) y
  UpDir -> V2 x (y-1)
  DownDir -> V2 x (y+1)
```

Now we've got all the building blocks of our movement system:

- we can go from movement keypresses to directions (`movementKeys`) that we can extend to any keypress to potentially a direction (`asMovement`);
- from directions to a vector modifying function (`calculateNewLocation`);
- and by glueing these together we can go from any keypress to potentially an updated position.

This is what's great about functional programming: it strongly steers you into writing very compartmentalised and self-contained parts that you can combine. Note that all 3 of the components are completely pure! There's no mutable state hanging around where we need to be cautious about whatever entity the `calculateNewLocation` function is operating on. We don't need to change things around to implement a "try to move" function (where perhaps we move something onto a tile and then reverse it if the move was invalid).

The last step is to check if the key pressed in our event handling loop was one of these movement keys and to adjust the state accordingly:

```haskell
runLoop :: MonadIO m => Game m ()
runLoop = do
  terminalClear
  playerPos <- gets playerPosition
  printText_ playerPos "@"
  terminalRefresh
  handleEvents_ Blocking $ \case
    TkClose -> pendQuit
    TkEscape -> pendQuit
    other -> case asMovement other of
      Just dir -> modify (\worldState ->
        worldState
          { playerPosition = calculateNewLocation dir (playerPosition worldState)
          })
      Nothing -> return ()
  shouldQuit <- gets pendingQuit
  unless shouldQuit runLoop
```

Again, a large amount of options which could make this a little less noisy - we could use pattern guards to inline our case statement; we could use lenses or optics to better deal with state updates (producing something closer to as you'd expect with setting a mutable field in an imperative language), or we could move the close events into the "action" map (the current direction map). Whilst my personal preference is to use optics for doing state updates, they *do* introduce a whole new layer of complexity that I'd like to avoid until absolutely necessary. We definitely will be wrapping these annoyingly wordy `modify` lambdas up shortly though!

Now if we build and run the program, you should not only see the @ in the centre of the screen but you can move it! Of course it's possible to move it off the screen and then forget which side of the screen you exited and therefore have no idea where your character is *but* it works!

# Wrapping up

We have something resembling a game!

In the next part, we're going to make it minorly more interesting - adding a map and collision.

And what will likely become the commonplace wrapping up:

The code is available at the accompaying GitHub repo [here](https://github.com/PPKFS/roguelike-tutorial-parts/).

Hopefully this was understandable - any feedback is greatly appreciated. You can find me on the various functional programming/roguelike Discords as `ppkfs` and on Bluesky as `@ppkfs.bsky.social`.

Feedback is greatly appreciated! Feel free to reach out with any questions or suggestions.


Hope to see you for Part 2 soon!
