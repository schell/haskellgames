---
title: Snakes on a Monad
articleTitle: Snakes on a Monad &#x2708;&#xFE0F;️
toc-title: Table of Contents
author: Schell Scivally
authorLink: https://github.com/schell
repo: https://github.com/schell/haskellgames/tree/master/snake
comments: https://github.com/schell/haskellgames/labels/snake
---

Welcome to my version of snake. Snake was originally implemented by the San
Diego, CA studio Gremlin Industries in 1976 under the name "Blockade". Gremlin
later went on to write [Zaxxon](https://en.wikipedia.org/wiki/Zaxxon), which is
one of my favorites. Hopefully one day I'll be able to write a Zaxxon, but for now
- we snake.

This post is a literate haskell file so we'll start with a wall of my favorite
language extensions.

> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE FunctionalDependencies     #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE LambdaCase                 #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE ScopedTypeVariables        #-}
> {-# LANGUAGE TemplateHaskell            #-}
> {-# LANGUAGE TupleSections              #-}

Next we'll get to our imports. Here's the least interesting stuff:

> import           Control.Applicative    ((<|>))
> import           Control.Monad          (unless, void, when, forM_)
> import           Control.Monad.IO.Class (MonadIO (..))
> import           Control.Monad.State    (MonadState (..), evalStateT)
> import           Data.Foldable          (elem)
> import           Prelude                hiding (elem)
> import           Text.Show.Pretty       (pPrint)

We're going to use lenses to mutate our game state.

> import           Control.Lens           (makeFields, use, (%=), (.=))

`use` allows us to _use_ getters to read monadic state.
`.=` takes a setting lens on the left and a value on the right and updates the
value in the monadic state.
`%=` takes a setting lens on the left and an update function and runs the
function on the current value read by the lens, setting the state to its
result value.

```haskell
use :: MonadState s m => Control.Lens.Getter.Getting a s a -> m a

(.=)
  :: MonadState s m =>
     Control.Lens.Setter.ASetter s s a b -> b -> m ()

(%=)
  :: MonadState s m =>
     Control.Lens.Setter.ASetter s s a b -> (a -> b) -> m ()
```

By the way, when you see code styled like the above, it means it is not part of
the running program, it's commented out and here simply for reference.

Next we'll import my favorite part of this game. It's the structure we use as the
body of our snake, `NonEmpty`! It represents a list that *must* always have at
least one item.

> import           Data.List.NonEmpty     (NonEmpty (..))

```haskell
data NonEmpty a = a :| [a]      -- Defined in ‘GHC.Base’
```

Next we'll need some way of keeping track of time.

> import           Data.Time.Clock        (NominalDiffTime, UTCTime, diffUTCTime,
>                                          getCurrentTime)

We'll need a way to randomly generate new positions to place food for the snake.

> import           System.Random          (randomRIO)

Lastly we'll import our SDL stuff. `V2`, `V4` and a number of other functions
we'll use later originally come from the 'linear' package and are re-exported by
'sdl2', which provides our SDL bindings.

> import           SDL                    (Point(..), V2 (..), V4 (..), ($=),
>                                          Renderer, EventPayload,
>                                          Window)
> import qualified SDL
> import           SDL.Input.Keyboard.Codes

The World
=========
Next we're going to spend some time defining the world and the things in it.

As a matter of convention I always define things before using them, that way when
I'm looking for the definition of something I can simply search up the page.

Here we'll make a type that represents all the directions of movement our snake
can slither.

> -- | Cardinal directions.
> data Direction
>   = North
>   | East
>   | South
>   | West
>   deriving (Show, Eq, Ord)

That's the only bit of custom world building we're going to do! Now we define the
world itself.

Note below the `makeFields` template haskell function. It's going to
automagically create lenses for each field in the record, along with a typeclass
of (roughly) the form:

```haskell
class Has{Suffix} s a where
  {suffix} = ... -- a lens
```

We'll use these type classes later for MTL style getter and setter constraints,
so our computations' effects are loud and clear.

> -- | Everything in the world! Oh, the simple life of a snake.
> data World = World
>   { _worldSnake    :: NonEmpty (V2 Int)
>   -- ^ The snake's body
>   , _worldFood     :: V2 Int
>   -- ^ The bit of food the snake is looking for
>   , _worldJoystick :: V2 Double
>   -- ^ The joystick position
>   , _worldLastDir  :: Maybe Direction
>   -- ^ The direction the snake is headed in.
>   } deriving (Show, Eq)
> makeFields ''World

In the definition above the snakes direction is a `Maybe Direction` because when
the snake first starts it is not moving.

Now let's build a `World` value. This will be the world the player sees when they
first start up the game.

> initialWorld :: World
> initialWorld =
>   World
>   { _worldSnake    = 0 :| []
>   , _worldFood     = 100
>   , _worldJoystick = 0
>   , _worldLastDir  = Nothing
>   }

Our map will be 64x48 points. There are no items or NPCs, so we don't ever build
a "map" in the classic game sense of the word, but we'll use this for positioning
the snake and its food.

> mapSize :: V2 Int
> mapSize = V2 64 48

The Game - our main state
=========================
`Game` represents our game state. I keep this separate from the world because to me
the world represents everything the player can influence, as well as everything
that needs to get rendered. The game state needs to know this like what time it
is, if the game is paused and have references to the renderer and the window.

> -- | All the things to run our game.
> data Game = Game
>   { _gameWindow    :: Window
>   -- ^ The SDL created window
>   , _gameRenderer  :: Renderer
>   -- ^ SDL's built-in 2d renderer
>   , _gameWorld     :: World
>   -- ^ The current world
>   , _gameDeltaTime :: NominalDiffTime
>   -- ^ How much time has passed this frame
>   , _gameLastUTC   :: UTCTime
>   -- ^ The timestamp of the last frame
>   , _gameIsPaused  :: Bool
>   -- ^ Whether or not the game is paused
>   } deriving (Show, Eq)
> makeFields ''Game

You can see above we also create lenses and field classes for this record.

Rendering
=========
Snake has got to have the easiest rendering job. Even pong is harder.

Below we'll need a couple functions to help with rendering.

> getWindowSize :: (MonadIO m, Num a) => Window -> m (V2 a)
> getWindowSize = (fmap fromIntegral <$>) . SDL.get . SDL.windowSize
>
>
> getTileSize :: (MonadIO m, Fractional a) => Window -> m (V2 a)
> getTileSize = fmap (/ fmap fromIntegral mapSize) . getWindowSize

We're really getting into the meat of our game here, and this is where `NonEmpty`
starts to shine. The snake should *always* have a head and here we can be
guaranteed that fact.

> renderSnakeAndFood
>   :: MonadIO m
>   => Window
>   -> Renderer
>   -> NonEmpty (V2 Int)
>   -> V2 Int
>   -> m ()
> renderSnakeAndFood win r (snakeHead :| body) fud = do
>   tileSize :: V2 Double <- getTileSize win
>   let tilePos    = (tileSize *) . (fromIntegral <$>)
>       tileRect p = floor <$> SDL.Rectangle (P $ tilePos p) tileSize

Colors are represented by a 4 channel, 8 bit vector of the form
*RGBA* where `V4 0 0 0 255` is solid black and `V4 255 255 255 255` is
solid white. Here we define the color of the snake and the color of food:

>       -- green
>       snakeColor = V4 0 255 0 255
>       -- yellow
>       foodColor  = V4 255 255 0 255

`$=` is what SDL uses as its monadic setter operator. To draw the food we set the
renderer's draw color and then draw a filled rectangle.

>   SDL.rendererDrawColor r $= foodColor
>   SDL.fillRect r $ Just $ tileRect fud

Next we draw the snake. We start with its head and then we map a draw function
over the rest of its body.

>   SDL.rendererDrawColor r $= snakeColor
>   SDL.fillRect r $ Just $ tileRect snakeHead
>   mapM_ (SDL.drawRect r . Just . tileRect) body

That's all for the snake and its food.
Here's a function for clearing the screen that we'll run each frame.

> renderClear
>   :: MonadIO m
>   => Renderer
>   -> m ()
> renderClear r = do
>   SDL.clear r
>   SDL.rendererDrawColor r $= V4 0 0 0 255
>   SDL.fillRect r Nothing

`renderFrame` will render a nice border around our game. The interesting bit here
is that we can define `V2`'s using number literals and add `V2`'s together using
the usual infix operators. This is due to
[`V2`'s Num instance](https://hackage.haskell.org/package/linear-1.20.8/docs/Linear-V2.html#t:V2)
You can see this with the expression `(wsz - 20)`.

> renderFrame
>   :: MonadIO m
>   => Renderer
>   -> Window
>   -> m ()
> renderFrame r w = do
>   wsz <- getWindowSize w
>   SDL.rendererDrawColor r $= V4 255 255 255 255
>   SDL.drawRect r
>     $ Just
>     $ SDL.Rectangle (P 10) (wsz - 20)


The top-level rendering function
--------------------------------
Now we wrap it all up in our one render function to rule them all. You can see
where our field typeclasses come into play. This `render` function can
only be run in a computation that has a `Renderer` accessible using the lens
`renderer`, as well as a `Window` using `window` and a `World` using `world`.
Similarly within this computation we can't do anything else but read and write
values to or from these explicitly annotated fields.

We `use` our lenses to read our game's current state. In cases where we have to
drill down into deeper records we can compose those lenses using good old
function composition `.`.

> render
>   :: ( MonadIO m
>      , MonadState s m
>      , HasRenderer s Renderer
>      , HasWindow s Window
>      , HasWorld s World
>      )
>   => m ()
> render = do
>   r <- use renderer
>   w <- use window
>   s <- use (world . snake)
>   f <- use (world . food)
>
>   renderClear r
>   renderSnakeAndFood w r s f
>   use window >>= renderFrame r
>
>   SDL.present r

As you can see, working with SDL's built-in renderer is a breeze. It's a lot like
working with an HTML5 canvas. It's all very imperative. In fact, this whole
program is very imperative, but you can see Haskell handles really well through it.

Time Step
=========
Next we build our tick function. This will determine how much time has passed
this frame, accounting for the game being paused. This will update some state and
return a `Bool` depending on whether or not we want to update the screen. I want
to give this game a real retro feel, so I quantize the time steps and we only
return `True` if more than a tenth of a second has passed.

> tickTime
>   :: ( MonadIO m
>      , MonadState s m
>      , HasDeltaTime s NominalDiffTime
>      , HasLastUTC s UTCTime
>      , HasIsPaused s Bool
>      )
>   => m Bool
> tickTime = do
>   now    <- liftIO getCurrentTime
>   prev   <- use lastUTC
>   delta  <- use deltaTime
>   paused <- use isPaused
>
>   let maxDelta   = 0.1
>       dt         = delta + if paused then 0 else diffUTCTime now prev
>       shouldStep = dt > maxDelta && not paused
>       nextDelta  = dt - if shouldStep then maxDelta else 0
>
>   lastUTC   .= now
>   deltaTime .= nextDelta
>   return shouldStep


Player Input
============
For player input we're going to support controllers with analog sticks, but our
game thinks in terms of cardinal directions so we'll need a function that maps
an 2 dimensional analog signal into cardinal directions. It's quite possible that
the analog signal isn't able to be mapped, so we're returning a `Maybe Direction`.

> axisValueToDirection
>   :: (Ord a, Floating a)
>   => V2 a
>   -> Maybe Direction
> axisValueToDirection v@(V2 x y)
>   -- If the magnitude of the signal is small, nothing happens. This is often
>   -- called a controller "dead zone".
>   | SDL.norm v < 0.3 = Nothing
>   -- If the player is pushing in a diagonal, nothing happens. This doesn't
>   -- cover all the diagonal cases, but it's fine I think.
>   | x == y = Nothing
>   -- Otherwise figure out the cardinal direction
>   | abs x > abs y =
>     Just
>       $ if x > 0
>         then East
>         else West
>   | y > 0     = Just South
>   | otherwise = Just North

Merge the direction from the current frame with that of the last frame. We do
this as a service to the player - we don't want them to die by turning their snake
around on itself, so if the current direction is the opposite of the old direction
we keep the old direction.

Here you can see it's looking pretty functional with all the `Applicative` style.

> mergeDirections
>   :: Maybe Direction
>   -> Maybe Direction
>   -> Maybe Direction
> mergeDirections mnew mold =
>  (firstIfNotOpposite <$> mnew <*> mold) <|> mnew <|> mold
>   where
>     firstIfNotOpposite North South = South
>     firstIfNotOpposite South North = North
>     firstIfNotOpposite West  East  = East
>     firstIfNotOpposite East  West  = West
>     firstIfNotOpposite a     _     = a

SDL Event Handling
------------------
These are the potatoes of our event loop. You can see all the possible SDL events
[here](https://hackage.haskell.org/package/sdl2-2.4.1.0/docs/SDL-Event.html#t:EventPayload).

> handleEvent
>   :: ( MonadIO m
>      , MonadState s m
>      , HasWorld s World
>      , HasIsPaused s Bool
>      )
>   => EventPayload
>   -> m ()

The first task is to detect when a joystick has been added. This gets run at the
very beginning if the game starts up with a controller already plugged in.

> handleEvent (SDL.JoyDeviceEvent (SDL.JoyDeviceEventData SDL.JoyDeviceAdded _)) =
>   SDL.availableJoysticks >>= mapM_ SDL.openJoystick

When we get some joystick axis action we determine what to do with it. Pattern
matching is a beautiful feature here. When used with the right binding names and
monadic comps, it's almost like reading English.

> handleEvent (SDL.JoyAxisEvent (SDL.JoyAxisEventData _ axis value)) = do
>   paused <- use isPaused
>   unless paused $ do
>     V2 x y <- use (world . joystick)
>     let denom = case signum value of
>                   1 -> 32767
>                   _ -> 32768
>         scl = (fromIntegral value / denom)
>         jxy = case axis of
>           0 -> V2 scl y
>           1 -> V2 x scl
>           _ -> V2 x y
>     world . joystick .= jxy
>     world . lastDir %= mergeDirections (axisValueToDirection jxy)

Here we are toggling the pause button on and off. `7` is the number of the button
I want to use as "pause". I'm fairly certain that button is the same on many
controllers but that's just an assumption - YMMV.

> handleEvent (SDL.JoyButtonEvent (SDL.JoyButtonEventData _ 7 SDL.JoyButtonPressed)) =
>   isPaused %= not

Lastly any other events that come down the pipe will get printed.

> handleEvent ev = liftIO $ print ev

In order to exit the program we need to know when a quit(ish) event comes
down the event pipe. Gaurds to the rescue!

> shouldQuit :: EventPayload -> Bool
> shouldQuit ev
>   -- if the player clicks the "close window" button
>   | SDL.QuitEvent <- ev = True
>   -- if the player hits control-q
>   | (SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ ksym)) <- ev
>   , SDL.Keysym _ KeycodeQ kmod <- ksym =
>       SDL.keyModifierLeftCtrl kmod
>       || SDL.keyModifierRightCtrl kmod
>   | otherwise = False


Game logic
==========
For many of us this is the most fun part. Now that our game can display stuff to
the player and we can get input from the player, we get to write the rules for
the world and see it all play out in real time. This is where experimentation is
profitable and exciting.

First we need to generate food. We'll use `randomRIO` to place the food in a
random place on the map.

> getRandomPosition
>   :: MonadIO m
>   => m (V2 Int)
> getRandomPosition = do
>   let V2 w h = mapSize
>   liftIO $
>     V2 <$> randomRIO (0, w)
>        <*> randomRIO (0, h)
>
>
> freshFood
>   :: ( MonadIO m
>      , MonadState s m
>      , HasWorld s World
>      )
>   => m ()
> freshFood = do
>   pos <- getRandomPosition
>   world . food .= pos

We'll use this same technique to place the snake on the map.

> freshSnake
>   :: ( MonadIO m
>      , MonadState s m
>      , HasWorld s World
>      )
>   => m ()
> freshSnake = do
>   pos <- getRandomPosition
>   world . snake .= pos :| []

When the snake goes off the map we want to wrap it around to the other side.

> wrap :: (Ord a, Num a) => a -> a -> a
> wrap l n | n < 0     = l - 1
>          | n >= l    = 0
>          | otherwise = n

Move the snake through the world according to its direction, wrapping it as needed.

> move
>   :: V2 Int
>   -- ^ The entity's position.
>   -> Direction
>   -- ^ The entity's direction.
>   -> V2 Int
> move (V2 x y) = \case
>   North -> V2 x $ wrap h $ pred y
>   South -> V2 x $ wrap h $ succ y
>   East  -> V2 (wrap w $ succ x) y
>   West  -> V2 (wrap w $ pred x) y
>   where V2 w h = mapSize

Here's our big world step. This gets the food, the snake, the direction and puts
it all together.

> stepWorld
>   :: ( MonadState s m
>      , MonadIO m
>      , HasWorld s World
>      )
>   => m ()
> stepWorld = do
>   -- print the world each step
>   use world >>= liftIO . pPrint
>   -- get the current food spot and the current snake
>   f0 <- use (world . food)
>   currentSnake@(s0 :| ss0) <- use (world . snake)

`use (world . lastDir)` returns a `Maybe Direction` and when there is a `Just d`,
`forM_` will run our update computation. Otherwise nothing happens.

>   -- do stuff based on the last direction, if there is one.
>   mayDir <- use (world . lastDir)
>   forM_ mayDir $ \d -> do
>     let nextMove = move s0 d
>     case () of

I liked writing this next bit, especially the `world . snake .= f0 :| s0 : ss0`.
Since the food and the snake segments are both just locations, we can extend the
snake simply by consing the current snake head to its tail and consing the food
as its new head!

>          -- the next move eats the food, make new food, make more snake!
>       () | nextMove == f0 -> do
>              world . snake .= f0 :| s0 : ss0
>              freshFood
>          -- the next move kills the snake, make new snake, make new food
>          | nextMove `elem` currentSnake -> do
>              freshSnake
>              freshFood

When moving the snake we can do a similar trick by consing the snakes head onto
its tail and taking only as many segments as the snake has from the resulting tail.
After that we cons the new position onto its tail as the new snake head. It gives
the perception that the snake is moving, but it's not, it's phasing through space.
Ooooh!

>          -- the next move simply moves the snake, move dat snake!
>          | otherwise -> do
>              let newTail = take (length ss0) $ s0 : ss0
>              world . snake .= nextMove :| newTail

We're sooooo close!
Get all of sdl's events and run our updates on the world. Tick the time and step
the world if it's a good time to do so (according to `tickTime`). Render all the
things! Check to see if we should quit. Are you kidding?! We never quit! Snake
till death! ⚰️

Again this is one of those times where Haskell reads like English. So smooze.

> mainLoop
>   :: ( MonadState Game m
>      , MonadIO m
>      )
>   => m ()
> mainLoop = do
>   evs <-
>     fmap SDL.eventPayload
>       <$> SDL.pollEvents
>   mapM_ handleEvent evs
>
>   shouldStep <- tickTime
>   when shouldStep stepWorld
>   render
>
>   unless (any shouldQuit evs) mainLoop

Okay, this part is ugly. Fear not though, it's really just some SDL stuff. For
our purposes today you can't really go wrong here. If you want more insight into
setting up the renderer check out
[the docs](https://hackage.haskell.org/package/sdl2-2.4.1.0/docs/SDL-Video-Renderer.html),
they're good.

> newGame :: IO Game
> newGame = do
>   -- initialize all the SDL subsystems. ALL THE THINGS!
>   SDL.initializeAll
>   let wcfg = SDL.defaultWindow
>         { SDL.windowInitialSize = V2 640 480 }
>       rcfg = SDL.defaultRenderer
>         { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
>   w <- SDL.createWindow "Snake" wcfg
>   r <- SDL.createRenderer w (-1) rcfg
>   t <- liftIO getCurrentTime
>   return Game
>     { _gameWorld    = initialWorld
>     , _gameWindow   = w
>     , _gameRenderer = r
>     , _gameLastUTC  = t
>     , _gameDeltaTime= 0
>     , _gameIsPaused = False
>     }

Run the game!
=============
Our main function below is very minimal - instantiate a new `Game` and pipe it
into a `StateT` runner. The state function itself is rather elegant - make a
fresh snake to eat some fresh food, tick the first time step and away we
go!

> start :: (MonadIO m, MonadState Game m) => m ()
> start = do
>   freshSnake
>   freshFood
>   void tickTime
>   mainLoop
>
>
> main :: IO ()
> main =
>   newGame
>     >>= evalStateT start

Conclusion
==========
Thanks for reading this far! Hopefully you've enjoyed seeing how easy it is to write
games in Haskell, or maybe you learned something about MTL style constraints,
`NonEmpty` or working with the SDL renderer. Feel free to write anything at all
(comments, requests, bugs, thanks, anything) in the github issues section for this
[snake implementation](https://github.com/schell/haskell-games-snake/issues). I
will also accept PRs, of course. Happy hacking! &#x1F40D; &#x1F913;
