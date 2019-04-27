{-# LANGUAGE DeriveAnyClass, DerivingStrategies, LambdaCase, OverloadedStrings,
             PatternSynonyms, RecursiveDo, ScopedTypeVariables,
             ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Exception (Exception, handle, throwIO)
import Control.Monad (forever, void)
import Data.Foldable (for_)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Reactive.Banana
import Reactive.Banana.Frameworks
import SDL (Point(..), Rectangle(..), V2(..), V4(..), ($=))

import qualified SDL


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  renderer :: SDL.Renderer <-
    initializeSDL

  (tickAddHandler, tick) :: (AddHandler (), () -> IO ()) <-
    newAddHandler

  (thrustAddHandler, thrust) :: (AddHandler (), () -> IO ()) <-
    newAddHandler

  doneVar :: MVar () <-
    newEmptyMVar

  network :: EventNetwork <-
    compile $ do
      ticks :: Event () <-
        fromAddHandler tickAddHandler

      thrusts :: Event () <-
        fromAddHandler thrustAddHandler

      (scene, done) :: (Behavior Y, Event ()) <-
        helicopterGame ticks thrusts

      scenes :: Event (Future Y) <-
        changes scene

      reactimate (void . tryPutMVar doneVar <$> done)

      reactimate' ((fmap.fmap) (render renderer) scenes)

  actuate network

  handle (\Bye -> pure ()) . forever $ do
    tick ()

    events :: [SDL.Event] <-
      SDL.pollEvents

    for_ events $ \case
      Quit   -> throwIO Bye
      Thrust -> thrust ()
      _      -> pure ()

    tryReadMVar doneVar >>= \case
      Nothing ->
        threadDelay (1000000 `div` 60)

      Just () ->
        throwIO Bye

initializeSDL :: IO SDL.Renderer
initializeSDL = do
  SDL.initializeAll

  window :: SDL.Window <-
    SDL.createWindow "" SDL.defaultWindow

  SDL.createRenderer window (-1) SDL.defaultRenderer

pattern Thrust :: SDL.Event
pattern Thrust <-
  SDL.Event _ (SDL.KeyboardEvent (isNonRepeatKeycodeSpace -> True))

isNonRepeatKeycodeSpace :: SDL.KeyboardEventData -> Bool
isNonRepeatKeycodeSpace event =
  and
    [ SDL.keysymKeycode (SDL.keyboardEventKeysym event) == SDL.KeycodeSpace
    , not (SDL.keyboardEventRepeat event)
    ]

pattern Quit :: SDL.Event
pattern Quit <-
  SDL.Event _ (SDL.KeyboardEvent (isKeycodeEscape -> True))

isKeycodeEscape :: SDL.KeyboardEventData -> Bool
isKeycodeEscape event =
  SDL.keysymKeycode (SDL.keyboardEventKeysym event) == SDL.KeycodeEscape

data Bye
  = Bye
  deriving stock (Show)
  deriving anyclass (Exception)


--------------------------------------------------------------------------------
-- Game logic
--------------------------------------------------------------------------------

helicopterGame ::
     Event ()
  -> Event ()
  -> MomentIO (Behavior Y, Event ())
helicopterGame ticks thrusts = mdo
  position :: Behavior Double <-
    makePosition ticks velocity

  velocity :: Behavior Double <-
    makeVelocity ticks acceleration

  acceleration :: Behavior Double <-
    makeAcceleration thrusts

  let
    dead :: Event ()
    dead =
      makeDead ticks position

  pure (position, dead)

makeAcceleration ::
     MonadMoment m
  => Event ()
  -> m (Behavior Acceleration)
makeAcceleration thrusts =
  accumB
    (-gAcceleration)
    (negate <$ thrusts)

makeDead :: Event () -> Behavior Y -> Event ()
makeDead ticks position =
  whenE
    ((\p -> p < gFloorY || p > gCeilingY) <$> position)
    ticks

makePosition ::
     MonadMoment m
  => Event ()
  -> Behavior Velocity
  -> m (Behavior Y)
makePosition ticks velocity =
  accumB
    gInitialPosition
    ((+) <$> velocity <@ ticks)

makeVelocity ::
     MonadMoment m
  => Event ()
  -> Behavior Acceleration
  -> m (Behavior Velocity)
makeVelocity ticks acceleration =
  accumB
    gInitialVelocity
    ((+) <$> acceleration <@ ticks)


--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

render ::
     SDL.Renderer
  -> Y
  -> IO ()
render renderer helicopter = do
  renderBackground
  renderCeiling
  renderFloor
  renderHelicopter
  SDL.present renderer

  where
    renderBackground :: IO ()
    renderBackground = do
      color gBackgroundColor
      SDL.clear renderer

    renderCeiling :: IO ()
    renderCeiling = do
      color gCeilingColor
      rect 0 0 gWindowWidth (gWindowHeight - floor gCeilingY)

    renderFloor :: IO ()
    renderFloor = do
      color gCeilingColor
      rect 0 (gWindowHeight - floor gFloorY) gWindowWidth (floor gFloorY)

    renderHelicopter :: IO ()
    renderHelicopter = do
      color gHelicopterColor
      rect (round (gHelicopterX-5)) (gWindowHeight - round (helicopter-5)) 10 10

    color :: V4 Word8 -> IO ()
    color =
      (SDL.rendererDrawColor renderer $=)

    rect :: CInt -> CInt -> CInt -> CInt -> IO ()
    rect x y w h =
      SDL.fillRect renderer (Just (Rectangle (P (V2 x y)) (V2 w h)))


--------------------------------------------------------------------------------
-- Types and type aliases
--------------------------------------------------------------------------------

type Acceleration
  = Double

type Score
  = Double

type Velocity
  = Double

-- | In game coordinates, where (0, 0) is at the bottom left corner.
type X
  = Double

-- | In game coordinates, where (0, 0) is at the bottom left corner.
type Y
  = Double


--------------------------------------------------------------------------------
-- Globals/settings
--------------------------------------------------------------------------------

gAcceleration :: Acceleration
gAcceleration =
  0.25

gBackgroundColor :: V4 Word8
gBackgroundColor =
  V4 0 0 0 0

gCeilingColor :: V4 Word8
gCeilingColor =
  V4 0 255 0 0

gCeilingY :: Y
gCeilingY =
  500

gFloorY :: Y
gFloorY =
  100

gHelicopterColor :: V4 Word8
gHelicopterColor =
  V4 255 0 0 0

gHelicopterX :: X
gHelicopterX =
  100

gInitialPosition :: Y
gInitialPosition =
  300

gInitialVelocity :: Velocity
gInitialVelocity =
  0

gWindowHeight :: CInt
gWindowHeight =
  600

gWindowWidth :: CInt
gWindowWidth =
  800
