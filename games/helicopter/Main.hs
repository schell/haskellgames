{-# LANGUAGE DeriveAnyClass, DerivingStrategies, LambdaCase, OverloadedStrings,
             PatternSynonyms, RecursiveDo, ScopedTypeVariables,
             ViewPatterns #-}

module Main where

import Control.Concurrent
import Control.Exception (Exception, handle, throwIO)
import Control.Monad (forever, void)
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Sequence (Seq, (|>))
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Reactive.Banana
import Reactive.Banana.Frameworks
import SDL (Point(..), Rectangle(..), V2(..), V4(..), ($=))
import System.Random

import qualified Data.Sequence as Seq
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

      (scene, done) :: (Behavior Scene, Event ()) <-
        helicopterGame ticks thrusts

      scenes :: Event (Future Scene) <-
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
  -> MomentIO (Behavior Scene, Event ())
helicopterGame ticks thrusts = mdo
  position :: Behavior Y <-
    makePosition ticks velocity

  velocity :: Behavior VelY <-
    makeVelocity ticks acceleration

  acceleration :: Behavior AccY <-
    makeAcceleration thrusts

  obstacles :: Behavior (Seq (X, Y)) <-
    makeObstacles ticks

  let
    dead :: Event ()
    dead =
      makeDead ticks position obstacles

  let
    scene :: Behavior Scene
    scene =
      (,) <$> position <*> obstacles

  pure (scene, dead)

makeAcceleration ::
     MonadMoment m
  => Event ()
  -> m (Behavior AccY)
makeAcceleration thrusts =
  accumB
    (-gAcceleration)
    (negate <$ thrusts)

makeDead ::
     Event ()
  -> Behavior Y
  -> Behavior (Seq (X, Y))
  -> Event ()
makeDead ticks position obstacles =
  whenE (isDead <$> position <*> obstacles) ticks

  where
    isDead :: Y -> Seq (X, Y) -> Bool
    isDead hy obs =
      or
        [ hy < gFloorY + 5
        , hy > gCeilingY - 5
        , obs
            & Seq.dropWhileL (\(x, _) -> x < gHelicopterX - 6)
            & Seq.takeWhileL (\(x, _) -> x <= gHelicopterX + 6)
            & any (\(_, y) -> y >= hy - 6 && y <= hy + 6)
        ]

makeObstacles ::
     Event ()
  -> MomentIO (Behavior (Seq (X, Y)))
makeObstacles ticks = mdo
  elapseds :: Event Int <-
    accumE 0 ((+1) <$ ticks)

  (ys, _) <-
    mapAccum
      (mkStdGen 0)
      (randomR (gFloorY + 5, gCeilingY - 5) <$
        filterE (\n -> n `rem` gObstacleSpawnRate == 0) elapseds)

  accumB
    Seq.empty
    (unions
      [ stepObstacles <$ ticks
      , (\y acc -> acc |> (gObstacleX, y)) <$> ys
      ])

  where
    stepObstacles :: Seq (X, Y) -> Seq (X, Y)
    stepObstacles =
      fmap (\(x, y) -> (x + gObstacleVelocity, y)) .
      Seq.dropWhileL (\(x, _) -> x < 0)

makePosition ::
     MonadMoment m
  => Event ()
  -> Behavior VelY
  -> m (Behavior Y)
makePosition ticks velocity =
  accumB
    gHelicopterPosition
    ((+) <$> velocity <@ ticks)

makeVelocity ::
     MonadMoment m
  => Event ()
  -> Behavior AccY
  -> m (Behavior VelY)
makeVelocity ticks acceleration =
  accumB
    gHelicopterVelocity
    ((+) <$> acceleration <@ ticks)


--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

render ::
     SDL.Renderer
  -> Scene
  -> IO ()
render renderer (helicopter, obstacles) = do
  renderBackground
  renderCeiling
  renderFloor
  renderHelicopter
  renderObstacles
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
      rect (round gHelicopterX - 5) (gWindowHeight - round helicopter - 5) 10 10

    renderObstacles :: IO ()
    renderObstacles = do
      color gObstacleColor
      for_ obstacles $ \(x, y) ->
        rect (round x - 5) (gWindowHeight - round y - 5) 10 10

    color :: V4 Word8 -> IO ()
    color =
      (SDL.rendererDrawColor renderer $=)

    rect :: CInt -> CInt -> CInt -> CInt -> IO ()
    rect x y w h =
      SDL.fillRect renderer (Just (Rectangle (P (V2 x y)) (V2 w h)))


--------------------------------------------------------------------------------
-- Types and type aliases
--------------------------------------------------------------------------------

type AccY
  = Double

-- | Helicopter, obstacles.
type Scene
  = (Y, Seq (X, Y))

type Score
  = Double

type VelX
  = Double

type VelY
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

gAcceleration :: AccY
gAcceleration =
  0.25

gBackgroundColor :: V4 Word8
gBackgroundColor =
  V4 0 0 0 0

gCeilingColor :: V4 Word8
gCeilingColor =
  V4 0 220 0 0

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

gHelicopterPosition :: Y
gHelicopterPosition =
  300

-- | Initial helicopter velocity.
gHelicopterVelocity :: VelY
gHelicopterVelocity =
  0

gObstacleColor :: V4 Word8
gObstacleColor =
  V4 180 180 180 0

-- | The number of frames between obstacles spawning.
gObstacleSpawnRate :: Int
gObstacleSpawnRate =
  4

-- | Obstacle velocity.
gObstacleVelocity :: VelX
gObstacleVelocity =
  -4

-- | X coordinate where obstacles spawn.
gObstacleX :: X
gObstacleX =
  800

gWindowHeight :: CInt
gWindowHeight =
  600

gWindowWidth :: CInt
gWindowWidth =
  800
