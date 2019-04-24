# Prerequisites
Welcome to the haskellgames version of Snake. This post is a literate haskell


\begin{code}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

import           Control.Applicative    ((<|>))
import           Control.Lens           (makeFields, use, (%=), (.=))
import           Control.Monad          (unless, void, when)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.State    (MonadState (..), evalStateT)
import           Data.Int               (Int16)
import           Data.List.NonEmpty     (NonEmpty (..))

import           Data.Foldable          (elem)
import           Data.Time.Clock        (NominalDiffTime, UTCTime, diffUTCTime,
                                         getCurrentTime)
import           Linear
import           Prelude                hiding (elem)
import           SDL
import           System.Random          (randomRIO)
import           Text.Show.Pretty       (pPrint)


-- | Cardinal directions.
data Direction =
    North
  | East
  | South
  | West
  deriving (Show, Eq, Ord)


-- | Everything in the world!
data World = World
  { _worldSnake    :: NonEmpty (V2 Int)
  , _worldFood     :: V2 Int
  , _worldJoystick :: V2 Double
  , _worldLastDir  :: Maybe Direction
  } deriving (Show, Eq)
makeFields ''World


-- | A world void of meaning or color.
initialWorld :: World
initialWorld = World
  { _worldSnake    = 0 :| []
  , _worldFood     = 100
  , _worldJoystick = 0
  , _worldLastDir  = Nothing
  }


mapSize :: V2 Int
mapSize = V2 64 48


-- | All the things to run our game.
data Game = Game
  { _gameWindow    :: Window
  , _gameRenderer  :: Renderer
  , _gameWorld     :: World
  , _gameDeltaTime :: NominalDiffTime
  , _gameLastUTC   :: UTCTime
  , _gameIsPaused  :: Bool
  } deriving (Show, Eq)
makeFields ''Game


getWindowSize :: (MonadIO m, Num a) => Window -> m (V2 a)
getWindowSize = (fmap fromIntegral <$>) . SDL.get . windowSize


getTileSize :: (MonadIO m, Fractional a) => Window -> m (V2 a)
getTileSize = fmap (/ fmap fromIntegral mapSize) . getWindowSize


renderSnakeAndFood
  :: MonadIO m
  => Window
  -> Renderer
  -> NonEmpty (V2 Int)
  -> V2 Int
  -> m ()
renderSnakeAndFood win r (snakeHead :| body) fud = do
  tileSize :: V2 Double <- getTileSize win
  let tilePos    = (tileSize *) . (fromIntegral <$>)
      tileRect p = floor <$> Rectangle (P $ tilePos p) tileSize
      snakeColor = V4 0 255 0 255
      foodColor  = V4 255 255 0 255

  rendererDrawColor r $= foodColor
  fillRect r $ Just $ tileRect fud

  rendererDrawColor r $= snakeColor
  fillRect r $ Just $ tileRect snakeHead
  mapM_ (drawRect r . Just . tileRect) body


render
  :: ( MonadIO m
     , MonadState s m
     , HasRenderer s Renderer
     , HasWindow s Window
     , HasWorld s World
     )
  => m ()
render = do
  r <- use renderer
  w <- use window
  s <- use (world . snake)
  f <- use (world . food)

  clear r
  rendererDrawColor r $= V4 0 0 0 255
  fillRect r Nothing

  renderSnakeAndFood w r s f

  wh <- getWindowSize =<< use window
  rendererDrawColor r $= V4 255 255 255 255
  drawRect r
    $ Just
    $ Rectangle (P 10) (wh - 20)
  present r


getRandomPosition
  :: MonadIO m
  => m (V2 Int)
getRandomPosition = do
  let V2 w h = mapSize
  liftIO $
    V2 <$> randomRIO (0, w)
       <*> randomRIO (0, h)


freshFood
  :: ( MonadIO m
     , MonadState s m
     , HasWorld s World
     )
  => m ()
freshFood = do
  pos <- getRandomPosition
  world . food .= pos


freshPlayer
  :: ( MonadIO m
     , MonadState s m
     , HasWorld s World
     )
  => m ()
freshPlayer = do
  pos <- getRandomPosition
  world . snake .= pos :| []


tickTime
  :: ( MonadIO m
     , MonadState s m
     , HasDeltaTime s NominalDiffTime
     , HasLastUTC s UTCTime
     , HasIsPaused s Bool
     )
  => m Bool
tickTime = do
  prev   <- use lastUTC
  now    <- liftIO getCurrentTime
  delta  <- use deltaTime
  paused <- use isPaused

  let maxDelta   = 0.1
      dt         = delta + if paused then 0 else diffUTCTime now prev
      shouldStep = dt > maxDelta && not paused
      nextDelta  = dt - if shouldStep then maxDelta else 0

  lastUTC   .= now
  deltaTime .= nextDelta
  return shouldStep


setup :: (MonadIO m, MonadState Game m) => m ()
setup = do
  freshPlayer
  freshFood
  void tickTime


axisValueToDirection
  :: (Ord a, Floating a)
  => V2 a
  -> Maybe Direction
axisValueToDirection v@(V2 x y)
  | norm v < 0.3 = Nothing
  | x == y = Nothing
  | abs x > abs y = if x > 0
                    then Just East
                    else Just West
  | y > 0     = Just South
  | otherwise = Just North


mergeDirections
  :: Maybe Direction
  -> Maybe Direction
  -> Maybe Direction
mergeDirections ma mb = (firstIfNotOpposite <$> ma <*> mb) <|> ma <|> mb
  where firstIfNotOpposite North South = South
        firstIfNotOpposite South North = North
        firstIfNotOpposite West  East  = East
        firstIfNotOpposite East  West  = West
        firstIfNotOpposite a     _     = a


scaleInt16 :: Int16 -> Double
scaleInt16 i = fromIntegral i / denom
  where denom = case signum i of
                  1 -> 32767
                  _ -> 32768


-- | Handle any sdl2 events.
-- You can see all possible events
-- [here](https://hackage.haskell.org/package/sdl2-2.4.1.0/docs/SDL-Event.html#t:EventPayload).
handleEvent
  :: ( MonadIO m
     , MonadState s m
     , HasWorld s World
     , HasIsPaused s Bool
     )
  => EventPayload
  -> m ()
handleEvent (JoyDeviceEvent (JoyDeviceEventData JoyDeviceAdded _)) =
  availableJoysticks >>= mapM_ openJoystick
handleEvent (JoyAxisEvent (JoyAxisEventData _ axis value)) = do
  paused <- use isPaused
  unless paused $ do
    V2 x y <- use (world . joystick)
    let scl = scaleInt16 value
        jxy = case axis of
          0 -> V2 scl y
          1 -> V2 x scl
          _ -> V2 x y
    world . joystick .= jxy
    world . lastDir %= mergeDirections (axisValueToDirection jxy)
handleEvent (JoyButtonEvent (JoyButtonEventData _ 7 JoyButtonPressed)) =
  isPaused %= not
handleEvent ev = liftIO $ print ev


-- | In order to exit the program we need to know when a quit(ish) event comes
-- down the event pipe.
shouldQuit :: EventPayload -> Bool
shouldQuit ev
  | QuitEvent <- ev = True

  | (KeyboardEvent (KeyboardEventData _ Pressed _ ksym)) <- ev
  , Keysym _ KeycodeQ kmod <- ksym =
      keyModifierLeftCtrl kmod
      || keyModifierRightCtrl kmod
  | otherwise = False


wrap :: (Ord a, Num a) => a -> a -> a
wrap l n | n < 0     = l - 1
         | n >= l    = 0
         | otherwise = n


-- Move and entity through the world.
move
  :: V2 Int
  -- ^ The entity's position.
  -> Direction
  -- ^ The entity's direction.
  -> V2 Int
move (V2 x y) = \case
  North -> V2 x $ wrap h $ pred y
  South -> V2 x $ wrap h $ succ y
  East  -> V2 (wrap w $ succ x) y
  West  -> V2 (wrap w $ pred x) y
  where V2 w h = mapSize


stepWorld
  :: ( MonadState s m
     , MonadIO m
     , HasWorld s World
     )
  => m ()
stepWorld = do
  use world >>= liftIO . pPrint
  f0 <- use (world . food)
  currentSnake@(s0 :| ss0) <-
    use (world . snake)
  mayd <- use (world . lastDir)
  case mayd of
    Nothing -> return ()
    Just d -> do
      let nextMove = move s0 d
      case () of
           -- The next move eats the food
        () | nextMove == f0 -> do
               world . snake .= f0 :| s0 : ss0
               freshFood
           -- The next move kills the snake
           | nextMove `elem` currentSnake -> do
               freshPlayer
               freshFood
           -- The next move simply moves the snake
           | otherwise -> do
               let newTail = take (length ss0) $ s0 : ss0
               world . snake .= nextMove :| newTail


mainLoop
  :: ( MonadState Game m
     , MonadIO m
     )
  => m ()
mainLoop = do
  -- Get all of sdl's events and run our updates on the world.
  evs <-
    fmap eventPayload
      <$> pollEvents
  mapM_ handleEvent evs

  shouldStep <- tickTime
  when shouldStep stepWorld
  render

  unless (any shouldQuit evs) mainLoop


newGame :: IO Game
newGame = do
  SDL.initializeAll
  let wcfg = defaultWindow
        { windowInitialSize = V2 640 480 }
      rcfg = defaultRenderer
        { rendererType = AcceleratedVSyncRenderer }
  w <- createWindow "Snake" wcfg
  r <- createRenderer w (-1) rcfg
  t <- liftIO getCurrentTime
  return Game
    { _gameWorld    = initialWorld
    , _gameWindow   = w
    , _gameRenderer = r
    , _gameLastUTC  = t
    , _gameDeltaTime= 0
    , _gameIsPaused = False
    }


main :: IO ()
main =
  newGame
    >>= evalStateT (setup >> mainLoop)
\end{code}
