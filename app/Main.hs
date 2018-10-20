{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiWayIf #-}
module Main where 

import Control.Concurrent
import Control.Monad
import Data.Text                              (Text)
import Foreign.C                              
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import FRP.Yampa
import Graphics.Rendering.OpenGL as GL hiding (Size)
import Text.Printf

import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot)
import SDL.Input.Keyboard.Codes

import Input
import Rendering

-- import Debug.Trace as DT

-- < Rendering > ----------------------------------------------------------

draw window (Game ppos bpos gstg) =
  do
    (Descriptor triangles numIndices) <- gameResources
     
    GL.clearColor $= Color4 0 0 0 1
    GL.clear [ColorBuffer]
    bindVertexArrayObject $= Just triangles
    drawElements Triangles numIndices GL.UnsignedInt nullPtr
     
    GL.accum GL.Accum  (1.0 - mBlur)
    GL.accum GL.Return 1.0
    SDL.glSwapWindow window
    GL.accum GL.Load mBlur

      where
        gameResources = case gstg of
          GameIntro   -> initIntroResources verticies indices ppos bpos
          GamePlaying -> initGameResources  verticies indices ppos bpos

 -- < Animate > ------------------------------------------------------------

type WinInput  = Event SDL.EventPayload
type WinOutput = (Game, Bool)

animate :: Text                     -- ^ window title
        -> Int                      -- ^ window width in pixels
        -> Int                      -- ^ window height in pixels
        -> SF WinInput (Game, Bool) -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    window <- openWindow title (toEnum winWidth, toEnum winHeight)

    lastInteraction <- newMVar =<< SDL.time   
    -- Input Logic ---------------------------------------------------------
    let senseInput _ =
          do
            currentTime <- SDL.time
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent                          
            return (dt, Event . SDL.eventPayload <$> mEvent) 
    -- Output Logic --------------------------------------------------------
        renderOutput _ ((gameState), shouldExit) =
          do
            draw window gameState
            return shouldExit 

    -- Reactimate -----------------------------------------------------
    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf

    closeWindow window
    
playerPos :: Double -> SF AppInput Double
playerPos pp0 =
  switch sf cont
    where
      sf = proc input -> do
        keyLeft  <- key SDL.ScancodeLeft  "Pressed" -< input
        keyRight <- key SDL.ScancodeRight "Pressed" -< input
        let res :: (  Double
                    , Event ()
                    , Event ())
            res =  ( pp0
                   , keyLeft
                   , keyRight)
        returnA -< (pp0, mergeEvents
                         [ keyLeft
                         , keyRight ] `tag` res)

      cont (x, keyLeft, keyRight) =
        if | isEvent keyLeft -> movePlayer x (-0.5)
           | otherwise       -> movePlayer x   0.5

movePlayer :: Double -> Double -> SF AppInput Double
movePlayer pp0 v0 =
  switch sf cont
    where
         sf = proc input -> do
            p       <- -- DT.trace ("p: " ++ show pp0 ++ "\n") $
                       (pp0 +) ^<< integral -< v0
            keyLeft <- key SDL.ScancodeLeft  "Released" -< input
            keyRight<- key SDL.ScancodeRight "Released" -< input
            returnA -< (p, mergeEvents
                           [ keyLeft  
                           , keyRight ] `tag` p) :: (Double, Event Double)
         cont = playerPos

ballPos :: Vel -> Pos -> SF () (Pos,Vel)
ballPos v0 p0 =
  ballPos' cor' rad p0 v0
    where
        cor'   = cor defPhysics
        rad    = 10 / (fromIntegral resY)

ballPos' :: COR -> Radius -> Pos -> Vel -> SF () (Pos,Vel)
ballPos' cor rad p0 v0 =
  bouncingBall' p0 v0
    where
      bouncingBall' p0 v0 =
        switch sf cont
          where
            sf   = proc () -> do
              ((p,v), col) <- -- DT.trace ("ball pos: " ++ show p0 ++ "\n") $
                              collidingBall' rad p0 v0 -< ()
              returnA -< ((p, v), col `tag` fromEvent col ) :: ((Pos, Vel), Event (Dir,(Pos,Vel)))
            cont (dir,(p,v)) = bouncingBall' p (reflect dir ((-cor) *^ v))            
      reflect l v = (2*(v `dot` l)/(l `dot` l)) *^ l ^-^ v

fallingBall :: Pos -> Vel -> SF () (Pos, Vel)
fallingBall bp0 bv0 =
  proc () -> do
    v <- -- DT.trace ("bv0: " ++ show bv0 ++ "\n") $
         (bv0 ^+^) ^<< integral -< gee defPhysics
    p <- -- DT.trace ("bp0: " ++ show bp0 ++ "\n") $
         (bp0 ^+^) ^<< integral -< v
    returnA -< (p,v)
      
collidingBall' :: Radius -> Pos -> Vel ->
                SF () ((Pos, Vel), Event (Dir,(Pos,Vel)))
collidingBall' rad p0 v0  = proc () -> do
  pv@(p,v) <- --DT.trace ("(p,v): " ++ show (p0,v0) ++ "\n") $
              fallingBall p0 v0 -< ()
  hitXMin  <- edgeTag ( 1, 0)   -< fst p <= xMin bounds + rad
  hitYMin  <- edgeTag ( 0, 1)   -< snd p <= yMin bounds + rad
  hitXMax  <- edgeTag (-1, 0)   -< fst p >= xMax bounds - rad
  hitYMax  <- edgeTag ( 0,-1)   -< snd p >= yMax bounds - rad
  let hitInfo = foldr1 (mergeBy mergeHits) [hitXMin,hitYMin,hitXMax,hitYMax]
  returnA -< (pv, hitInfo `attach` pv)
  where
    mergeHits = (^+^) -- simply add the two collision directions together.

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

-- < Physics > --------------------------------------------------------------

data Bounds =
  Bounds
  { xMin  :: Double
  , xMax  :: Double
  , yMin  :: Double
  , yMax  :: Double
  }

bounds :: Bounds
bounds = 
  bounds' xMin xMax yMin yMax
    where
      xMin = -400 -- minX
      xMax =  400 -- maxX
      yMin =  0   -- minY
      yMax =  600 -- maxY

bounds' :: Double -> Double -> Double -> Double -> Bounds
bounds' xmin xmax ymin ymax =
  Bounds
  { xMin = xmin / fromIntegral resX
  , xMax = xmax / fromIntegral resX
  , yMin = ymin / fromIntegral resY
  , yMax = ymax / fromIntegral resY
  }

data PhysicsContext =
     PhysC
     { gee :: Acc    -- A unit of acceleration due to gravity
     , cor :: Double -- coefficient of restitution
     }
  deriving Show

type COR  = Double
type Vel  = (Double, Double)
type Acc  = (Double, Double)
type Dir  = (Double, Double)

defPhysics =
  PhysC
  { gee = (0.0,-4.9)
  , cor = 1.01
  }

-- < Game Logic > ---------------------------------------------------------
data GameStage = GameIntro
               | GamePlaying
               | GameFinished
               | GameMenu
               deriving Show

data Game =
     Game
     { pPos :: Double    -- Player Position
     , bPos :: Pos       -- Ball   Position
     , gStg :: GameStage -- Game   Stage
     } 
  deriving Show

type Pos  = (Double, Double)

defaultGame :: Game
defaultGame = Game pp0 bp0 GameIntro
  where
    pp0 = 0         :: Double
    bp0 = (0.0,0.4) :: (Double, Double)

mainGame :: SF AppInput Game
mainGame =
  loopPre defaultGame $ 
  proc (input, gameState) -> do
    gs <- case gStg gameState of
            GameIntro   -> gameIntro -< (input, gameState)
            GamePlaying -> gamePlay  -< input
    returnA -< (gs, gs)

gameIntro :: SF (AppInput, Game) Game
gameIntro =
  switch sf cont        
     where sf =
             proc (input, gameState) -> do
               introState <- returnA -< gameState
               playState  <- returnA -< gameState { gStg =  GamePlaying }
               skipE      <- key SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after loadDelay () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) `tag` playState)
           cont game  = 
             proc input -> do
               returnA  -< game

gamePlay :: SF AppInput Game
gamePlay =
    switch sf (const mainGame)        
     where sf =
             proc input -> do
               gameState <- gameSession -< input
               reset     <- key SDL.ScancodeSpace "Pressed" -< input
               returnA   -< (gameState, reset)

gameSession :: SF AppInput Game
gameSession =
  proc input -> do
    ppos         <- playerPos   $ pPos defaultGame -< input
    (bpos, bvel) <- ballPos bv0 $ bPos defaultGame -< ()
    returnA      -< Game ppos bpos GamePlaying
      where bv0 = (0.5,0.5) :: (Double, Double)

-- < Global Constants > ---------------------------------------------------
mBlur     = 0.25 :: Float
loadDelay = 5.0  :: Double
resX      = 800  :: Int
resY      = 600  :: Int

-- < Main Function > ------------------------------------------------------
main :: IO ()
main =  do
  animate "Pong"
            resX
            resY
            (parseWinInput >>> (mainGame &&& handleExit))

-- | A game of Pong in haskell
-- 
-- | Game state:
--
-- TODO : Add music
-- TODO : Add Score
-- TODO : Add bat collision
-- TODO : Add Lives Count
-- TODO : Add "You Lost" screen
--
-- score      :: Integer
-- score  (x) <- 0->n
--
-- lives      :: Integer
-- lives  (x) <- n->0

