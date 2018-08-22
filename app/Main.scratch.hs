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
import LoadShaders
import Text.Printf

import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot)
import Input

import Debug.Trace as DT

-- < Rendering > ----------------------------------------------------------
openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) =
  do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear                    
    do renderQuality <- SDL.get SDL.HintRenderScaleQuality          
       when (renderQuality /= SDL.ScaleLinear) $                    
         putStrLn "Warning: Linear texture filtering not enabled!"  
     
    window <- SDL.createWindow
            "Pong Yampa / SDL / OpenGL Example"
            SDL.defaultWindow {SDL.windowInitialSize = V2 sizex sizey,
                               SDL.windowOpenGL = Just SDL.defaultOpenGL}
    SDL.showWindow window
    _ <- SDL.glCreateContext window
    
    return window

closeWindow :: SDL.Window -> IO ()
closeWindow window =
  do
    SDL.destroyWindow window
    SDL.quit

draw :: SDL.Window -> Double -> (Double, Double) -> IO ()
draw window ppos bpos =
  do
    (Descriptor triangles numIndices) <- initResources verticies indices ppos bpos

    GL.clearColor $= Color4 0 0 0 1
    GL.clear [ColorBuffer]
    bindVertexArrayObject $= Just triangles
    drawElements Triangles numIndices GL.UnsignedInt nullPtr

    GL.accum GL.Accum  (1.0 - mBlur)
    GL.accum GL.Return 1.0
    SDL.glSwapWindow window
    GL.accum GL.Load mBlur

-- < OpenGL > -------------------------------------------------------------
data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices

verticies :: [GLfloat]
verticies =
  [ -- | positions    -- | colors      -- | uv
    1.0,  1.0, 0.0,   1.0, 0.0, 0.0,   1.0, 1.0,
    1.0, -1.0, 0.0,   0.0, 1.0, 0.0,   1.0, 0.0,
   -1.0, -1.0, 0.0,   0.0, 0.0, 1.0,   0.0, 0.0,
   -1.0,  1.0, 0.0,   0.0, 0.0, 0.0,   0.0, 1.0
  ]

indices :: [GLuint]
indices =
  [          -- Note that we start from 0!
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]

realToFracT :: (Double, Double) -> (GLfloat, GLfloat)
realToFracT = (\ (x,y) -> (realToFrac x, realToFrac y))

initResources :: [GLfloat] -> [GLuint] -> Double -> (Double, Double) -> IO Descriptor
initResources vs idx ppos bpos =  
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length verticies
    withArray verticies $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head verticies))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length indices
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * length indices)
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 8 * floatSize

    -- | Positions
    let vPosition  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | UV
    let uvCoords   = AttribLocation 1
        uvOffset   = 6 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled

    -- | Shaders
    program <- loadShaders [
        ShaderInfo VertexShader (FileSource "Shaders/shader.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/shader.frag")]
    currentProgram $= Just program

    -- | Set Uniforms
    location0         <- get (uniformLocation program "fPPos")
    uniform location0 $= (realToFrac ppos :: GLfloat)

    location1         <- get (uniformLocation program "vBPos")
    uniform location1 $= (Vector2 (realToFrac $ fst bpos)
                                  (realToFrac $ snd bpos) :: Vector2 GLfloat)

    location2         <- get (uniformLocation program "u_resolution")
    let u_res         = Vector2 (toEnum resX) (toEnum resY) :: Vector2 GLfloat
    uniform location2 $= u_res

    currentTime       <- SDL.time
    location3         <- get (uniformLocation program "u_time")
    uniform location3 $= (currentTime :: GLfloat)
    
    -- | Set Transform Matrix
    let tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ] :: [GLfloat]
          
    transform         <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location4         <- get (uniformLocation program "transform")
    uniform location4 $= transform
    
    -- | Unload buffers
    bindVertexArrayObject         $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing

    -- return $ Descriptor triangles posOffset (fromIntegral numIndices)
    return $ Descriptor triangles (fromIntegral numIndices)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

 -- < Animate > ------------------------------------------------------------
type WinInput = Event SDL.EventPayload
-- TODO: refactor to pass game state, rather than individual substates
-- ~
-- type WinOutput = ( Game, Bool) - something like that would be nice!
-- type WinOutput = ( (Double, (Double, Double)), Bool)
--type WinOutput = (GameState, Bool)
type WinOutput = (Game, Bool)

animate :: Text                   -- ^ window title
        -> Int                    -- ^ window width in pixels
        -> Int                    -- ^ window height in pixels
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf =
  do
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
        renderSF _ ((Game ppos bpos), shouldExit) =
          do
            draw window ppos bpos
            return shouldExit 

    -- Reactimate -----------------------------------------------------
    reactimate (return NoEvent)
               senseInput
               renderSF
               sf

    closeWindow window

-- < Input Handling > -----------------------------------------------------

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
            p       <- DT.trace ("p: " ++ show pp0 ++ "\n") $
                       (pp0 +) ^<< integral -< v0
            keyLeft <- key SDL.ScancodeLeft  "Released" -< input
            keyRight<- key SDL.ScancodeRight "Released" -< input
            returnA -< (p, mergeEvents
                           [ keyLeft  
                           , keyRight ] `tag` p) :: (Double, Event Double)
         cont = playerPos


ballPos :: Pos -> Vel -> SF () (Pos,Vel)
ballPos p0 v0 =
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
    v <- DT.trace ("bv0: " ++ show bv0 ++ "\n") $
         (bv0 ^+^) ^<< integral -< gee defPhysics
    p <- DT.trace ("bp0: " ++ show bp0 ++ "\n") $
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

-- < Game Types > --------------------------------------------------------------

type COR  = Double
type Pos  = (Double, Double)
type Vel  = (Double, Double)
type Acc  = (Double, Double)
type Dir  = (Double, Double)

data Bounds =
  Bounds
  { xMin  :: Double
  , xMax  :: Double
  , yMin  :: Double
  , yMax  :: Double }

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
  , yMax = ymax / fromIntegral resY }

data PhysicsContext =
     PhyC
       { gee :: Acc    -- A unit of acceleration due to gravity
       , cor :: Double -- coefficient of restitution
       }

defPhysics =
  PhyC { gee = (0.0,-4.9)
       , cor = 1.0
       }

data Suka = Blad { foo :: Double
                 , bar :: String }

-- data Game = Game { pPos  :: Double    -- Player Position
-- --                 , pVel  :: Vel       --        Velocity
--                  , bPos  :: Pos       -- Ball   Position
-- --                 , bVel  :: Vel       --        Velocity
-- --                 , lives :: Integer
-- --                 , score :: Integer
--                  --, iter  :: Integer   -- for Motion Blur
--                  } 
--           deriving Show

data Game =
     Game { pPos  :: Double    -- Player Position
          , bPos  :: Pos       -- Ball   Position
--          , exit  :: Bool      -- Reset  Game
          } 
          deriving Show


type GameState = (Double, (Double, Double))
--   { pPosST  :: Double
--   , bPosST  :: Pos }

-- < Game Logic > ---------------------------------------------------------

pp0 :: Double           -- player position
pp0 = 0

bp0 :: (Double, Double) -- ball position
bp0 = (0.0,0.4)

bv0 :: (Double, Double) -- ball velocity
bv0 = (1.0,1.0)

mBlur :: Float
mBlur = 0.25

initGame :: Game -> Game
initGame x = x

game :: SF AppInput Game
game = switch sf (const game)        
     where sf =
             proc input -> do
               gameState <- gameSession -< input
               reset     <- key SDL.ScancodeSpace "Pressed" -< input
               returnA   -< (gameState, reset)

gameSession :: SF AppInput Game
gameSession =
  proc input -> do
    ppos         <- playerPos pp0     -< input
    (bpos, bvel) <- ballPos   bp0 bv0 -< ()
    --returnA      -< Game ppos pvel bpos bvel lvs sc
    returnA      -< Game ppos bpos

pvel = undefined
lvs  = undefined
sc   = undefined
    
-- < Main Function > ------------------------------------------------------

data Foo =
     Bar { foo :: Double
         , bar :: String }
         deriving Show

resX = 800 :: Int
resY = 600 :: Int

main :: IO ()
main =
  do
    animate "Pong"
            resX
            resY
            --(parseWinInput >>> ((game >>^ initGame) &&& handleExit))
            (parseWinInput >>> (game &&& handleExit))
            -- | TODO: look at other examples of state handling, see if it all can be compressed to e.g.:
            -- (parseWinInput >>> ( ((game >>^ gameState)) &&& handleExit)) ... 

-- (>>^) :: Arrow a => a b c -> (c -> d) -> a b d
-- game :: SF AppInput Game
--             pPos :: Game -> Double
-- (game >>^ pPos)
--      :: SF AppInput Double

-- (game >>^ bPos)
--      :: SF AppInput (Double, Double)

-- (&&&) :: a b c -> a b c' -> a b (c, c')
-- (game >>^ pPos) &&& (game >>^ bPos)
-- :: SF AppInput Double -> SF AppInput (Double, Double) -> SF AppInput (Double, (Double, Double))

-- handleExit :: SF AppInput Bool
-- ( ((game >>^ pPos) &&& (game >>^ bPos)) &&& handleExit) ::
-- SF AppInput (Double, (Double, Double)) &&& SF AppInput Bool -> SF AppInput (Double, (Double, Double), Bool)

-- parseWinInput :: SF WinInput AppInput
-- (>>>) :: Category cat => cat a b -> cat b c -> cat a c
-- ...           :: SF          AppInput (Double, (Double, Double), Bool) ->
-- SF WinInput (Double, (Double, Double), Bool)

-- parseWinInput :: SF WinInput AppInput
-- game :: SF AppInput Game
-- pPos :: Game -> Double
-- bPos :: Game -> Double
-- handleExit :: SF AppInput Bool
-- (>>^) :: Arrow a => a b c -> (c -> d) -> a b d
-- (>>>) :: Category cat => cat a b -> cat b c -> cat a c
-- (&&&) :: a b c -> a b c' -> a b (c, c')


-- | Game state:
-- player pos :: Double
-- player pos (x)   <- keyLeft / keyRight
--
-- ball   pos :: (Double, Double)
-- ball   pos (x,y) <- dynamics + player collision
--
-- score      :: Integer
-- score  (x) <- 0->n
--
-- lives      :: Integer
-- lives  (x) <- n->0
--
--
-- Splash screen (show for 3 seconds, Any Key -> skip to Start menu)
--   Start menu (Press Start -> Game screen, Exit)
--     Game screen (play, Esc -> exit)

testGame :: SF AppInput Game
testGame =
  proc input -> do
    returnA -< Game ppos bpos GamePlaying
      where
        ppos = pPos defaultGameState
        bpos = bPos defaultGameState

gameIntro :: SF AppInput Game
gameIntro =
  switch sf (const mainGame)        
     where sf =
             proc input -> do
               gameState <- gameSession -< input
               suka      <- returnA -< (Game ppos bpos GamePlaying)
               skip      <- key SDL.ScancodeSpace "Pressed" -< input
               cont      <- after loadDelay () -< ()
               returnA   -< (gameState, skip `lMerge` cont)
                 where
                   playGame = Game ppos bpos GamePlaying
                   ppos = pPos defaultGame
                   bpos = bPos defaultGame
