{-# LANGUAGE OverloadedStrings #-}
module Rendering
  ( openWindow
  , closeWindow
  , initIntroResources
  , initGameResources
  , verticies
  , indices
  , Descriptor (Descriptor)
  ) where

import Control.Monad
import Data.Text                              (Text)
import Foreign.C
import Foreign.Marshal.Array                  (withArray)
import Foreign.Ptr                            (plusPtr, nullPtr, Ptr)
import Foreign.Storable                       (sizeOf)
import Graphics.GLUtil (readTexture, texture2DWrap)
import Graphics.Rendering.OpenGL as GL hiding (Size)
import SDL                             hiding (Point, Event, Timer, (^+^), (*^), (^-^), dot)

import LoadShaders

resX = 800 :: Int
resY = 600 :: Int

openWindow :: Text -> (CInt, CInt) -> IO SDL.Window
openWindow title (sizex,sizey) = do
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
closeWindow window = do
    SDL.destroyWindow window
    SDL.quit

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

initIntroResources :: [GLfloat] -> [GLuint] -> Double -> (Double, Double) -> IO Descriptor
initIntroResources vs idx ppos bpos =  
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

    -- | Assign Textures
    activeTexture            $= TextureUnit 0
    let tex_00 = "Resources/Textures/Pong.png"
    tx0 <- loadTex tex_00
    texture Texture2D        $= Enabled
    textureBinding Texture2D $= Just tx0

    -- | Shaders
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "Shaders/gameIntro.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/gameIntro.frag")]
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

    return $ Descriptor triangles (fromIntegral numIndices)

loadTex :: FilePath -> IO TextureObject
loadTex f =
  do
    t <- either error id <$> readTexture f
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Repeated, ClampToEdge)
    return t

initGameResources :: [GLfloat] -> [GLuint] -> Double -> (Double, Double) -> IO Descriptor
initGameResources vs idx ppos bpos =  
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
        ShaderInfo VertexShader   (FileSource "Shaders/gamePlay.vert"),
        ShaderInfo FragmentShader (FileSource "Shaders/gamePlay.frag")]
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

    return $ Descriptor triangles (fromIntegral numIndices)

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

