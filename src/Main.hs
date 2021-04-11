{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((&), (^.), (.~), makeLenses)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State (runState, state)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.JSString as JSS
import Data.Text (Text)
import GHCJS.DOM (inAnimationFrame')
import GHCJS.DOM.ANGLEInstancedArrays
    ( drawArraysInstancedANGLE
    , vertexAttribDivisorANGLE )
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import GHCJS.DOM.Types
    ( ANGLEInstancedArrays(..)
    , GLenum
    , GLintptr
    , GObject(..)
    , HTMLCanvasElement(..)
    , JSM
    , liftJSM
    , RenderingContext(..)
    , toJSVal
    , uncheckedCastTo
    , unsafeCastTo
    , WebGLBuffer
    , WebGLProgram
    , WebGLRenderingContext(..)
    , WebGLShader
    , WebGLUniformLocation )
import qualified GHCJS.DOM.WebGLRenderingContextBase as GL
import Language.Javascript.JSaddle (jsf, jsg, new)
import Linear.Matrix ((!*!), identity, mkTransformationMat, M44, transpose)
import Linear.Metric (dot, norm)
import Linear.Projection (ortho)
import Linear.V2 (V2(..), _x, _y)
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Vector ((^*), scaled)
import qualified Reflex as RX
import Reflex ((<@))
import Reflex.Dom (el', _element_raw, mainWidgetWithCss, text)
import System.Random (mkStdGen, randomR, StdGen)

data GLContext = GLContext
    { gl :: WebGLRenderingContext
    , angleInstancedArrays :: ANGLEInstancedArrays }

data GLObjects = GLObjects
    { translationBuffer :: WebGLBuffer
    , projectionUniform :: WebGLUniformLocation }

type Particles = [Particle]
type ProjectionMatrix = M44 Double

data Particle = Particle
    { _position :: V2 Float
    , _velocity :: V2 Float } deriving (Eq, Show)
makeLenses ''Particle

data CanvasSize = CanvasSize
    { width :: Int
    , height :: Int } deriving (Eq, Show)

data BoundingBox = BoundingBox
    { _left :: Float
    , _right :: Float
    , _bottom :: Float
    , _top :: Float }
makeLenses ''BoundingBox

main :: JSM ()
main = mainWidgetWithCss style $ do
    (canvasEl, _) <- el' "canvas" $ text ""
    canvasRaw <- liftJSM . unsafeCastTo HTMLCanvasElement $
        _element_raw canvasEl
    glContext <- liftJSM $ getGLContext canvasRaw
    glObjects <- liftJSM $ initGL glContext

    eTick <- RX.tickLossyFromPostBuildTime $ realToFrac tickInterval

    (bCanvasSize, eCanvasSizeChanged) <- trackCanvasSize
        canvasRaw eTick

    let bProjectionMatrix = projectionMatrixFromCanvasSize <$> bCanvasSize

    let bBoundingBox = boundingBoxFromCanvasSize <$> bCanvasSize

    bParticles <- RX.accumB
        updateParticles initialParticles (bBoundingBox <@ eTick)
    let eRender = RX.tag ((,) <$> bParticles <*> bProjectionMatrix) eTick

    RX.performEvent_ $
        liftIO . updateViewportSize glContext <$> eCanvasSizeChanged
    RX.performEvent_ $ liftIO . (render glContext glObjects) <$> eRender

    return ()

getGLContext :: HTMLCanvasElement -> IO GLContext
getGLContext canvas = do
    gl <- do
        context <- Canvas.getContextUnsafe
            canvas ("webgl" :: Text) ([] :: [()])
        webglContext <- unsafeCastTo WebGLRenderingContext context
        return webglContext
    angleInstancedArrays <- uncheckedCastTo ANGLEInstancedArrays <$>
        GL.getExtensionUnsafe gl ("ANGLE_instanced_arrays" :: Text)
    return $ GLContext gl angleInstancedArrays

initGL :: GLContext -> IO GLObjects
initGL GLContext{..} = do
    GL.clearColor gl 0.5 0.5 0.5 1.0

    vertexBuffer <- GL.createBuffer gl
    GL.bindBuffer gl GL.ARRAY_BUFFER (Just vertexBuffer)
    bufferDataFloat gl
        GL.ARRAY_BUFFER particleGeometryData GL.STATIC_DRAW

    translationBuffer <- GL.createBuffer gl
    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
    bufferDataSizeOnly gl GL.ARRAY_BUFFER
        (fromIntegral $ maxParticlesNum * 8) GL.DYNAMIC_DRAW

    vertexShader <- buildShader gl GL.VERTEX_SHADER vertexShaderSource
    fragmentShader <- buildShader
        gl GL.FRAGMENT_SHADER fragmentShaderSource
    program <- buildProgram gl vertexShader fragmentShader
    GL.useProgram gl (Just program)

    modelUniform <- GL.getUniformLocation
        gl (Just program) ("u_model" :: Text)
    GL.uniformMatrix4fv gl (Just modelUniform) False $
        listFromMatrix $ modelMatrix $ realToFrac particleRadius

    projectionUniform <- GL.getUniformLocation
        gl (Just program) ("u_projection" :: Text)

    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just vertexBuffer
    positionAttr <- fromIntegral <$> GL.getAttribLocation
        gl (Just program) ("a_position" :: Text)
    GL.enableVertexAttribArray gl positionAttr
    GL.vertexAttribPointer gl positionAttr 2 GL.FLOAT False 0 0
    vertexAttribDivisorANGLE angleInstancedArrays
        (fromIntegral positionAttr) 0

    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
    translationAttr <- fromIntegral <$> GL.getAttribLocation
        gl (Just program) ("a_translation" :: Text)
    GL.enableVertexAttribArray gl translationAttr
    GL.vertexAttribPointer gl translationAttr 2 GL.FLOAT False 0 0
    vertexAttribDivisorANGLE angleInstancedArrays
        (fromIntegral translationAttr) 1

    err <- GL.getError gl
    putStrLn $ "Error: " ++ show err

    return $ GLObjects
        translationBuffer
        projectionUniform

vertexShaderSource :: String
vertexShaderSource = L.intercalate "\n"
    [ "attribute vec2 a_position;"
    , "attribute vec2 a_translation;"
    , "uniform mat4 u_model;"
    , "uniform mat4 u_projection;"
    , "void main() {"
    , "    gl_Position ="
    , "        u_projection * (u_model * vec4(a_position, 0.0, 1.0) + "
    , "        vec4(a_translation, 0.0, 1.0));"
    , "}" ]

fragmentShaderSource :: String
fragmentShaderSource = L.intercalate "\n"
    [ "void main() {"
    , "   gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);"
    , "}" ]

render :: GLContext
       -> GLObjects
       -> (Particles, ProjectionMatrix)
       -> IO ()
render GLContext{..} GLObjects{..} (particles, projectionMatrix) = do
    GL.uniformMatrix4fv gl (Just projectionUniform) False $
        listFromMatrix projectionMatrix
    GL.clear gl GL.COLOR_BUFFER_BIT

    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
    bufferSubDataFloat gl GL.ARRAY_BUFFER 0 translations
    GL.bindBuffer gl GL.ARRAY_BUFFER Nothing

    drawArraysInstancedANGLE angleInstancedArrays
        GL.TRIANGLE_FAN 0 (particleGeometryNumSlices + 2) $ length particles
    where
        translations = [n | p <- particles, n <- [ x p, y p ]]
        x p = p ^. position ^. _x
        y p = p ^. position ^. _y

buildShader :: WebGLRenderingContext -> GLenum -> String -> JSM WebGLShader
buildShader gl shaderType sourceCode = do
    shader <- GL.createShader gl shaderType
    GL.shaderSource gl (Just shader) sourceCode
    GL.compileShader gl $ Just shader
    log <- GL.getShaderInfoLogUnsafe gl $ Just shader
    putStrLn $ "Shader compilation log: " ++ log
    return shader

buildProgram :: WebGLRenderingContext
             -> WebGLShader
             -> WebGLShader
             -> JSM WebGLProgram
buildProgram gl vertexShader fragmentShader = do
    program <- GL.createProgram gl
    GL.attachShader gl (Just program) (Just vertexShader)
    GL.attachShader gl (Just program) (Just fragmentShader)
    GL.linkProgram gl (Just program)
    log <- GL.getProgramInfoLogUnsafe gl (Just program)
    putStrLn $ "Program log: " ++ log
    return program

style :: BS.ByteString
style = BS.intercalate "\n"
    [ "body {"
    , "    margin: 0;"
    , "}"
    , "canvas {"
    , "    display: block;"
    , "    width: 100vw;"
    , "    height: 100vh;"
    , "}" ]

particleGeometryData :: [Float]
particleGeometryData = concat $ center : perimeter where
    concat ps = [n | (x, y) <- ps, n <- [x, y]]
    center = (0.0, 0.0)
    perimeter = point <$> [0 .. numSlices]
    point index = (cos a, sin a) where
        a = (fromIntegral index) * 2.0 * pi / (fromIntegral numSlices)
    numSlices = particleGeometryNumSlices

particleGeometryNumSlices :: Int
particleGeometryNumSlices = 7

listFromMatrix :: M44 a -> [a]
listFromMatrix = concat . fmap Foldable.toList . Foldable.toList

consoleLog :: String -> IO ()
consoleLog = js_consoleLog . JSS.pack

foreign import javascript unsafe "console.log($1)"
    js_consoleLog :: JSS.JSString -> IO ()

bufferDataFloat :: WebGLRenderingContext
                -> GLenum
                -> [Float]
                -> GLenum
                -> JSM ()
bufferDataFloat gl binding data' usage = do
    toJSVal gl ^. jsf ("bufferData" :: Text)
        ( binding
        , new (jsg ("Float32Array" :: Text)) [ data' ]
        , usage )
    return ()

bufferDataSizeOnly :: WebGLRenderingContext
                   -> GLenum
                   -> Int32
                   -> GLenum
                   -> JSM ()
bufferDataSizeOnly gl binding size usage = do
    toJSVal gl ^. jsf ("bufferData" :: Text) ( binding, size, usage )
    return ()

bufferSubDataFloat :: WebGLRenderingContext
                   -> GLenum
                   -> Int32
                   -> [Float]
                   -> JSM ()
bufferSubDataFloat gl binding offset data' = do
    toJSVal gl ^. jsf ("bufferSubData" :: Text)
        ( binding
        , offset
        , new (jsg ("Float32Array" :: Text)) [ data' ] )
    return ()

maxParticlesNum :: Int
maxParticlesNum = 10000

initialParticles :: Particles
initialParticles =
    -- initialParticles2
    take 300 . L.unfoldr (Just . randomParticle) $ mkStdGen 0

initialParticles2 :: Particles
initialParticles2 =
    [ Particle { _position = V2 100 100, _velocity = V2 200 0 }
    , Particle { _position = V2 200 110, _velocity = V2 0 0 }
    , Particle { _position = V2 200 90, _velocity = V2 0 0 } ]

maxInitialSpeed :: Float
maxInitialSpeed = 200

randomParticle :: StdGen -> (Particle, StdGen)
randomParticle = runState $ do
    x <- state $ randomR (100.0, 200.0)
    y <- state $ randomR (100.0, 200.0)
    vx <- state $ randomR (-maxInitialSpeed, maxInitialSpeed)
    vy <- state $ randomR (-maxInitialSpeed, maxInitialSpeed)
    return $ Particle { _position = V2 x y, _velocity = V2 vx vy }

updateParticles :: Particles -> BoundingBox -> Particles
updateParticles particles bbox = updateParticle bbox particles <$> particles

updateParticle :: BoundingBox -> Particles -> Particle -> Particle
updateParticle bbox particles particle =
    clampToBoundingBox bbox .
    bounceOfWalls bbox .
    integrateVelocity .
    handleCollisions (filter (/= particle) particles) $
        particle

integrateVelocity :: Particle -> Particle
integrateVelocity particle = particle & position .~
    ( particle ^. position +
      particle ^. velocity * (realToFrac tickInterval) )

handleCollisions :: Particles -> Particle -> Particle
handleCollisions particles particle =
    foldr handleCollision particle particles

handleCollision :: Particle -> Particle -> Particle
handleCollision anotherParticle particleOfInterest =
    if collision
    then particleOfInterest
        & velocity .~ (v1 - dp ^* (dot_dv_dp / (norm_dp ** 2)))
    else particleOfInterest
    where
        collision = onCollistionDistance && movingTowardsEachOther
        onCollistionDistance = norm_dp < 2 * particleRadius
        movingTowardsEachOther = dot_dv_dp < 0
        norm_dp = norm dp
        dot_dv_dp = dot dv dp
        dv = v1 - v2
        dp = p1 - p2
        v1 = particleOfInterest ^. velocity
        p1 = particleOfInterest ^. position
        v2 = anotherParticle ^. velocity
        p2 = anotherParticle ^. position

bounceOfWalls :: BoundingBox -> Particle -> Particle
bounceOfWalls bbox = bounceLeft
                   . bounceRight
                   . bounceBottom
                   . bounceTop
    where
        bounceLeft p =
            if p ^. position ^. _x < (bbox ^. left + particleRadius)
            then p & (velocity . _x) .~ (abs $ p ^. velocity ^. _x)
            else p
        bounceRight p =
            if p ^. position ^. _x > (bbox ^. right - particleRadius)
            then p & (velocity . _x) .~ (- (abs $ p ^. velocity ^. _x))
            else p
        bounceBottom p =
            if p ^. position ^. _y < (bbox ^. bottom + particleRadius)
            then p & (velocity . _y) .~ (abs $ p ^. velocity ^. _y)
            else p
        bounceTop p =
            if p ^. position ^. _y > (bbox ^. top - particleRadius)
            then p & (velocity . _y) .~ (- (abs $ p ^. velocity ^. _y))
            else p

clampToBoundingBox :: BoundingBox -> Particle -> Particle
clampToBoundingBox bbox = clampLeft
                        . clampRight
                        . clampBottom
                        . clampTop
    where
        clampLeft p =
            if p ^. position ^. _x < (bbox ^. left + particleRadius)
            then p & (position . _x) .~ (bbox ^. left + particleRadius)
            else p
        clampRight p =
            if p ^. position ^. _x > (bbox ^. right - particleRadius)
            then p & (position . _x) .~ (bbox ^. right - particleRadius)
            else p
        clampBottom p =
            if p ^. position ^. _y < (bbox ^. bottom + particleRadius)
            then p & (position . _y) .~ (bbox ^. bottom + particleRadius)
            else p
        clampTop p =
            if p ^. position ^. _y > (bbox ^. top - particleRadius)
            then p & (position . _y) .~ (bbox ^. top - particleRadius)
            else p

trackCanvasSize :: ( Monad m
                   , RX.MonadHold t m
                   , MonadIO (RX.Performable m)
                   , RX.MonadSample t (RX.Performable m)
                   , RX.PerformEvent t m
                   , RX.TriggerEvent t m )
                => HTMLCanvasElement
                -> RX.Event t a
                -> m (RX.Behavior t CanvasSize, RX.Event t CanvasSize)
trackCanvasSize canvas event = do
    (eSizeChanged, triggerSizeChanged) <- RX.newTriggerEvent
    bSize <- RX.hold (CanvasSize 0 0) eSizeChanged
    RX.performEvent_ $ (\_ -> do
            CanvasSize{..} <- RX.sample bSize
            liftIO $ do
                width' <- floor <$> Element.getClientWidth canvas
                height' <- floor <$> Element.getClientHeight canvas
                if width' /= width || height' /= height then do
                    Canvas.setWidth canvas (fromIntegral width')
                    Canvas.setHeight canvas (fromIntegral height')
                    triggerSizeChanged $ CanvasSize width' height'
                    return ()
                else
                    return ()
        ) <$> event
    return (bSize, eSizeChanged)

updateViewportSize :: GLContext -> CanvasSize -> JSM ()
updateViewportSize GLContext{..} CanvasSize{..} =
    GL.viewport gl 0 0 (fromIntegral width) (fromIntegral height)

projectionMatrixFromCanvasSize :: CanvasSize -> M44 Double
projectionMatrixFromCanvasSize CanvasSize{..} =
    transpose $ projection !*! modelView
    where
        projection = ortho
            (-width') width' (-height') height' (-1.0) 1.0
        modelView = mkTransformationMat
            identity $ V3 (-width') (-height') 0.0
        width' = (fromIntegral width) / 4.0
        height' = (fromIntegral height) / 4.0

modelMatrix :: Double -> M44 Double
modelMatrix radius = scaled $ V4 radius radius 1.0 1.0

particleRadius :: Float
particleRadius = 10.0

tickInterval :: Double
tickInterval = 0.04

boundingBoxFromCanvasSize :: CanvasSize -> BoundingBox
boundingBoxFromCanvasSize CanvasSize{..} = BoundingBox
    { _left = 0.0
    , _right = fromIntegral width
    , _bottom = 0.0
    , _top = fromIntegral height }
