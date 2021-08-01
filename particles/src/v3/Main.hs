{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Vector.Unboxed as VU
import Data.Witherable (catMaybes)
import Debug.Trace
import qualified GHCJS.Buffer as Buffer
import qualified GHCJS.DOM as DOM
import GHCJS.DOM.ANGLEInstancedArrays
    ( drawArraysInstancedANGLE
    , vertexAttribDivisorANGLE )
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import qualified GHCJS.DOM.Performance as Performance
import qualified GHCJS.DOM.GlobalPerformance as GlobalPerformance
import GHCJS.DOM.Types
    ( ANGLEInstancedArrays(..)
    , ArrayBuffer(..)
    , GLenum
    , GObject(..)
    , HTMLCanvasElement(..)
    , Performance
    , RenderingContext(..)
    , uncheckedCastTo
    , unsafeCastTo
    , WebGLBuffer
    , WebGLProgram
    , WebGLRenderingContext(..)
    , WebGLShader
    , WebGLUniformLocation )
import qualified GHCJS.DOM.WebGLRenderingContextBase as GL
import JavaScript.TypedArray.Internal (setIndex)
import Language.Javascript.JSaddle
    ( fromJSVal
    , ghcjsPure
    , JSM
    , jsval
    , liftJSM
    , MonadJSM )
import Linear.Matrix ((!*!), identity, mkTransformationMat, M44, transpose)
import Linear.Projection (ortho)
import Linear.V2 (_x, _y)
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Vector (scaled)
import qualified Particles.Model as Model
import qualified Particles.Model3 as Model3
import Particles.Types
import Particles.UI.GL
import qualified Reflex as RX
import Reflex.Dom (el', _element_raw, mainWidgetWithCss, text)

data GLContext = GLContext
    { gl :: WebGLRenderingContext
    , angleInstancedArrays :: ANGLEInstancedArrays }

data GLObjects = GLObjects
    { translationBuffer :: WebGLBuffer
    , projectionUniform :: WebGLUniformLocation }

type ProjectionMatrix = M44 Double
type ProjectionMatrixList = [Double]

data CanvasSize = CanvasSize
    { width :: Int
    , height :: Int } deriving (Eq, Show)

{-# INLINE kParticlesNum #-}
kParticlesNum :: Int
kParticlesNum = 1000

{-# INLINE kMapBucketCapacity #-}
kMapBucketCapacity :: Int
kMapBucketCapacity = 64

{-# INLINE kMapCellSize #-}
kMapCellSize :: Double
kMapCellSize = 20

main :: IO ()
main = mainWidgetWithCss style $ do
    window <- DOM.currentWindowUnchecked
    performance <- GlobalPerformance.getPerformance window
    (canvasEl, _) <- el' "canvas" $ text ""
    canvasRaw <- liftJSM . unsafeCastTo HTMLCanvasElement $
        _element_raw canvasEl
    glContext <- liftJSM $ getGLContext canvasRaw
    glObjects <- liftJSM $ initGL glContext
    particlesTransformationsStagingBuffer <-
        liftJSM $ Buffer.create $ kParticlesNum * 8

    (eAnimationFrame, triggerAnimationFrame) <- RX.newTriggerEvent
    let animationFrameCallback = \ timestamp -> do
            _ <- DOM.inAnimationFrame' animationFrameCallback
            liftIO $ triggerAnimationFrame timestamp
            return ()
    _ <- liftJSM $ DOM.inAnimationFrame' animationFrameCallback
    let eTick = eAnimationFrame

    (dCanvasSize, eCanvasSize) <- trackCanvasSize canvasRaw eTick
    let dProjectionMatrix =
            fmap projectionMatrixFromCanvasSize <$> dCanvasSize
    let dProjectionMatrixList = fmap listFromMatrix <$> dProjectionMatrix
    let dBoundingBox = fmap boundingBoxFromCanvasSize <$> dCanvasSize
    dModel <- RX.accumDyn updateModel Nothing
        $ catMaybes $ RX.tagPromptlyDyn dBoundingBox eTick
    let dParticles = fmap Model3.particles <$> dModel
    let dRenderingParams =
            RX.ffor2 dParticles dProjectionMatrixList $
                \ps pm -> (,) <$> ps <*> pm
    let eRender = catMaybes $ RX.tagPromptlyDyn dRenderingParams eTick

    RX.performEvent_
        $ liftJSM . updateViewportSize glContext
            <$> catMaybes eCanvasSize
    RX.performEvent_
        $ liftJSM
        . render glContext glObjects performance
            particlesTransformationsStagingBuffer
            <$> eRender

    return ()

updateModel
    :: Maybe Model3.ModelState
    -> BoundingBox
    -> Maybe Model3.ModelState
updateModel model bbox
    = Just
    $! maybe
        (Model3.initialState kMapBucketCapacity kMapCellSize bbox kParticlesNum)
        (Model3.unsafeUpdateState kMapBucketCapacity kMapCellSize bbox)
        model

getGLContext :: HTMLCanvasElement -> JSM GLContext
getGLContext canvas = do
    gl <- do
        context <- Canvas.getContextUnsafe
            canvas ("webgl" :: Text) ([] :: [()])
        unsafeCastTo WebGLRenderingContext context
    angleInstancedArrays <- uncheckedCastTo ANGLEInstancedArrays <$>
        GL.getExtensionUnsafe gl ("ANGLE_instanced_arrays" :: Text)
    return $ GLContext gl angleInstancedArrays

initGL :: GLContext -> JSM GLObjects
initGL GLContext{..} = do
    GL.clearColor gl 0.5 0.5 0.5 1.0

    vertexBuffer <- GL.createBuffer gl
    GL.bindBuffer gl GL.ARRAY_BUFFER (Just vertexBuffer)
    bufferDataFloat gl
        GL.ARRAY_BUFFER particleGeometryData GL.STATIC_DRAW

    translationBuffer <- GL.createBuffer gl
    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
    bufferDataSizeOnly gl GL.ARRAY_BUFFER
        (fromIntegral $ Model.maxParticlesNum * 8) GL.DYNAMIC_DRAW

    vertexShader <- buildShader gl GL.VERTEX_SHADER vertexShaderSource
    fragmentShader <- buildShader
        gl GL.FRAGMENT_SHADER fragmentShaderSource
    program <- buildProgram gl vertexShader fragmentShader
    GL.useProgram gl (Just program)

    modelUniform <- GL.getUniformLocation
        gl (Just program) ("u_model" :: Text)
    GL.uniformMatrix4fv gl (Just modelUniform) False $
        listFromMatrix $ modelMatrix Model.particleRadius

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
    liftIO $ putStrLn $ "Error: " ++ show err

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
       -> Performance
       -> Buffer.MutableBuffer
       -> (Particles2, ProjectionMatrixList)
       -> JSM ()
render GLContext{..}
       GLObjects{..}
       performance
       particlesTransformationsStagingBuffer
       (particles, projectionMatrix) = do
    Performance.mark performance ("render-begin" :: Text)

    stageBufferF32 <- ghcjsPure $
            Buffer.getFloat32Array particlesTransformationsStagingBuffer
    stageBufferJS <- ghcjsPure . jsval $ stageBufferF32
    stageBuffer <- fromJSVal stageBufferJS :: JSM (Maybe ArrayBuffer)
    VU.iforM_ particles $ \ pIndex p -> do
        let offset = pIndex * 2
        let px = p ^. position . _x
        let py = p ^. position . _y
        setIndex offset px stageBufferF32
        setIndex (offset + 1) py stageBufferF32

    GL.uniformMatrix4fv gl (Just projectionUniform) False projectionMatrix
    GL.clear gl GL.COLOR_BUFFER_BIT

    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
    Performance.mark performance ("render-bufferSubDataFloat-begin" :: Text)
    GL.bufferSubData gl GL.ARRAY_BUFFER 0 stageBuffer
    Performance.mark performance ("render-bufferSubDataFloat-end" :: Text)
    GL.bindBuffer gl GL.ARRAY_BUFFER Nothing

    Performance.mark performance ("render-drawArraysInstancedANGLE-begin" :: Text)
    drawArraysInstancedANGLE angleInstancedArrays
        GL.TRIANGLE_FAN 0 (particleGeometryNumSlices + 2) $ VU.length particles
    Performance.mark performance ("render-end" :: Text)

    Performance.measure performance
        ("render" :: Text)
        (Just ("render-begin" :: Text))
        (Just ("render-end" :: Text))
    Performance.measure performance
        ("render-bufferSubDataFloat" :: Text)
        (Just ("render-bufferSubDataFloat-begin" :: Text))
        (Just ("render-bufferSubDataFloat-end" :: Text))
    Performance.measure performance
        ("render-drawArraysInstancedANGLE" :: Text)
        (Just ("render-drawArraysInstancedANGLE-begin" :: Text))
        (Just ("render-end" :: Text))

buildShader :: WebGLRenderingContext -> GLenum -> String -> JSM WebGLShader
buildShader gl shaderType sourceCode = do
    shader <- GL.createShader gl shaderType
    GL.shaderSource gl (Just shader) sourceCode
    GL.compileShader gl $ Just shader
    infoLog <- GL.getShaderInfoLogUnsafe gl $ Just shader
    liftIO $ putStrLn $ "Shader compilation log: " ++ infoLog
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
    infoLog <- GL.getProgramInfoLogUnsafe gl (Just program)
    liftIO $ putStrLn $ "Program log: " ++ infoLog
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

particleGeometryData :: [Double]
particleGeometryData = concat_ $ center : perimeter where
    concat_ ps = [n | (x, y) <- ps, n <- [x, y]]
    center = (0.0, 0.0)
    perimeter = point <$> [0 .. numSlices]
    point index = (cos a, sin a) where
        a = fromIntegral index * 2.0 * pi / fromIntegral numSlices
    numSlices = particleGeometryNumSlices

particleGeometryNumSlices :: Int
particleGeometryNumSlices = 7

{-# INLINE listFromMatrix #-}
listFromMatrix :: M44 a -> [a]
listFromMatrix = concatMap Foldable.toList . Foldable.toList

trackCanvasSize :: ( Monad m
                   , RX.MonadHold t m
                   , MonadJSM (RX.Performable m)
                   , RX.MonadSample t (RX.Performable m)
                   , RX.PerformEvent t m
                   , RX.TriggerEvent t m )
                => HTMLCanvasElement
                -> RX.Event t a
                -> m ( RX.Dynamic t (Maybe CanvasSize)
                     , RX.Event t (Maybe CanvasSize) )
trackCanvasSize canvas event = do
    (eSize, triggerSize) <- RX.newTriggerEvent
    dSize <- RX.holdDyn Nothing eSize
    let bSize = RX.current dSize
    let performer = \_ -> do
            currentSize <- RX.sample bSize
            liftJSM $ do
                width' <- floor <$> Element.getClientWidth canvas
                height' <- floor <$> Element.getClientHeight canvas
                let triggerEvent = do
                        Canvas.setWidth canvas $ fromIntegral width'
                        Canvas.setHeight canvas $ fromIntegral height'
                        liftIO $ triggerSize $
                            Just $ CanvasSize width' height'
                        return ()
                case currentSize of
                    Just CanvasSize{..} ->
                        when (width' /= width || height' /= height)
                            triggerEvent
                    Nothing -> triggerEvent
    RX.performEvent_ $ performer <$> event
    return (dSize, eSize)

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
        width' = fromIntegral width / 4.0
        height' = fromIntegral height / 4.0

modelMatrix :: Double -> M44 Double
modelMatrix radius = scaled $ V4 radius radius 1.0 1.0

boundingBoxFromCanvasSize :: CanvasSize -> BoundingBox
boundingBoxFromCanvasSize CanvasSize{..} =
    makeBoundingBox 0.0 (fromIntegral width) 0.0 (fromIntegral height)
