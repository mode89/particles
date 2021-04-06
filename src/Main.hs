{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.JSString as JSS
import Data.Text (Text)
import Data.Time (getCurrentTime)
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
import Linear.Projection (ortho)
import Linear.V2 (V2(..), _x, _y)
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Vector (scaled)
import qualified Reflex as RX
import Reflex.Dom (el', _element_raw, mainWidgetWithCss, text)

data App = App
    { canvas :: HTMLCanvasElement
    , gl :: WebGLRenderingContext
    , angleInstancedArrays :: ANGLEInstancedArrays
    , translationBuffer :: WebGLBuffer
    , projectionUniform :: WebGLUniformLocation }

type Particles = [Particle]
type ProjectionMatrix = M44 Double

data Particle = Particle
    { position :: V2 Float
    , velocity :: V2 Float }

data CanvasSize = CanvasSize
    { width :: Int
    , height :: Int } deriving (Eq, Show)

main :: JSM ()
main = mainWidgetWithCss style $ do
    (eAnimationFrame, triggerAnimationFrame) <- RX.newTriggerEvent
    (canvasEl, _) <- el' "canvas" $ text ""
    canvasRaw <- liftJSM . unsafeCastTo HTMLCanvasElement $
        _element_raw canvasEl
    app <- liftJSM $ do
        gl <- Canvas.getContextUnsafe
            canvasRaw ("webgl" :: Text) ([] :: [()])
                >>= unsafeCastTo WebGLRenderingContext

        GL.clearColor gl 0.5 0.5 0.5 1.0

        angleInstancedArrays <- uncheckedCastTo ANGLEInstancedArrays <$>
            GL.getExtensionUnsafe gl ("ANGLE_instanced_arrays" :: Text)

        vertexBuffer <- GL.createBuffer gl
        GL.bindBuffer gl GL.ARRAY_BUFFER (Just vertexBuffer)
        bufferDataFloat gl
            GL.ARRAY_BUFFER particleGeometryData GL.STATIC_DRAW

        translationBuffer <- GL.createBuffer gl
        GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
        bufferDataSizeOnly gl GL.ARRAY_BUFFER
            (fromIntegral $ particlesNum * 8) GL.DYNAMIC_DRAW

        vertexShader <- buildShader gl GL.VERTEX_SHADER vertexShaderSource
        fragmentShader <- buildShader
            gl GL.FRAGMENT_SHADER fragmentShaderSource
        program <- buildProgram gl vertexShader fragmentShader
        GL.useProgram gl (Just program)

        modelUniform <- GL.getUniformLocation
            gl (Just program) ("u_model" :: Text)
        GL.uniformMatrix4fv gl (Just modelUniform) False $
            listFromMatrix $ modelMatrix particleRadius

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

        let loop = \timestamp -> do
                        triggerAnimationFrame timestamp
                        inAnimationFrame' loop
                        return ()
        inAnimationFrame' loop

        return App { canvas = canvasRaw, .. }

    (bCanvasSize, eCanvasSizeChanged) <- trackCanvasSize
        canvasRaw eAnimationFrame

    now <- liftIO getCurrentTime
    eTick <- RX.tickLossy 0.04 now

    let bProjectionMatrix = projectionMatrixFromCanvasSize <$> bCanvasSize

    bParticles <- RX.accumB updateParticles initialParticles eTick
    let eRender = RX.tag ((,) <$> bParticles <*> bProjectionMatrix) eAnimationFrame

    RX.performEvent_ $
        liftIO . updateViewportSize (gl app) <$> eCanvasSizeChanged
    RX.performEvent_ $ liftIO . (render app) <$> eRender

    return ()

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

render :: App
     -> (Particles, ProjectionMatrix)
     -> IO ()
render App{..} (particles, projectionMatrix) = do
    GL.uniformMatrix4fv gl (Just projectionUniform) False $
        listFromMatrix projectionMatrix
    GL.clear gl GL.COLOR_BUFFER_BIT

    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
    bufferSubDataFloat gl GL.ARRAY_BUFFER 0 translations
    GL.bindBuffer gl GL.ARRAY_BUFFER Nothing

    drawArraysInstancedANGLE angleInstancedArrays
        GL.TRIANGLE_FAN 0 (particleGeometryNumSlices + 2) 2
    where
        translations = [n | p <- particles, n <- [ x p, y p ]]
        x p = position p ^. _x
        y p = position p ^. _y

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

particlesNum :: Int
particlesNum = 2

initialParticles :: Particles
initialParticles =
    [ Particle { position = V2 0.0 0.0 }
    , Particle { position = V2 1.0 1.0 } ]

updateParticles :: Particles -> RX.TickInfo -> Particles
updateParticles particles RX.TickInfo{..} = shift <$> particles where
    shift particle@Particle{..} = particle {
        position = V2 (position ^. _x + 1.0) (position ^. _y)
    }

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

updateViewportSize :: WebGLRenderingContext -> CanvasSize -> JSM ()
updateViewportSize gl CanvasSize{..} =
    GL.viewport gl 0 0 (fromIntegral width) (fromIntegral height)

projectionMatrixFromCanvasSize :: CanvasSize -> M44 Double
projectionMatrixFromCanvasSize CanvasSize{..} =
    transpose $ projection !*! modelView
    where
        projection = ortho
            (-halfWidth) halfWidth (-halfHeight) halfHeight (-1.0) 1.0
        modelView = mkTransformationMat
            identity $ V3 (-halfWidth) (-halfHeight) 0.0
        halfWidth = 0.5 * (fromIntegral width)
        halfHeight = 0.5 * (fromIntegral height)

modelMatrix :: Double -> M44 Double
modelMatrix radius = scaled $ V4 radius radius 1.0 1.0

particleRadius :: Double
particleRadius = 10.0
