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
import qualified GHCJS.DOM.DOMRect as DOMRect
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
import Linear.Matrix (identity, M44)
import Linear.Projection (ortho)
import Linear.V2 (V2(..), _x, _y)
import qualified Reflex as RX
import Reflex.Dom (el', _element_raw, mainWidgetWithCss, text)

data App = App
    { canvas :: HTMLCanvasElement
    , gl :: WebGLRenderingContext
    , angleInstancedArrays :: ANGLEInstancedArrays
    , translationBuffer :: WebGLBuffer }

data State = State
    { particles :: Particles }

type Particles = [Particle]

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
    (gl, angleInstancedArrays, translationBuffer) <- liftJSM $ do
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

        let matrix = ortho (-2.0) 2.0 (-2.0) 2.0 (-1.0) 1.0 :: M44 Double
        modelViewProjectionUniform <- GL.getUniformLocation
            gl (Just program) ("u_modelViewProjection" :: Text)
        GL.uniformMatrix4fv gl
            (Just modelViewProjectionUniform) False $ listFromMatrix matrix

        err <- GL.getError gl
        putStrLn $ "Error: " ++ show err

        let loop = \timestamp -> do
                        triggerAnimationFrame timestamp
                        inAnimationFrame' loop
                        return ()
        inAnimationFrame' loop

        return (gl, angleInstancedArrays, translationBuffer)

    let app = App { canvas = canvasRaw, .. }

    now <- liftIO getCurrentTime
    eTick <- RX.tickLossy 0.04 now

    bState <- RX.accumB updateState initialState eTick
    let eRender = RX.attach bState eAnimationFrame

    RX.performEvent_ $ liftIO . (render app) <$> eRender

    (bCanvasSize, eCanvasSizeChanged) <- trackCanvasSize
        canvasRaw eAnimationFrame

    return ()

vertexShaderSource :: String
vertexShaderSource = L.intercalate "\n"
    [ "attribute vec2 a_position;"
    , "attribute vec2 a_translation;"
    , "uniform mat4 u_modelViewProjection;"
    , "void main() {"
    , "    gl_Position ="
    , "        u_modelViewProjection *"
    , "        vec4(a_position + a_translation, 0.0, 1.0);"
    , "}" ]

fragmentShaderSource :: String
fragmentShaderSource = L.intercalate "\n"
    [ "void main() {"
    , "   gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);"
    , "}" ]

render :: App
     -> (State, Double)
     -> IO ()
render App{..} (State{..}, timestamp) = do
    rect <- Element.getBoundingClientRect canvas
    width <- floor <$> DOMRect.getWidth rect
    height <- floor <$> DOMRect.getHeight rect
    Canvas.setWidth canvas (fromIntegral width)
    Canvas.setHeight canvas (fromIntegral height)
    GL.viewport gl 0 0 width height
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
    [ "html, body {"
    , "    display: flex;"
    , "    flex-direction: column;"
    , "    height: 100%;"
    , "    margin: 0;"
    , "}"
    , "canvas {"
    , "    width: 100%;"
    , "    height: 100%;"
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

initialState :: State
initialState = State {
      particles =
        [ Particle { position = V2 0.0 0.0 }
        , Particle { position = V2 1.0 1.0 } ]
    }

updateState :: State -> RX.TickInfo -> State
updateState State{..} RX.TickInfo{..} = State {
        particles = shift <$> particles
    } where
        shift particle@Particle{..} = particle {
            position = V2 (position ^. _x + 0.01) (position ^. _y)
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
                rect <- Element.getBoundingClientRect canvas
                width' <- floor <$> DOMRect.getWidth rect
                height' <- floor <$> DOMRect.getHeight rect
                if width' /= width || height' /= height then do
                    Canvas.setWidth canvas (fromIntegral width')
                    Canvas.setHeight canvas (fromIntegral height')
                    triggerSizeChanged $ CanvasSize width' height'
                    return ()
                else
                    return ()
        ) <$> event
    return (bSize, eSizeChanged)
