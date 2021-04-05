{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
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
import qualified GHCJS.DOM.DOMRect as DOMRect
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import GHCJS.DOM.Types
    ( ANGLEInstancedArrays(..)
    , GLenum
    , GObject(..)
    , HTMLCanvasElement(..)
    , JSM
    , liftJSM
    , RenderingContext(..)
    , toJSVal
    , uncheckedCastTo
    , unsafeCastTo
    , WebGLProgram
    , WebGLRenderingContext(..)
    , WebGLShader
    , WebGLUniformLocation )
import qualified GHCJS.DOM.WebGLRenderingContextBase as GL
import Language.Javascript.JSaddle (jsf, jsg, new)
import Linear.Matrix (identity, M44)
import Linear.Projection (ortho)
import Reflex (newTriggerEvent, performEvent_)
import Reflex.Dom (el', _element_raw, mainWidgetWithCss, text)

data App = App
    { gl :: WebGLRenderingContext
    , angleInstancedArrays :: ANGLEInstancedArrays }

main :: JSM ()
main = mainWidgetWithCss style $ do
    (eRender, triggerRender) <- newTriggerEvent
    (canvas, _) <- el' "canvas" $ text ""
    canvasRaw <- liftJSM . unsafeCastTo HTMLCanvasElement $
        _element_raw canvas
    (gl, angleInstancedArrays) <- liftJSM $ do
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
        bufferDataFloat gl
            GL.ARRAY_BUFFER [0.0, 0.0, 1.0, 1.0] GL.STATIC_DRAW

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
                        triggerRender timestamp
                        inAnimationFrame' loop
                        return ()
        inAnimationFrame' loop

        return (gl, angleInstancedArrays)

    let app = App { gl = gl
                  , angleInstancedArrays = angleInstancedArrays }

    performEvent_ $ liftIO . (tick app canvasRaw) <$> eRender

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

tick :: App
     -> HTMLCanvasElement
     -> Double
     -> IO ()
tick App{..} canvas timestamp = do
    rect <- Element.getBoundingClientRect canvas
    width <- floor <$> DOMRect.getWidth rect
    height <- floor <$> DOMRect.getHeight rect
    Canvas.setWidth canvas (fromIntegral width)
    Canvas.setHeight canvas (fromIntegral height)
    GL.viewport gl 0 0 width height
    GL.clear gl GL.COLOR_BUFFER_BIT
    drawArraysInstancedANGLE angleInstancedArrays
        GL.TRIANGLE_FAN 0 (particleGeometryNumSlices + 2) 2

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
bufferDataFloat gl binding values usage = do
    toJSVal gl ^. jsf ("bufferData" :: Text)
        ( binding
        , new (jsg ("Float32Array" :: Text)) [ values ]
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
