{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import qualified Data.List as L
import Data.Text (Text)
import GHCJS.DOM (inAnimationFrame')
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.DOMRect as DOMRect
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import GHCJS.DOM.Types
    ( GLenum
    , HTMLCanvasElement(..)
    , JSM
    , liftJSM
    , RenderingContext(..)
    , toJSVal
    , unsafeCastTo
    , WebGLProgram
    , WebGLRenderingContext(..)
    , WebGLShader )
import qualified GHCJS.DOM.WebGLRenderingContextBase as GL
import Language.Javascript.JSaddle (jsf, jsg, new)
import Linear.Matrix (identity, M44)
import Linear.Projection (ortho)
import Reflex (newTriggerEvent, performEvent_)
import Reflex.Dom (el', _element_raw, mainWidgetWithCss, text)

main :: JSM ()
main = mainWidgetWithCss style $ do
    (eRender, triggerRender) <- newTriggerEvent
    (canvas, _) <- el' "canvas" $ text ""
    (context, canvasRaw) <- liftJSM $ do
        canvasRaw <- unsafeCastTo HTMLCanvasElement $ _element_raw canvas
        context <- Canvas.getContextUnsafe
            canvasRaw ("webgl" :: Text) ([] :: [()])
                >>= unsafeCastTo WebGLRenderingContext

        GL.clearColor context 0.5 0.5 0.5 1.0

        vertexBuffer <- GL.createBuffer context
        GL.bindBuffer context GL.ARRAY_BUFFER (Just vertexBuffer)
        toJSVal context ^. jsf ("bufferData" :: Text)
            ( GL.ARRAY_BUFFER :: GLenum
            , new (jsg ("Float32Array" :: Text))
                [[ -1.0, -1.0
                 , -1.0,  1.0
                 ,  1.0,  1.0 :: Float ]]
            , GL.STATIC_DRAW :: GLenum )

        vertexShader <- buildShader
            context GL.VERTEX_SHADER vertexShaderSource
        fragmentShader <- buildShader
            context GL.FRAGMENT_SHADER fragmentShaderSource
        program <- buildProgram context vertexShader fragmentShader
        GL.useProgram context (Just program)

        positionAttributeLocation <- fromIntegral <$> GL.getAttribLocation
            context (Just program) ("a_position" :: Text)
        GL.enableVertexAttribArray context positionAttributeLocation
        GL.vertexAttribPointer context
            positionAttributeLocation 2 GL.FLOAT False 0 0

        let matrix = ortho (-2.0) 2.0 (-2.0) 2.0 (-1.0) 1.0 :: M44 Double
        modelViewProjectionUniform <- GL.getUniformLocation
            context (Just program) ("u_modelViewProjection" :: Text)
        GL.uniformMatrix4fv context
            (Just modelViewProjectionUniform) False $ listFromMatrix matrix

        err <- GL.getError context
        putStrLn $ "Error: " ++ show err

        let loop = \timestamp -> do
                        triggerRender timestamp
                        inAnimationFrame' loop
                        return ()
        inAnimationFrame' loop

        return (context, canvasRaw)

    performEvent_ $ liftIO . (tick context canvasRaw) <$> eRender

    return ()

vertexShaderSource :: String
vertexShaderSource = L.intercalate "\n"
    [ "attribute vec2 a_position;"
    , "uniform mat4 u_modelViewProjection;"
    , "void main() {"
    , "    gl_Position ="
    , "        vec4(a_position, 0.0, 1.0) * u_modelViewProjection;"
    , "}" ]

fragmentShaderSource :: String
fragmentShaderSource = L.intercalate "\n"
    [ "void main() {"
    , "   gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);"
    , "}" ]

tick :: WebGLRenderingContext
     -> HTMLCanvasElement
     -> Double
     -> IO ()
tick context canvas timestamp = do
    rect <- Element.getBoundingClientRect canvas
    width <- floor <$> DOMRect.getWidth rect
    height <- floor <$> DOMRect.getHeight rect
    Canvas.setWidth canvas (fromIntegral width)
    Canvas.setHeight canvas (fromIntegral height)
    GL.viewport context 0 0 width height
    GL.clear context GL.COLOR_BUFFER_BIT
    GL.drawArrays context GL.TRIANGLES 0 3

buildShader :: WebGLRenderingContext -> GLenum -> String -> JSM WebGLShader
buildShader context shaderType sourceCode = do
    shader <- GL.createShader context shaderType
    GL.shaderSource context (Just shader) sourceCode
    GL.compileShader context $ Just shader
    log <- GL.getShaderInfoLogUnsafe context $ Just shader
    putStrLn $ "Shader compilation log: " ++ log
    return shader

buildProgram :: WebGLRenderingContext
             -> WebGLShader
             -> WebGLShader
             -> JSM WebGLProgram
buildProgram context vertexShader fragmentShader = do
    program <- GL.createProgram context
    GL.attachShader context (Just program) (Just vertexShader)
    GL.attachShader context (Just program) (Just fragmentShader)
    GL.linkProgram context (Just program)
    log <- GL.getProgramInfoLogUnsafe context (Just program)
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

listFromMatrix :: M44 a -> [a]
listFromMatrix = concat . fmap Foldable.toList . Foldable.toList
