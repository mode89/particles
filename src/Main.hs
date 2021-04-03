module Main where

import Control.Lens ((^.))
import GHCJS.DOM (currentDocumentUnchecked, inAnimationFrame')
import qualified GHCJS.DOM.Element as Element
import qualified GHCJS.DOM.DOMRect as DOMRect
import GHCJS.DOM.Document
    ( createElement
    , getBodyUnsafe
    , getDocumentElementUnsafe )
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.Types
    ( ArrayBuffer(..)
    , Element(..)
    , GLenum
    , HTMLCanvasElement(..)
    , JSM
    , JSVal
    , pToJSVal
    , RenderingContext(..)
    , toJSVal
    , ToJSVal
    , unsafeCastTo
    , WebGLProgram
    , WebGLRenderingContext(..)
    , WebGLShader )
import qualified GHCJS.DOM.WebGLRenderingContextBase as GL
import Language.Javascript.JSaddle (js, jsf, jsg, jss, new)

main :: JSM ()
main = do
    document <- currentDocumentUnchecked

    html <- getDocumentElementUnsafe document
    styleSet html "display" "flex"
    styleSet html "flex-direction" "column"
    styleSet html "height" "100%"
    styleSet html "margin" "0"
    body <- getBodyUnsafe document
    styleSet body "display" "flex"
    styleSet body "flex-direction" "column"
    styleSet body "height" "100%"
    styleSet body "margin" "0"

    canvas <- createElement document "canvas"
        >>= unsafeCastTo HTMLCanvasElement
    styleSet canvas "width" "100%"
    styleSet canvas "height" "100%"
    appendChild body canvas
    context <- Canvas.getContextUnsafe canvas "webgl" ([] :: [()])
        >>= unsafeCastTo WebGLRenderingContext

    GL.clearColor context 0.5 0.5 0.5 1.0

    vertexBuffer <- GL.createBuffer context
    GL.bindBuffer context GL.ARRAY_BUFFER (Just vertexBuffer)
    toJSVal context ^. jsf "bufferData"
        ( GL.ARRAY_BUFFER :: GLenum
        , new (jsg "Float32Array")
            [[ -1.0, -1.0
             , -1.0,  1.0
             ,  1.0,  1.0 :: Float ]]
        , GL.STATIC_DRAW :: GLenum )

    vertexShader <- buildShader context GL.VERTEX_SHADER
        "attribute vec2 a_position; \
        \void main() { \
        \    gl_Position = vec4(a_position, 0.0, 1.0); \
        \}"
    fragmentShader <- buildShader context GL.FRAGMENT_SHADER
        "void main() { \
        \   gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0); \
        \}"
    program <- buildProgram context vertexShader fragmentShader
    GL.useProgram context (Just program)

    positionAttributeLocation <- fromIntegral <$>
        GL.getAttribLocation context (Just program) "a_position"
    GL.enableVertexAttribArray context positionAttributeLocation
    GL.vertexAttribPointer context
        positionAttributeLocation 2 GL.FLOAT False 0 0

    err <- GL.getError context
    putStrLn $ "Error: " ++ show err

    inAnimationFrame' $ tick context canvas

    return ()

tick :: WebGLRenderingContext -> HTMLCanvasElement -> Double -> JSM ()
tick context canvas timestamp = do
    rect <- Element.getBoundingClientRect canvas
    width <- floor <$> DOMRect.getWidth rect
    height <- floor <$> DOMRect.getHeight rect
    Canvas.setWidth canvas (fromIntegral width)
    Canvas.setHeight canvas (fromIntegral height)
    GL.viewport context 0 0 width height
    GL.clear context GL.COLOR_BUFFER_BIT
    GL.drawArrays context GL.TRIANGLES 0 3
    inAnimationFrame' $ tick context canvas
    return ()

styleSet :: (ToJSVal a, ToJSVal b) => a -> String -> b -> JSM ()
styleSet element property value =
    toJSVal element ^. js "style" ^. jss property value

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
