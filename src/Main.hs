module Main where

import Control.Lens ((^.))
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Document
    ( createElement
    , getBodyUnsafe
    , getDocumentElementUnsafe )
import GHCJS.DOM.HTMLCanvasElement (getContextUnsafe)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.Types
import qualified GHCJS.DOM.WebGLRenderingContextBase as GL
import Language.Javascript.JSaddle (js, jss, MakeObject)

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
    context <- getContextUnsafe canvas "webgl" ([] :: [()])
        >>= unsafeCastTo WebGLRenderingContext

    GL.clearColor context 0.5 0.5 0.5 1.0
    GL.clear context GL.COLOR_BUFFER_BIT

    return ()

styleSet :: (ToJSVal a, ToJSVal b) => a -> String -> b -> JSM ()
styleSet element property value =
    toJSVal element ^. js "style" ^. jss property value
