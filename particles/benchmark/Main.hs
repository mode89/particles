{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Debug.Trace
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.WebGLRenderingContextBase as GL
import qualified Particles.UI.GL as PGL

main :: IO ()
main = do
    document <- DOM.currentDocumentUnchecked
    body <- Document.getBodyUnchecked document
    canvas <- do
        element <- Document.createElement document "canvas"
        canvasElement <- DOM.unsafeCastTo DOM.HTMLCanvasElement element
        return canvasElement
    Node.appendChild_ body canvas
    gl <- do
        context <- Canvas.getContextUnsafe canvas "webgl" ([] :: [Double])
        context' <- DOM.unsafeCastTo DOM.WebGLRenderingContext context
        return context'
    buffer <- GL.createBuffer gl
    GL.bindBuffer gl GL.ARRAY_BUFFER (Just buffer)
    PGL.bufferDataSizeOnly gl GL.ARRAY_BUFFER 16000000 GL.DYNAMIC_DRAW
    let !bufferData = [0 .. 20000]
    defaultMain [
          bench "bufferSubDataFloat" $ nfIO $
            PGL.bufferSubDataFloat gl GL.ARRAY_BUFFER 0 bufferData
        , bench "sum" $ whnf sum bufferData
        ]
