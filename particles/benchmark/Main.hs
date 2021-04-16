{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import Criterion.Types
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
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
    containers <- prepareContainers
    defaultMain [
          bench "bufferSubDataFloat" $ nfIO $
            PGL.bufferSubDataFloat gl GL.ARRAY_BUFFER 0 bufferData
        , bgroupContainers containers
        ]

bgroupContainers :: ( [Float]
                    , V.Vector Float
                    , VU.Vector Float
                    , VM.IOVector Float
                    , VUM.IOVector Float )
                 -> Benchmark
bgroupContainers (l, v, vu, vm, vum) = bgroup "containers" [
      bgroup "sum" [
          bench "list" $ whnf sum l
        , bench "vector" $
            nf (V.foldl (+) 0.0) v
        , bench "unboxed-vector"
            $ nf (VU.foldl (+) 0.0) vu
        ]
    , bgroup "update" [
          bench "list" $
            nf (sum . map (+1.0)) l
        , bench "vector" $
            nf (sum . V.map (+1.0)) v
        , bench "unboxed-vector" $
            nf (VU.foldl (+) 0.0 . VU.map (+1.0)) vu
        ]
    , bgroup "update-inplace" [
          bench "mutable-vector" $ nfIO $ VM.imapM_
            (\i x -> VM.write vm i (x + 1.0 :: Float)) vm
        , bench "mutable-unboxed-vector" $ nfIO $ VUM.imapM_
            (\i x -> VUM.write vum i (x + 1.0 :: Float)) vum
        ]
    ]

prepareContainers = do
    let l = [0 .. 20000 :: Float]
    let v = V.fromList [0 .. 20000 :: Float]
    let vu = VU.fromList [0 .. 20000 :: Float]
    vm <- VM.generate 20000 fromIntegral
    vum <- VUM.generate 20000 fromIntegral
    return (l, v, vu, vm, vum)
