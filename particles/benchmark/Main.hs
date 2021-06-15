{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Monad (foldM)
import Criterion.Main
import Criterion.Types
import Debug.Trace
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Particles.Model as P
import qualified Particles.Model2 as PM2
import qualified Particles.Types as P

#ifdef __GHCJS__
import Control.DeepSeq (NFData, rnf, rwhnf)
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Document as Document
import qualified GHCJS.DOM.HTMLCanvasElement as Canvas
import qualified GHCJS.DOM.Node as Node
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.WebGLRenderingContextBase as GL
import qualified Particles.UI.GL as PGL

data DOMEnvironment = DOMEnvironment {
    gl :: DOM.WebGLRenderingContext,
    bufferData :: [Double]
    }

instance NFData DOMEnvironment where rnf = rwhnf
#endif

main :: IO ()
main = do
    let bbox = P.BoundingBox 0 1920 0 1080
    let !particles = P.initialParticles bbox
    let !particles2 = PM2.initialParticles bbox
    containers <- prepareContainers
    defaultMain [
#ifdef __GHCJS__
        env prepareDOMEnvironment bgroupDOM,
#endif
        bench "update-particles" $ nfIO $ foldM (\ps _ -> do
                return $ P.updateParticles ps bbox
            ) particles [1..100],
        bench "updateParticles2" $ nfIO $ foldM (\ps _ -> do
                return $ PM2.updateParticles bbox ps
            ) particles2 [1..100],
        bgroupContainers containers
        ]

bgroupContainers :: ( [Double]
                    , V.Vector Double
                    , VU.Vector Double
                    , VM.IOVector Double
                    , VUM.IOVector Double )
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
            (\i x -> VM.write vm i (x + 1.0 :: Double)) vm
        , bench "mutable-unboxed-vector" $ nfIO $ VUM.imapM_
            (\i x -> VUM.write vum i (x + 1.0 :: Double)) vum
        ]
    , bgroup "continuous-update" [
          bench "immutable" $ nfIO $ foldM (\v _ -> do
                return $ VU.map nextX v
            ) vu [1..1000]
        , bench "mutable" $ nfIO $ foldM (\v _ -> do
                VUM.imapM_ (\i _ -> do
                        VUM.modify v nextX i
                    ) v
                return v
            ) vum [1..1000]
        , bench "thaw-freeze" $ nfIO $
            foldM (\v _ -> do
                vm <- VU.thaw v
                VUM.imapM_ (\i x -> VUM.write vm i (nextX x)) vm
                VU.freeze vm
            ) vu [1..1000]
        ]
    , bgroup "fusion" [
          bench "list" $ whnf (sum . map (\ x -> x + 1)) l
        , bench "vector" $ whnf (VU.sum . VU.map (\ x -> x + 1)) vu
        ]
    ]

nextX !x = x + sqrt x + sqrt (x + 1) + sqrt (x + 2) + sqrt (x + 3)

prepareContainers = do
    let l = [0 .. 20000 :: Double]
    let v = V.fromList [0 .. 20000 :: Double]
    let vu = VU.fromList [0 .. 20000 :: Double]
    vm <- VM.generate 20000 fromIntegral
    vum <- VUM.generate 20000 fromIntegral
    return (l, v, vu, vm, vum)

#ifdef __GHCJS__
bgroupDOM ~(DOMEnvironment gl bufferData) = bgroup "dom" [
    bench "bufferSubDataFloat" $ nfIO $
        PGL.bufferSubDataFloat gl GL.ARRAY_BUFFER 0 bufferData
    ]

prepareDOMEnvironment :: IO DOMEnvironment
prepareDOMEnvironment = do
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
    return $ DOMEnvironment gl bufferData
#endif
