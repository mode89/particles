{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Monad (foldM)
import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Particles.Map2 as P2
import qualified Particles.Map3 as P3
import qualified Particles.Map3.Types as P3
import qualified Particles.Model as P
import qualified Particles.Model2 as P2
import qualified Particles.Model3 as P3
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

kBucketCapacity = 64

main :: IO ()
main = do
    let !bbox = P.makeBoundingBox 0 1920 0 1080
    let !ps1 = P.initialParticles 500 bbox
    let !ps2 = P2.initialParticles 500 bbox
    let !psSorted = sortParticles bbox ps2
    let !pmap3 = P3.make kBucketCapacity 50 bbox ps2
    let !model3 = P3.initialState kBucketCapacity 50 bbox 500
    containers <- prepareContainers
    defaultMain [
#ifdef __GHCJS__
        env prepareDOMEnvironment bgroupDOM,
#endif
        bgroupUpdate bbox ps1 ps2 model3,
        bgroupMap bbox psSorted pmap3,
        bgroupContainers containers
        ]

bgroupUpdate
    :: P.BoundingBox
    -> P.Particles
    -> P.Particles2
    -> P3.ModelState
    -> Benchmark
bgroupUpdate !bbox !ps1 !ps2 !model3 = bgroup "updateParticles" [
      bench "v3" $ nfIO $ foldM (\ model@(P3.ModelState !a !b !c)  _ -> do
            return $ P3.unsafeUpdateState kBucketCapacity 50 bbox model
        ) model3 [1 :: Int .. 10]
    , bench "v2" $ nfIO $ foldM (\ !ps _ -> do
            return $ P2.updateParticles kBucketCapacity 50 bbox ps
        ) ps2 [1 :: Int .. 100]
    , bench "v1" $ nfIO $ foldM (\ !ps _ -> do
            return $ P.updateParticles ps bbox
        ) ps1 [1 :: Int .. 100]
    ]

bgroupMap
    :: P.BoundingBox
    -> P.Particles2
    -> P3.ParticlesMap
    -> Benchmark
bgroupMap !bbox !ps !pmap3 = bgroup "map" [
      bench "v2/make" $ whnf (P2.make kBucketCapacity 50 bbox) ps
    , bench "v3/make" $ whnf (P3.make kBucketCapacity 50 bbox) ps
    , bench "v3/unsafeUpdate" $ whnf (P3.unsafeUpdate kBucketCapacity 50 bbox ps) pmap3
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

sortParticles :: P.BoundingBox -> P.Particles2 -> P.Particles2
sortParticles !bbox !ps
    = VU.concatMap bucketParticles
    $ VU.enumFromN 0 (VU.length $ P3.mapBucketsSizes pmap)
    where
        pmap = P3.make kBucketCapacity 50 bbox ps
        bucketParticles :: Int -> P.Particles2
        bucketParticles bIndex = VU.map (ps VU.!) bucket where
            bSize = P3.mapBucketsSizes pmap VU.! bIndex
            bucket = VU.slice (bIndex * kBucketCapacity)
                bSize (P3.mapBucketsStorage pmap)
