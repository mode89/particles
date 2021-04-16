{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens ((^.), (.~))
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.ByteString as BS
import qualified Data.Foldable as Foldable
import Data.Int (Int32)
import qualified Data.List as L
import qualified Data.JSString as JSS
import Data.Text (Text)
import Data.Witherable (catMaybes)
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
import Language.Javascript.JSaddle (jsf, jsg, JSM, liftJSM, MonadJSM, new)
import Linear.Matrix ((!*!), identity, mkTransformationMat, M44, transpose)
import Linear.Projection (ortho)
import Linear.V2 (V2(..), _x, _y)
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Linear.Vector (scaled)
import qualified Particles.Model as Model
import Particles.Types
import qualified Reflex as RX
import Reflex ((<@))
import Reflex.Dom (el', _element_raw, mainWidgetWithCss, text)

data GLContext = GLContext
    { gl :: WebGLRenderingContext
    , angleInstancedArrays :: ANGLEInstancedArrays }

data GLObjects = GLObjects
    { translationBuffer :: WebGLBuffer
    , projectionUniform :: WebGLUniformLocation }

type ProjectionMatrix = M44 Double

data CanvasSize = CanvasSize
    { width :: Int
    , height :: Int } deriving (Eq, Show)

main :: IO ()
main = mainWidgetWithCss style $ do
    (canvasEl, _) <- el' "canvas" $ text ""
    canvasRaw <- liftJSM . unsafeCastTo HTMLCanvasElement $
        _element_raw canvasEl
    glContext <- liftJSM $ getGLContext canvasRaw
    glObjects <- liftJSM $ initGL glContext

    eTick <- RX.tickLossyFromPostBuildTime $ realToFrac Model.tickInterval

    (bCanvasSize, eCanvasSizeChanged) <- trackCanvasSize canvasRaw eTick

    let bProjectionMatrix =
            fmap projectionMatrixFromCanvasSize <$> bCanvasSize

    let bBoundingBox = fmap boundingBoxFromCanvasSize <$> bCanvasSize

    bParticles <- RX.accumB Model.updateParticles Model.initialParticles $
        RX.tagMaybe bBoundingBox eTick
    let bRenderingParams = RX.ffor2 bParticles bProjectionMatrix $
            \ps pm -> ((,) ps) <$> pm
    let eRender = RX.tagMaybe bRenderingParams eTick

    RX.performEvent_ $ liftJSM . updateViewportSize glContext <$>
        catMaybes eCanvasSizeChanged
    RX.performEvent_ $ liftJSM . (render glContext glObjects) <$> eRender

    return ()

getGLContext :: HTMLCanvasElement -> JSM GLContext
getGLContext canvas = do
    gl <- do
        context <- Canvas.getContextUnsafe
            canvas ("webgl" :: Text) ([] :: [()])
        webglContext <- unsafeCastTo WebGLRenderingContext context
        return webglContext
    angleInstancedArrays <- uncheckedCastTo ANGLEInstancedArrays <$>
        GL.getExtensionUnsafe gl ("ANGLE_instanced_arrays" :: Text)
    return $ GLContext gl angleInstancedArrays

initGL :: GLContext -> JSM GLObjects
initGL GLContext{..} = do
    GL.clearColor gl 0.5 0.5 0.5 1.0

    vertexBuffer <- GL.createBuffer gl
    GL.bindBuffer gl GL.ARRAY_BUFFER (Just vertexBuffer)
    bufferDataFloat gl
        GL.ARRAY_BUFFER particleGeometryData GL.STATIC_DRAW

    translationBuffer <- GL.createBuffer gl
    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
    bufferDataSizeOnly gl GL.ARRAY_BUFFER
        (fromIntegral $ Model.maxParticlesNum * 8) GL.DYNAMIC_DRAW

    vertexShader <- buildShader gl GL.VERTEX_SHADER vertexShaderSource
    fragmentShader <- buildShader
        gl GL.FRAGMENT_SHADER fragmentShaderSource
    program <- buildProgram gl vertexShader fragmentShader
    GL.useProgram gl (Just program)

    modelUniform <- GL.getUniformLocation
        gl (Just program) ("u_model" :: Text)
    GL.uniformMatrix4fv gl (Just modelUniform) False $
        listFromMatrix $ modelMatrix $ realToFrac Model.particleRadius

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
    liftIO $ putStrLn $ "Error: " ++ show err

    return $ GLObjects
        translationBuffer
        projectionUniform

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

render :: GLContext
       -> GLObjects
       -> (Particles, ProjectionMatrix)
       -> JSM ()
render GLContext{..} GLObjects{..} (particles, projectionMatrix) = do
    GL.uniformMatrix4fv gl (Just projectionUniform) False $
        listFromMatrix projectionMatrix
    GL.clear gl GL.COLOR_BUFFER_BIT

    GL.bindBuffer gl GL.ARRAY_BUFFER $ Just translationBuffer
    bufferSubDataFloat gl GL.ARRAY_BUFFER 0 translations
    GL.bindBuffer gl GL.ARRAY_BUFFER Nothing

    drawArraysInstancedANGLE angleInstancedArrays
        GL.TRIANGLE_FAN 0 (particleGeometryNumSlices + 2) $ length particles
    where
        translations = [n | p <- particles, n <- [ x p, y p ]]
        x p = p ^. position ^. _x
        y p = p ^. position ^. _y

buildShader :: WebGLRenderingContext -> GLenum -> String -> JSM WebGLShader
buildShader gl shaderType sourceCode = do
    shader <- GL.createShader gl shaderType
    GL.shaderSource gl (Just shader) sourceCode
    GL.compileShader gl $ Just shader
    log <- GL.getShaderInfoLogUnsafe gl $ Just shader
    liftIO $ putStrLn $ "Shader compilation log: " ++ log
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
    liftIO $ putStrLn $ "Program log: " ++ log
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

trackCanvasSize :: ( Monad m
                   , RX.MonadHold t m
                   , MonadJSM (RX.Performable m)
                   , RX.MonadSample t (RX.Performable m)
                   , RX.PerformEvent t m
                   , RX.TriggerEvent t m )
                => HTMLCanvasElement
                -> RX.Event t a
                -> m ( RX.Behavior t (Maybe CanvasSize)
                     , RX.Event t (Maybe CanvasSize) )
trackCanvasSize canvas event = do
    (eSizeChanged, triggerSizeChanged) <- RX.newTriggerEvent
    bSize <- RX.hold Nothing eSizeChanged
    let performer = \_ -> do
            currentSize <- RX.sample bSize
            liftJSM $ do
                width' <- floor <$> Element.getClientWidth canvas
                height' <- floor <$> Element.getClientHeight canvas
                let triggerEvent = do
                        Canvas.setWidth canvas $ fromIntegral width'
                        Canvas.setHeight canvas $ fromIntegral height'
                        liftIO $ triggerSizeChanged $
                            Just $ CanvasSize width' height'
                        return ()
                case currentSize of
                    Just (CanvasSize{..}) ->
                        if width' /= width || height' /= height
                        then triggerEvent
                        else return ()
                    Nothing -> triggerEvent
    RX.performEvent_ $ performer <$> event
    return (bSize, eSizeChanged)

updateViewportSize :: GLContext -> CanvasSize -> JSM ()
updateViewportSize GLContext{..} CanvasSize{..} =
    GL.viewport gl 0 0 (fromIntegral width) (fromIntegral height)

projectionMatrixFromCanvasSize :: CanvasSize -> M44 Double
projectionMatrixFromCanvasSize CanvasSize{..} =
    transpose $ projection !*! modelView
    where
        projection = ortho
            (-width') width' (-height') height' (-1.0) 1.0
        modelView = mkTransformationMat
            identity $ V3 (-width') (-height') 0.0
        width' = (fromIntegral width) / 4.0
        height' = (fromIntegral height) / 4.0

modelMatrix :: Double -> M44 Double
modelMatrix radius = scaled $ V4 radius radius 1.0 1.0

boundingBoxFromCanvasSize :: CanvasSize -> BoundingBox
boundingBoxFromCanvasSize CanvasSize{..} = BoundingBox
    { _left = 0.0
    , _right = fromIntegral width
    , _bottom = 0.0
    , _top = fromIntegral height }
