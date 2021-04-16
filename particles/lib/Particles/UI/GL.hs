{-# LANGUAGE OverloadedStrings #-}

module Particles.UI.GL where

import Control.Lens ((^.))
import Data.Int (Int32)
import Data.Text (Text)
import GHCJS.DOM.Types
    ( GLenum
    , toJSVal
    , WebGLRenderingContext )
import Language.Javascript.JSaddle (jsf, jsg, JSM, new)

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
