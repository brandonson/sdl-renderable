{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, FlexibleInstances #-}
module Graphics.UI.SDL.Renderable (
  Renderable(..),
  RenderComposite(..)
) where

import Graphics.UI.SDL as SDL

class Renderable a where
  render :: a -> SDL.Surface -> IO ()
        
instance Renderable SDL.Surface where
  render sourceSurface ontoSurface = do
                           _ <- SDL.blitSurface sourceSurface Nothing ontoSurface Nothing
                           return ()
                           
data RenderComposite = forall a . Renderable a => RenderComposite [a]

instance Renderable RenderComposite where
  render (RenderComposite renderData) onto = mapM_ (\s -> render s onto) renderData