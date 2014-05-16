module Graphics.UI.SDL.RenderUtils (
  colorAsGFXWord,
  clearScreen
)where

import Graphics.UI.SDL as SDL
import Data.Bits

colorAsGFXWord :: SDL.Color -> SDL.Pixel
colorAsGFXWord (Color r g b) = Pixel $ (fromIntegral r `shiftL` 24)
                            .|.(fromIntegral g `shiftL` 16)
                            .|.(fromIntegral b `shiftL` 8)
                            .|. 255
clearScreen :: SDL.Surface -> SDL.Color -> IO ()                           
clearScreen surface (Color r g b) = do
                                      color <- SDL.mapRGB (SDL.surfaceGetPixelFormat surface) r g b
                                      _ <-SDL.fillRect surface Nothing color
                                      return ()