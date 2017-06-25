{-# LANGUAGE LambdaCase #-}

module Main where

import Lib

import Codec.Picture
import Data.Array.Repa
import Data.Array.Repa.Repr.Vector
import Data.Convertible
import Data.Semigroup ((<>))
import qualified Data.Vector.Storable
import Options.Applicative
import qualified System.Environment
import System.Exit


data CommandLineArgs = CommandLineArgs
  { inputFileName  :: String,
    outputFileName :: String }

parseCommandLineArgs :: Parser CommandLineArgs
parseCommandLineArgs = CommandLineArgs
         <$> strOption
                 ( long "input"
                   <> short 'i'
                   <> metavar "INPUT_FILE"
                   <> help "Input file" )
         <*> strOption
                 ( long "output"
                   <> short 'o'
                   <> metavar "OUTPUT_FILE"
                   <> help "Output file" )


main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (parseCommandLineArgs <**> helper)
           ( fullDesc
             <> progDesc "Do some things to an image."
             <> header "photogrammeter" )

main' :: CommandLineArgs -> IO ()
main' (CommandLineArgs inputFileName outputFileName) = readImage inputFileName >>= \case
    Right image -> do
      print (typeName image)
      savePngImage outputFileName $ ( rgbFloatArrayToImage . doProcessing . imageToRgbFloatArray . convertRGB8) image
    Left message -> do
         print message
         exitWith (ExitFailure (-2))

typeName :: DynamicImage -> String
typeName (ImageY8 _) = "ImageY8"
typeName (ImageY16 _) = "ImageY16"
typeName (ImageYF _) = "ImageYF"
typeName (ImageYA8 _) = "ImageYA8"
typeName (ImageYA16 _) = "ImageYA16"
typeName (ImageRGB8 _) = "ImageRGB8"
typeName (ImageRGB16 _) = "ImageRGB16"
typeName (ImageRGBF _) = "ImageRGBF"
typeName (ImageRGBA8 _) = "ImageRGBA8"
typeName (ImageRGBA16 _) = "ImageRGBA16"
typeName (ImageYCbCr8 _) = "ImageYCbCr8"
typeName (ImageCMYK8 _) = "ImageCMYK8"
typeName (ImageCMYK16 _) = "ImageCMYK16"


-- Convert image to a repa array of floats.
-- First converts the image to [Float], which I can't image is the best way to go about it, but I can't figure
-- out how to do it better.
imageToRgbFloatArray image =
    ((rgb8ArrayForImage (imageWidth image) (imageHeight image)) . uint8VectorToFloatList) image


rgb8ArrayForImage :: Int -> Int -> [Float] -> Array U DIM3 Float
rgb8ArrayForImage width height = fromListUnboxed (Z :. (width::Int) :. (height::Int) :. (3::Int))

uint8VectorToFloatList = (Prelude.map ((/256.0).fromIntegral)) . Data.Vector.Storable.toList . imageData

rgbFloatArrayToImage :: Array U DIM3 Float -> DynamicImage
rgbFloatArrayToImage array =
    (ImageRGB8 . (Image width height) . Data.Vector.Storable.fromList . (Prelude.map (convert.(*256.0))) . toList) array
        where width = ((!!2) . listOfShape . extent ) array
              height = ((!!1) . listOfShape . extent ) array



doProcessing :: Array U DIM3 Float -> Array U DIM3 Float
doProcessing = id
