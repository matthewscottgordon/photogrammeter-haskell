{-# LANGUAGE LambdaCase #-}

module Main where

import Lib

import Codec.Picture
import Data.Semigroup ((<>))
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
      savePngImage outputFileName image
    Left message -> do
         print message
         exitWith (ExitFailure (-2))
