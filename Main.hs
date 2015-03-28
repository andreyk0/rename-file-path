{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main
where

import Control.Exception
import Control.Monad
import Data.Char
import Options.Applicative
import System.FilePath.Posix
import System.Posix.Files


data ConversionFormat =
  ConversionFormat { convertFileName :: String -> String
                   , convertExtension :: String -> String
                   }

instance Show ConversionFormat where
  show _ = "..."


data CmdLineArgs =
  CmdLineArgs { verbose :: Bool
              , format :: ConversionFormat
              , filePaths :: [String]
              } deriving (Show)


parseConversionFormat :: String -> ReadM ConversionFormat
parseConversionFormat _ = do
  s <- str
  case s
    of (n : '.' : e : []) ->
         return $ ConversionFormat (formatToConvF n) (formatToConvF e)

       x ->
         readerError $ "Unknown conversion format: " <> x

  where formatToConvF c = if (isUpper c)
                          then fmap (toUpper)
                          else fmap (toLower)


cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs =
  CmdLineArgs
     <$> switch
         ( long "verbose"
        <> short 'v'
        <> help "be verbose" )

     <*> option (str >>= parseConversionFormat )
         ( long "format"
        <> short 'f'
        <> help "name conversion format" )

     <*> some (argument str (metavar "FILES..."))


cmdLineParser :: ParserInfo CmdLineArgs
cmdLineParser =
  info (helper <*> cmdLineArgs)
      ( fullDesc
     <> header "Renames given file paths." )


makeNewFilePath :: ConversionFormat -> String -> String
makeNewFilePath ConversionFormat{..} fn =
  d </> (convertFileName fnb) <.> (convertExtension fne)
  where (d, f) = splitFileName fn
        (fnb, fne) = splitExtensions f


makeTodo :: CmdLineArgs -> [IO ()]
makeTodo CmdLineArgs {..} = fmap (moveSingleFile) filePaths
  where moveSingleFile f = do
          let newFp = makeNewFilePath format f
          if (newFp == f)
            then when verbose $ putStrLn $ "Skipping " <> f <> ", already matches pattern ..."
            else do when verbose $ putStrLn $ "Moving " <> f <> " -> " <> newFp
                    catch (rename f newFp)
                          (\(e::IOException) -> putStrLn $ "ERROR moving " <> f <> " -> " <> newFp <> " :: " <> (show e))


main :: IO ()
main = do
  args <- execParser cmdLineParser
  let todo = makeTodo args
  sequence_ todo
