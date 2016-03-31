module Main where

import qualified CLI as CLI
import qualified API.Main as API

import System.Environment
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = getArgs >>= chooseMode


chooseMode :: [String] -> IO ()
chooseMode [] = hPutStrLn stderr "Invalid arguments. Use --cli or --api"
chooseMode ("--cli":_) = CLI.main
chooseMode ("--api":_) = API.main
chooseMode _ = hPutStrLn stderr "Invalid arguments."
