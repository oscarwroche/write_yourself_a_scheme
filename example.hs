module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  line <- getLine
  putStrLn $ line 
