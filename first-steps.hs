module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn ("What is your name?")
  name <- getLine
  putStrLn ("Hello, " ++ name)