module Main where

import Data.String.Strip
import Why3.AsProcess

main :: IO ()
main = do
  putStrLn $ show $ discharge Z3 "theory X goal a: true end"

-- main = interact strip
