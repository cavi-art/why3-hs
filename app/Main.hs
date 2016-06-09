module Main where

import Why3.AsProcess

main :: IO ()
main = do
  putStrLn $ show $ dischargeTheory Z3 "theory X goal a: forall a:int. a = a end"
