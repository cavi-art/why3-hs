{-# LANGUAGE FlexibleInstances #-}
module Why3.AsProcess (
  discharge,
  isValidProof,
  dischargeTheory,

  Prover(..),
  SMTOutput(..),
  VersionedProver
                      ) where

import Data.Char (toLower)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess)


data Prover = AltErgo | CVC4 | Eprover | Gappa | Zenon | Z3

-- We are not yet implementing them all, but they are here.
-- The only ones implemented are valid, invalid and unknown.
data SMTOutput = Valid | Invalid | Unknown | Failure | Timeout String
  deriving (Eq, Show)

type VersionedProver = (Prover, Maybe String)

discharge :: ProverSpec a => a -> String -> String -> SMTOutput
discharge prover theoryImports = why3Output . getWhy3Output prover . embedFormulaInTheory
  where embedFormulaInTheory f = "theory T " ++ theoryImports ++ " goal g: " ++ f ++ "end"

dischargeTheory :: ProverSpec a => a -> String -> SMTOutput
dischargeTheory prover = why3Output . getWhy3Output prover


isValidProof :: SMTOutput -> Bool
isValidProof Valid = True
isValidProof _ = False


getWhy3Output :: ProverSpec a => a -> String -> String
getWhy3Output prover text =
  unsafePerformIO $ readProcess pathToWhy3 why3Params text
  where proverStr = stringFromProver prover
        pathToWhy3 = "why3"
        why3Params = ["prove",
                      "-F", "why",
                      "--timelimit", "1", -- one second fixed timelimit for now
                      "-", "-P", proverStr]

{-# NOINLINE getWhy3Output #-}


why3Output :: String -> SMTOutput
why3Output str = if theWord == "valid" then Valid
                 else if theWord == "invalid" then Invalid
                      else Unknown
  where fromColon = drop 2 $ dropWhile (/= ':') str
        theWord = map toLower $ takeWhile (/= ' ') fromColon


class ProverSpec a where
  stringFromProver :: ProverSpec a => a -> String


instance ProverSpec Prover where
  stringFromProver AltErgo = "alt-ergo"
  stringFromProver CVC4 = "cvc4"
  stringFromProver Eprover = "eprover"
  stringFromProver Gappa = "gappa"
  stringFromProver Zenon = "zenon"
  stringFromProver Z3 = "Z3"

instance ProverSpec (Prover, String) where
  stringFromProver (prover, version) = stringFromProver prover ++ "," ++ version

instance ProverSpec VersionedProver where
  stringFromProver (prover, Nothing) = stringFromProver prover
  stringFromProver (prover, Just version) = stringFromProver (prover, version)
