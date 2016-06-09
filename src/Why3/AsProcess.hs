{-# LANGUAGE FlexibleInstances #-}
module Why3.AsProcess (
  discharge,
  isValidProof,
  dischargeTheory,
  usableWhy3,
  why3Provers,

  Prover(..),
  SMTOutput(..),
  VersionedProver
                      ) where

import Control.Monad
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, readProcessWithExitCode)


-- | This constant should probably be removed. We are trying to find
-- why3 in the PATH, and this is the internal constant to keep track
-- of that.
pathToWhy3 = "why3"


data Prover = AltErgo | CVC4 | Eprover | Gappa | Zenon | Z3
  deriving (Eq, Show)

-- We are not yet implementing them all, but they are here.
-- The only ones implemented are valid, invalid and unknown.
data SMTOutput = Valid | Invalid | Unknown | Failure | Timeout String
  deriving (Eq, Show)

type VersionedProver = (Prover, Maybe String)

discharge :: ProverSpec a => a -> String -> String -> SMTOutput
discharge prover theoryImports = why3Output . getWhy3Output prover . embedFormulaInTheory theoryImports


embedFormulaInTheory theoryImports f = "theory T " ++ theoryImports ++ " goal g: " ++ f ++ " end"


dischargeTheory :: ProverSpec a => a -> String -> SMTOutput
dischargeTheory prover = why3Output . getWhy3Output prover


isValidProof :: SMTOutput -> Bool
isValidProof Valid = True
isValidProof _ = False


getWhy3Output :: ProverSpec a => a -> String -> String
getWhy3Output prover text =
  unsafePerformIO $ readProcess pathToWhy3 why3Params text
  where proverStr = stringFromProver prover
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


-- | Find out whether Why3 can be called from Haskell.
usableWhy3 = unsafePerformIO $ do
  (exit, _, _) <- readProcessWithExitCode "which" [pathToWhy3] ""
  return $ case exit of
    ExitSuccess -> True
    _ -> False


why3Provers :: [(Prover, String)]
why3Provers = unsafePerformIO $ proversList
  where proversString = readProcess pathToWhy3 ["--list-provers"] ""
        stripLine = T.unpack . T.strip . T.pack
        proversList = catMaybes <$> (fmap stringToProver) <$> tail <$> (fmap (fmap stripLine) $ lines <$> proversString)
{-# NOINLINE why3Provers #-}


stringToProver = unLift . liftM2 (,) stringToProverName stringToProverVersion
 where unLift (Just a, Just b) = Just (a, b)
       unLift (_, _) = Nothing

stringToProverVersion = Just . unparenthize . version
  where unparenthize = init . tail -- remove first and last letters (should be parens)
        version = head . tail . words -- second element of words of the argument

-- | This function returns an incomplete list. There are some provers
-- which are not implemented here. This could be bypassed by just
-- using the raw string.
stringToProverName s =
  let proverName = head $ words s
  in case proverName of
    "Alt-Ergo" -> Just AltErgo
    "CVC4" -> Just CVC4
    "Eprover" -> Just Eprover
    "Gappa" -> Just Gappa
    "Z3" -> Just Z3
    "Zenon" -> Just Zenon
    _ -> Nothing


class ProverSpec a where
  stringFromProver :: ProverSpec a => a -> String

instance ProverSpec String where
  stringFromProver = id

instance ProverSpec (String, String) where
  stringFromProver (prover, version) = prover ++ "," ++ version

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
