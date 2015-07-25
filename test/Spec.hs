import Control.Monad
import Data.Functor
import Data.List
import System.IO.HVFS
import System.Path
import System.Process

main :: IO ()
main = do
    examples <- filter (".hs" `isSuffixOf`) <$> recurseDir SystemFS "examples"
    forM_ examples compile
  where
    compile file = callProcess "ghc" ["-c", "-fforce-recomp", file]
