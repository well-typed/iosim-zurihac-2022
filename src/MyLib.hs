module MyLib where

import Control.Monad.IOSim

-- import qualified Project as P
import qualified Demo as D

-- Run main and show trace
--
main :: IO ()
main = putStrLn trace

trace :: String
trace = ppTrace $ runSimTrace D.example

