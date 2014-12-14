
{-# Language ViewPatterns, DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

module Main where

import Text.ParserCombinators.Parsec
import Data.List.Split
import GHC.Generics

import Data.Aeson
import Data.Aeson.Types

import Data.Coerce

-- import Data.String.CSV
import Data.CSV
import qualified Data.Vector as V


import Control.Monad
import qualified Data.ByteString.Lazy as L

import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import System.Directory
import System.Environment


import qualified Data.Map as Map
import qualified Data.Text as T

import qualified Codec.Compression.GZip as GZip


newtype DRecord  = DRecord (Map String DRecord2 ) deriving ( Show, Generic)
newtype DRecord2 = DRecord2 (Map Int Double)      deriving ( Show, Generic)


instance ToJSON DRecord
instance ToJSON DRecord2 where
  toJSON (DRecord2 v) = Array $ V.fromList $ map (\(k,v) -> object ["time" .= k, "value" .= v ] ) $ Map.toList v
-- instance ToJSON (Map Int Double) where
-- instance ToJSON (HashMap Int Double)
-- instance Generic (Map Int Double)

timeToInt :: String -> Int
timeToInt (splitOn ":" -> [read -> a, read -> b]) = a * 60 + b

processFile :: String -> IO DRecord
processFile f = do
   -- do result <- 
   Right (h:a) <- parseFromFile csvFile f
   return $ foldr ( addLineMap ) (DRecord Map.empty) a

addLineMap :: [String] -> DRecord -> DRecord
addLineMap [timeToInt -> t, c, read -> v] ( DRecord a )= DRecord .  Map.insertWithKey f c ( DRecord2 $ Map.singleton t v) $ a
   where
    f _ (DRecord2 n) (DRecord2 o) = DRecord2 $ n `Map.union` o

-- px = "a"

main = do
  [px] <- getArgs
  print "cool"
  files <- return . filter (( > 2 ) . length)   =<< getDirectoryContents ("set-" ++ px)
  ~ldr <- forM files $ \f -> do
      dr <- processFile  ("set-" ++ px  ++ "/" ++ f)
      L.writeFile ( (\d -> "json/"++ px ++ "/" ++ d ++ ".json") . head . splitOn "." $ f ) $ GZip.compress $ encode dr
      return dr
  L.writeFile ("json/"++ px ++ "/all.json") $  GZip.compress $ encode ldr

