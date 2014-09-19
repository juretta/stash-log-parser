{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Stash.Log.Types (
    AggregationLevel(..)
  , Millis(..)
) where

import           Data.Data
import           Text.Printf
import           Control.DeepSeq

data AggregationLevel = Hour | Minute deriving (Data,Typeable,Show,Eq)

newtype Millis = Millis {
  millis :: Int
} deriving (Num, Eq, Ord, PrintfArg)

instance Show Millis where
    show (Millis s) = show s

instance NFData Millis where
    rnf (Millis m) = m `seq` ()
