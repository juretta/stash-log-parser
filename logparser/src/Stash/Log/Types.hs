{-# LANGUAGE DeriveDataTypeable #-}

module Stash.Log.Types (
    AggregationLevel(..)
) where

import Data.Typeable
import Data.Data

data AggregationLevel = Hour | Minute deriving (Data,Typeable,Show,Eq)
