{-# OPTIONS_GHC -Wno-duplicate-exports #-}
-- | Haskell bindings for postgres full text search.
--   for a good explenation see <https://rachbelaid.com/postgres-full-text-search-is-good-enough/>
--
--   see the [readme](https://hackage.haskell.org/package/esqueleto-textsearch#tutorial) for a full tutorial.
module Database.Esqueleto.TextSearch (
   (@@.)
  , prefixAndQuery
  , toSearchTerm
  , SearchTerm
  , ts_rank
  , defaultWeights
  , Weights (..)
  , RegConfig
  , NormalizationOption(..)
  , module Database.Esqueleto.TextSearch.Language
  , module Database.Esqueleto.TextSearch.Types
) where

import Database.Esqueleto.TextSearch.Language
import Database.Esqueleto.TextSearch.Types
