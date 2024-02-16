
-- | Haskell bindings for postgres full text search.
--   for a good explenation see https://rachbelaid.com/postgres-full-text-search-is-good-enough/
module Database.Esqueleto.TextSearch (
    module Database.Esqueleto.TextSearch.Language
  , module Database.Esqueleto.TextSearch.Types
) where

import Database.Esqueleto.TextSearch.Language
import Database.Esqueleto.TextSearch.Types
