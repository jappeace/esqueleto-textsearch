{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Esqueleto.TextSearch.Language
  ( (@@.)
  , SearchTerm
  , toSearchTerm
  , prefixAndQuery
  , to_tsvector
  , to_tsquery
  , plainto_tsquery
  , ts_rank
  , ts_rank_cd
  , setweight
  ) where

import Data.String (IsString)
import Data.Text (Text)
import Database.Esqueleto (SqlExpr, Value, val)
#if MIN_VERSION_esqueleto(3,5,0)
import Database.Esqueleto.Internal.Internal (unsafeSqlBinOp, unsafeSqlFunction)
#else
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp, unsafeSqlFunction)
#endif
import Database.Esqueleto.TextSearch.Types
import qualified Data.Text as T
import Data.List.NonEmpty(nonEmpty, NonEmpty, toList)

(@@.)
  :: SqlExpr (Value TsVector)
  -> SqlExpr (Value (TsQuery Lexemes))
  -> SqlExpr (Value Bool)
(@@.) = unsafeSqlBinOp "@@"

to_tsvector
  :: IsString a
  => SqlExpr (Value RegConfig)
  -> SqlExpr (Value a)
  -> SqlExpr (Value TsVector)
to_tsvector a b = unsafeSqlFunction "to_tsvector" (a, b)

to_tsquery
  :: SqlExpr (Value RegConfig)
  -> SqlExpr (Value (TsQuery Words))
  -> SqlExpr (Value (TsQuery Lexemes) )
to_tsquery a b = unsafeSqlFunction "to_tsquery" (a, b)

plainto_tsquery
  :: SqlExpr (Value RegConfig)
  -> SqlExpr (Value Text)
  -> SqlExpr (Value (TsQuery Lexemes))
plainto_tsquery a b = unsafeSqlFunction "plainto_tsquery" (a, b)

ts_rank
  :: SqlExpr (Value Weights)
  -> SqlExpr (Value TsVector)
  -> SqlExpr (Value (TsQuery Lexemes))
  -> SqlExpr (Value [NormalizationOption])
  -> SqlExpr (Value Double)
ts_rank a b c d = unsafeSqlFunction "ts_rank" (a, b, c, d)

ts_rank_cd
  :: SqlExpr (Value Weights)
  -> SqlExpr (Value TsVector)
  -> SqlExpr (Value (TsQuery Lexemes))
  -> SqlExpr (Value [NormalizationOption])
  -> SqlExpr (Value Double)
ts_rank_cd a b c d = unsafeSqlFunction "ts_rank_cd" (a, b, c, d)

setweight
  :: SqlExpr (Value TsVector)
  -> SqlExpr (Value Weight)
  -> SqlExpr (Value TsVector)
setweight a b = unsafeSqlFunction "setweight" (a, b)

-- | (&&) for tsquery. This function would be called (&&.) but
-- Esqueleto's (&&.) confines that fn to sql boolean expressions.
-- x::tsquery && y::tsquery == to_tsquery('x & y')
tsquery_and :: SqlExpr (Value (TsQuery Lexemes))
      -> SqlExpr (Value (TsQuery Lexemes))
      -> SqlExpr (Value (TsQuery Lexemes))
tsquery_and = unsafeSqlBinOp "&&"

-- | format the query into lexemes
--   the result can be used in '@@.' for example.
--   defaults to english.
--
--   @
--
--      where_ $ (index ^. UnitSearchIndexDocument) @@. prefixAndQuery query
--
--   @
--
prefixAndQuery :: SearchTerm -> SqlExpr (Value (TsQuery Lexemes))
prefixAndQuery = prefixAndQueryLang "english"

prefixAndQueryLang :: RegConfig -> SearchTerm -> SqlExpr (Value (TsQuery Lexemes))
prefixAndQueryLang language (SearchTerm ts) =
  foldr1 tsquery_and
  $ map (to_tsquery (val language) . val . Word Prefix []) $ toList ts


-- | A valid search term
newtype SearchTerm = SearchTerm { unQuery :: NonEmpty Text }
  deriving (Show)

-- | constructs a valid search query, removes a bunch of illegal
--   characters and splits the terms for better results
toSearchTerm :: Text -> Maybe SearchTerm
toSearchTerm q = SearchTerm <$> nonEmpty qs
  -- We disallow whitespace, \ and ' for the sake of producing a Text
  -- that can fit postgresql's requirements for to_tsquery's text
  -- argument. Note that this is not done nor needed for security reasons
  where qs = filter (not . T.null) $ T.words
             $ T.filter (`notElem` ['\\', '\'']) $ T.strip q
