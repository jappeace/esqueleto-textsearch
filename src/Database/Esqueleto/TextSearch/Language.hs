{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Database.Esqueleto.TextSearch.Language
  ( (@@.)
  , prefixAndQuery
  , toSearchTerm
  , toSearchTermWeighted
  , SearchTerm
  , to_tsvector
  , to_tsquery
  , plainto_tsquery
  , ts_rank
  , ts_rank_cd
  , setweight
  ) where

import Data.String (IsString)
import Data.Text (Text)
#if MIN_VERSION_esqueleto(3,5,0)
import Database.Esqueleto.Internal.Internal (unsafeSqlBinOp, unsafeSqlFunction)
import Database.Esqueleto.Experimental (SqlExpr, Value, val)
#else
import Database.Esqueleto (SqlExpr, Value, val)
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp, unsafeSqlFunction)
#endif
import Database.Esqueleto.TextSearch.Types
import qualified Data.Text as T
import Data.List.NonEmpty(nonEmpty, NonEmpty, toList)


-- | Apply some query to a tsvector document
--   for example:
--
-- @
-- searchCompany :: SqlExpr (Entity CompanySearchIndex) -> SearchTerm -> SqlQuery ()
-- searchCompany company term = do
--   let query = prefixAndQuery term
--       norm = val []
--   where_ $ (company ^. CompanySearchIndexDocument) @@. query
-- @
--
(@@.)
  :: SqlExpr (Value TsVector) -- ^ the document to search in
  -> SqlExpr (Value (TsQuery Lexemes)) -- ^ the query made by 'prefixAndQuery'
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

-- | Organize search result by weights. This allows you to put better
--   matching results higher.
--   for example:
--
-- @
-- searchCompany :: SqlExpr (Entity CompanySearchIndex) -> SearchTerm -> SqlQuery ()
-- searchCompany company term = do
--   let query = prefixAndQuery term
--       norm = val []
--   where_ $ (company ^. CompanySearchIndexDocument) @@. query
--   orderBy [desc (ts_rank (val defaultWeights)
--                  (company ^. CompanySearchIndexDocument)
--                  query norm)]
-- @
--
ts_rank
  :: SqlExpr (Value Weights) -- ^ relative weighting of a b c and d, see 'defaultWeights'
  -> SqlExpr (Value TsVector) -- ^ the document to search in
  -> SqlExpr (Value (TsQuery Lexemes)) -- ^ the query made by 'prefixAndQuery'
  -> SqlExpr (Value [NormalizationOption]) -- ^ normalization option to indicate how to deal with document length
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
--   the result can be used in '@@.' for example:
--
-- @
-- searchCompany :: SqlExpr (Entity CompanySearchIndex) -> SearchTerm -> SqlQuery ()
-- searchCompany company term = do
--   let query = prefixAndQuery term
--       norm = val []
--   where_ $ (company ^. CompanySearchIndexDocument) @@. query
-- @
--
prefixAndQuery :: SearchTerm -> SqlExpr (Value (TsQuery Lexemes))
prefixAndQuery = prefixAndQueryLang "english"

-- | specify a language to be used with the query.
prefixAndQueryLang :: RegConfig -> SearchTerm -> SqlExpr (Value (TsQuery Lexemes))
prefixAndQueryLang language (SearchTerm ts) =
  foldr1 tsquery_and
  $ map (to_tsquery (val language) . val) $ toList ts


-- | A valid search term.
--   created with 'toSearchTerm'.
newtype SearchTerm = SearchTerm { unQuery :: NonEmpty (TsQuery Words) }
  deriving stock Show
  deriving newtype Semigroup

-- | Constructs a valid search query, removes a bunch of illegal
--   characters and splits the terms for better results.
--   Also checks if there is anything in the search term.
--
--   using a search term is optional, but it's probably what you want.
--   all underlying primitives are exposed.
toSearchTerm :: Text -> Maybe SearchTerm
toSearchTerm = toSearchTermWeighted []

-- | create a search term with some weight, this allows tweaking of priority of certain terms.
--   for example if you want to do some post processing on user input.
--   where they insist on typing dashes,
--   you split on the dash and concatinate the search term with lower
--   priority splitted strings on the dash.
--   so the full string is high priority, substrings lower.
--   use the semigroup instance on search term to combine these.
toSearchTermWeighted :: [Weight] -> Text -> Maybe SearchTerm
toSearchTermWeighted weights q = SearchTerm .  fmap (Word Prefix weights) <$> nonEmpty qs
  -- We disallow whitespace, \ and ' for the sake of producing a Text
  -- that can fit postgresql's requirements for to_tsquery's text
  -- argument. Note that this is not done nor needed for security reasons
  where qs = filter (not . T.null) $ T.words
             $ T.filter (`notElem` ['\\', '\'']) $ T.strip q
