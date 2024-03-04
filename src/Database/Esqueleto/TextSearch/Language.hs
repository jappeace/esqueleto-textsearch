{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Database.Esqueleto.TextSearch.Language
  ( (@@.)
  , prefixAndQuery
  , prefixOrQuery
  , prefixAndQueryLang
  , prefixOrQueryLang
  , toSearchTerm
  , toSearchTermWeighted
  , andWords
  , orWords
  , to_tsvector
  , to_tsquery
  , to_tsquery_en
  , plainto_tsquery
  , ts_rank
  , ts_rank_cd
  , setweight
  -- * ts binary
  , tsquery_or
  , tsquery_and
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
-- searchCompany :: SqlExpr (Entity CompanySearchIndex) -> NonEmpty (TsQuery Words) -> SqlQuery ()
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

-- | constructs a lexeme query out of a word algebra
--   english is the internal model used by postgres.
--
--   @
-- searchCompany :: SqlExpr (Entity CompanySearchIndex) -> Text -> SqlQuery ()
-- searchCompany company term = do
--   let query = to_tsquery (val "english") $ val $ andWords $ prefixAndQuery term
--   where_ $ (company ^. CompanySearchIndexDocument) @@. query
--   @
--
to_tsquery
  :: SqlExpr (Value RegConfig)
  -> SqlExpr (Value (TsQuery Words))
  -> SqlExpr (Value (TsQuery Lexemes) )
to_tsquery a b = unsafeSqlFunction "to_tsquery" (a, b)

-- | 'to_tsquery' defaulted to english
--
--   @
-- searchCompany :: SqlExpr (Entity CompanySearchIndex) -> Text -> SqlQuery ()
-- searchCompany company term = do
--   let query = to_tsquery_en $ val $ andWords $ prefixAndQuery term
--   where_ $ (company ^. CompanySearchIndexDocument) @@. query
--   @
--
to_tsquery_en :: SqlExpr (Value (TsQuery Words)) -> SqlExpr (Value (TsQuery Lexemes))
to_tsquery_en = to_tsquery (val "english")

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
-- searchCompany :: SqlExpr (Entity CompanySearchIndex) -> NonEmpty (TsQuery Words) -> SqlQuery ()
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
--
-- @
-- x::tsquery && y::tsquery == to_tsquery('x & y')
-- @
--
tsquery_and :: SqlExpr (Value (TsQuery Lexemes))
      -> SqlExpr (Value (TsQuery Lexemes))
      -> SqlExpr (Value (TsQuery Lexemes))
tsquery_and = unsafeSqlBinOp "&&"

-- | (||) for tsquery. This function would be called (&&.) but
-- Esqueleto's (||.) confines that fn to sql boolean expressions.
--
-- @
-- x::tsquery || y::tsquery == to_tsquery('x | y')
-- @
--
tsquery_or :: SqlExpr (Value (TsQuery Lexemes))
      -> SqlExpr (Value (TsQuery Lexemes))
      -> SqlExpr (Value (TsQuery Lexemes))
tsquery_or = unsafeSqlBinOp "||"

{-# DEPRECATED prefixAndQuery, prefixAndQueryLang, prefixOrQuery, prefixOrQueryLang, prefixAndQueryLangWith "these functions are simple wrappers for 'to_tsquery', use that directly instead" #-}
-- | format the query into lexemes
--   the result can be used in '@@.' for example:
--
-- @
-- searchCompany :: SqlExpr (Entity CompanySearchIndex) -> (NonEmpty (TsQuery Words)) -> SqlQuery ()
-- searchCompany company term = do
--   let query = prefixAndQuery term
--       norm = val []
--   where_ $ (company ^. CompanySearchIndexDocument) @@. query
-- @
--
--  this uses && to combine queries
prefixAndQuery :: (NonEmpty (TsQuery Words)) -> SqlExpr (Value (TsQuery Lexemes))
prefixAndQuery = prefixAndQueryLang "english"

-- | specify a language to be used with the query.
prefixAndQueryLang :: RegConfig -> (NonEmpty (TsQuery Words)) -> SqlExpr (Value (TsQuery Lexemes))
prefixAndQueryLang = prefixAndQueryLangWith tsquery_and

prefixOrQuery :: (NonEmpty (TsQuery Words)) -> SqlExpr (Value (TsQuery Lexemes))
prefixOrQuery = prefixOrQueryLang "english"

-- | same as 'prefixAndQueryLang' but uses || to combine quereis
prefixOrQueryLang :: RegConfig -> (NonEmpty (TsQuery Words)) -> SqlExpr (Value (TsQuery Lexemes))
prefixOrQueryLang = prefixAndQueryLangWith tsquery_or

-- | allows specifying which binary operation is used for combining queries.
prefixAndQueryLangWith :: (SqlExpr (Value (TsQuery Lexemes))
      -> SqlExpr (Value (TsQuery Lexemes))
      -> SqlExpr (Value (TsQuery Lexemes))) -> RegConfig -> (NonEmpty (TsQuery Words)) -> SqlExpr (Value (TsQuery Lexemes))
prefixAndQueryLangWith binOp language ts =
  foldr1 binOp
  $ map (to_tsquery (val language) . val) $ toList ts

-- | same as 'prefixAndQuery' without wrapping 'to_tsquery'.
andWords :: NonEmpty (TsQuery Words) -> TsQuery Words
andWords = foldr1 (:&)

-- | same as 'prefixOrQuery' without wrapping 'to_tsquery'.
orWords :: NonEmpty (TsQuery Words) -> TsQuery Words
orWords = foldr1 (:|)

-- | Constructs a valid search query, removes a bunch of illegal
--   characters and splits the terms for better results.
--   Also checks if there is anything in the search term.
--
--   using a search term is optional, but it's probably what you want.
--   all underlying primitives are exposed.
toSearchTerm :: Text -> Maybe (NonEmpty (TsQuery Words))
toSearchTerm = toSearchTermWeighted []

-- | create a search term with some weight, this allows for restricting on specific weighs.
--   see: https://www.postgresql.org/docs/current/textsearch-controls.html#TEXTSEARCH-PARSING-QUERIES
--   use the semigroup instance on search term to combine searchterms.
toSearchTermWeighted :: [Weight] -> Text -> Maybe (NonEmpty (TsQuery Words))
toSearchTermWeighted weights q = fmap (Word Prefix weights) <$> nonEmpty qs
  -- We disallow whitespace, \ and ' for the sake of producing a Text
  -- that can fit postgresql's requirements for to_tsquery's text
  -- argument. Note that this is not done nor needed for security reasons
  where qs = filter (not . T.null) $ T.words
             $ T.filter (`notElem` ['\\', '\'']) $ T.strip q
