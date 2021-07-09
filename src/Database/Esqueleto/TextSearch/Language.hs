{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Esqueleto.TextSearch.Language
  ( (@@.)
  , to_tsvector
  , to_tsquery
  , plainto_tsquery
  , ts_rank
  , ts_rank_cd
  , setweight
  ) where

import Data.String (IsString)
import Data.Text (Text)
import Database.Esqueleto (SqlExpr, Value)
#if MIN_VERSION_esqueleto(3,5,0)
import Database.Esqueleto.Internal.Internal (unsafeSqlBinOp, unsafeSqlFunction)
#else
import Database.Esqueleto.Internal.Sql (unsafeSqlBinOp, unsafeSqlFunction)
#endif
import Database.Esqueleto.TextSearch.Types

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
