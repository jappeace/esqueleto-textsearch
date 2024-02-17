# esqueleto-textsearch
Haskell bindings for postgres full text search in esqueleto.
for a good explenation see https://rachbelaid.com/postgres-full-text-search-is-good-enough/

you can turn postgres into a database that is similar
in performance for search as elastic search, without having 
to deal with elastic search.

## Hacking
install [nix](https://nixos.org/download) (the package manager).
Enter the nix shell.
```
nix develop
cabal build
```

running the tests with nix:
```
nix check
```


## Tutorial

1. decide which fields you want to search for,
   this can be from several tables
2. setup a materialized view with the fields, for example:
```sql
CREATE MATERIALIZED VIEW public.companies_search_index AS
 SELECT c.id,
    c.name,
    c.type,
    c.trade,
    countries.name AS domicile,
    p.name AS parent,
    c.is_archived,
    COALESCE(string_agg((cd.name)::text, ', '::text), ''::text) AS domains,
    (((((setweight(to_tsvector((c.name)::text), 'A'::"char") || setweight(to_tsvector((COALESCE(c.type, ''::character varying))::text), 'D'::"char")) || setweight(to_tsvector((c.trade)::text), 'D'::"char")) || setweight(to_tsvector((COALESCE(countries.name, ''::character varying))::text), 'C'::"char")) || setweight(to_tsvector((COALESCE(p.name, ''::character varying))::text), 'B'::"char")) || setweight(to_tsvector(COALESCE(string_agg((cd.name)::text, ' '::text), ''::text)), 'A'::"char")) AS document
   FROM (((public.companies c
     LEFT JOIN public.countries ON ((countries.id = c.domicile)))
     LEFT JOIN public.companies p ON ((p.id = c.parent)))
     LEFT JOIN public.company_domains cd ON ((cd.company_id = c.id)))
  GROUP BY c.id, p.parent, c.name, c.type, c.trade, countries.name, p.name
  WITH NO DATA;
```
the [blogpost](https://rachbelaid.com/postgres-full-text-search-is-good-enough/) describes better what goes on here.

3. Create an associated persistent model to convince esqueleto this is a table

```haskell
mkPersistWith sqlSettings $(discoverEntities) [persistLowerCase|
CompanySearchIndex sql=companies_search_index
  Id           CompanyId -- in this case a 1 to 1 relation with the real company table
  -- these fields are mostly for debugging the document
  name         Text
  type         Text Maybe
  trade        Text
  domicile     Text Maybe
  parent       Text Maybe
  domains      Text
  -- we query based on the documeent, this is the important bit.
  document     TsVector
  isArchived   Bool
  deriving     Show
|]

instance ToBaseId CompanySearchIndex where
  type BaseEnt CompanySearchIndex = Company
  toBaseIdWitness = CompanySearchIndexKey
```
4. make sure to periodically refresh this view, or setup database triggers to do that for you.
```sql
CREATE FUNCTION public.refresh_companies_search_index() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
begin
    refresh materialized view companies_search_index;
    return null;
end $$;

CREATE TRIGGER refresh_companies_search_index AFTER INSERT OR DELETE OR UPDATE OR TRUNCATE ON public.companies FOR EACH STATEMENT EXECUTE FUNCTION public.refresh_companies_search_index();
```
5. use this library to search, for example:
```haskell
import Database.Esqueleto.TextSearch.Language(SearchTerm, prefixAndQuery, ((@@.)), ts_rank)
import Database.Esqueleto.TextSearch.Types(defaultWeights)

searchCompany :: SqlExpr (Entity CompanySearchIndex) -> SearchTerm -> SqlQuery ()
searchCompany company term = do
  let query = prefixAndQuery term
      norm = val []
  where_ $ (company ^. CompanySearchIndexDocument) @@. query
  orderBy [desc (ts_rank (val defaultWeights)
                 (company ^. CompanySearchIndexDocument)
                 query norm)]
```
