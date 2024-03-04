# Change log for esqueleto text search 

## Version 1.3.0
+ Update misleading tsquery and and or
+ expose underlying lists in search terms
  + you may get some breaking change since SearchTerm no longer exists.
    It should be replaced with `NonEmpty (TsQuery Words)`.
    I thought it better to just cleanup since the library is still rather young.
+ remove search term datatype in favor of underlying.
  I realized the newtype would be heavy to maintain for little benefit
+ Add alternative api which is simpler using the word algebra,
  added some examples in the docs which use these.

## Version 1.2.1
+ Update misleading docs

## Version 1.2.0
+ Add prefix or query.
+ expose binops on tsquery

## Version 1.1.5
+ enable tests in CI
+ allow setting of weights in tsquery

## Version 1.1.4
+ link to new homepage
+ add better documentation. rexport important stuff in the main module explicetly

## Version 1.1.3
+ update maintainer field to me.
+ update copyright field.

## Version 1.1.2
+ add myself as author considering I wrote most stuff at this point

## Version 1.1.1
+ add tutorial in the readme

## Version 1.1.0

+ Removed weird patch numbering
+ added a saner api for doing actual search.
+ improve documentation.
+ bump text
+ drop data default dependency.


## Version 1.0.0.3 

initial version, uploaded from:
https://github.com/creichert/esqueleto-textsearch
