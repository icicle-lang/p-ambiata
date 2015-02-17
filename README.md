p
=

```
The Ambiata Prelude. Short and sweet.
```

General rules.

 - Safety first.
 - Consistency of functionality.
 - Attempt at completeness of functionality across core structures.

Guidance - "Should it go in `p`"

 - Is something that is very general & useful, with an obvious semantic and can be implemented from `base` - YES.
 - If it is unsafe - NO.
 - Is there a more general version - NO.
 - Is it something available in base in only _some_ GHC versions - YES.
 - It has a lot of dependencies - Proabably Not - Maybe case for splitting into p-*.
 - It introduces name clashes - Probably Not - Consider a standalone module designed to be import qualified in `P`, i.e. `P.Text as T`.

### Depending on `P`

This follows standard process of all internal modules:
 - git submodule in lib directly
 - entry in cabal file with no version bounds
 - makefile dep to init submodule and add to sandbox
 
To do this in a normal project:

```
# add git submodule
git submodule add git@github.com:ambiata/p lib/p
# add to cabal the first time (assumes sandbox already exists)
cabal sandbox add-source lib/p
# add p to dependencies
vi project.cabal
# add entries to make file, see https://github.com/ambiata/vee/blob/aed02c5cfa7b5b609b6e175440ae71ab2615615c/Makefile#L13-L20
vi Makefile
```

