module Funcons.Golang.Library(
    funcons, entities, types,
    module Funcons.Golang.GOBFuncons.GOBFuncons,
    module Funcons.Golang.GO2Types.GO2Types,
) where

import Funcons.EDSL
import Funcons.Golang.GOBFuncons.GOBFuncons hiding (funcons, entities, types)
import qualified Funcons.Golang.GOBFuncons.GOBFuncons
import Funcons.Golang.GO2Types.GO2Types hiding (funcons, entities, types)
import qualified Funcons.Golang.GO2Types.GO2Types

funcons = libUnions
    [ Funcons.Golang.GOBFuncons.GOBFuncons.funcons
    , Funcons.Golang.GO2Types.GO2Types.funcons
    ]

entities = concat
    [ Funcons.Golang.GOBFuncons.GOBFuncons.entities
    , Funcons.Golang.GO2Types.GO2Types.entities
    ]

types = typeEnvUnions
    [ Funcons.Golang.GOBFuncons.GOBFuncons.types
    , Funcons.Golang.GO2Types.GO2Types.types
    ]
