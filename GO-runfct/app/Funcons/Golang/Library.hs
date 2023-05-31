module Funcons.Golang.Library(
    funcons, entities, types,
    module Funcons.Golang.GO2Types.GO2Types,
) where

import Funcons.EDSL
import Funcons.Golang.GO2Types.GO2Types hiding (funcons, entities, types)
import qualified Funcons.Golang.GO2Types.GO2Types

funcons = libUnions
    [ Funcons.Golang.GO2Types.GO2Types.funcons
    ]

entities = concat
    [ Funcons.Golang.GO2Types.GO2Types.entities
    ]

types = typeEnvUnions
    [ Funcons.Golang.GO2Types.GO2Types.types
    ]
