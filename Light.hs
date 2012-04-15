module Light where

import Algebra

-- Light position diffuseColor specularColor
data Light = Light Point Color Color deriving (Eq, Show)


