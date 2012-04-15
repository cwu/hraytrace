module Light where

import Algebra

-- Light position ambientColor diffuseColor specularColor
data Light = Light Point Color Color Color deriving (Eq, Show)


