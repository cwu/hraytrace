module Object where

import Geometry
import Algebra

-- Object shape material
data Object = Object Shape Material deriving (Eq, Show)

-- Material diffuse specular shininess
data Material = Material Color Color Double deriving (Eq, Show)
