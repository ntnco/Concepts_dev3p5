{- Auteur: Antoine Colson
 - Remise: 3 juin 2020
 -}

data Exp = Enum Int          -- Une constante
         | Eplus Exp Exp     -- e1 + e2
         | Etimes Exp Exp    -- e1 * e2
         deriving Show

optimize :: Exp -> Exp
optimize (Enum x) = (Enum x)    -- cas d'un constante
optimize (Eplus y z) = let (a,b) = (optimize y, optimize z) 
    in case (a, b) of 
    (x, Enum 0) -> x          -- addition avec zéro
    (Enum 0, x) -> x
    (x, y) -> Eplus x y
optimize (Etimes y z) = let (a,b) = (optimize y, optimize z)
    in case (a, b) of
    (Enum 0, _) -> Enum 0     -- multiplication par 0
    (_, Enum 0) -> Enum 0
    (Enum 1, x) -> x          -- multiplication par 1
    (x, Enum 1) -> x
    (x, y) -> Etimes x y
