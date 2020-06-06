{- Auteur: Antoine Colson
 - Remise: 3 juin 2020
 -}

data Exp = Enum Int          -- Une constante
         | Eplus Exp Exp     -- e1 + e2
         | Etimes Exp Exp    -- e1 * e2
         deriving Show

optimize :: Exp -> Exp
-- on commence par gérer le cas de base d'une constante
optimize (Enum x) = (Enum x)

-- ensuite on gère les 2 cas de la multiplication
optimize (Etimes x y) = optiMul (Etimes (optimize x) (optimize y))
    where optiMul (Etimes (Enum 0) _) = Enum 0
          optiMul (Etimes _ (Enum 0)) = Enum 0
          optiMul (Etimes (Enum 1) x) = x
          optiMul (Etimes x (Enum 1)) = x
          optiMul (Etimes x y) = Etimes x y -- retourne l'input

-- enfin on gère le cas de l'addition
optimize (Eplus x y) = optiAdd (Eplus (optimize x) (optimize y))
    where optiAdd (Eplus (Enum 0) y) = y
          optiAdd (Eplus x (Enum 0)) = x
          optiAdd (Eplus x y) = Eplus x y -- retourne l'input
