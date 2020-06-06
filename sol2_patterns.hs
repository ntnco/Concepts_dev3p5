{- Auteur: Antoine Colson
 - Remise: 3 juin 2020
 -}

data Exp = Enum Int          -- Une constante
         | Eplus Exp Exp     -- e1 + e2
         | Etimes Exp Exp    -- e1 * e2
         deriving Show

optimize :: Exp -> Exp
optimize (Enum x) = (Enum x) -- cas d'une constante
optimize (Eplus y z) = helper (Eplus (optimize y) (optimize z)) 
optimize (Etimes y z) = helper (Etimes (optimize y) (optimize z)) 

helper :: Exp -> Exp
helper (Eplus b (Enum 0)) = b       -- addition avec zéro
helper (Eplus (Enum 0) x) = x
helper (Etimes (Enum 0) _) = Enum 0 -- multiplic par zéro
helper (Etimes _ (Enum 0)) = Enum 0
helper (Etimes (Enum 1) x) = x      -- multiplic par un
helper (Etimes x (Enum 1)) = x
helper a = a -- aucune optimisation possible
