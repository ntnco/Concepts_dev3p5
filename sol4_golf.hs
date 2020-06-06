{- Auteur: Antoine Colson
 - Exercice 3½, version golf pour le fun
 -}

data Exp = Enum Int          -- Une constante
         | Eplus Exp Exp     -- e1 + e2
         | Etimes Exp Exp    -- e1 * e2
         deriving (Eq, Show)

optimize x@(Enum _)=x
optimize(Eplus x y)|z==a=b|z==b=a|otherwise=Eplus a b
    where(o,a,b,z)=(optimize,o x,o y,Enum 0)
optimize(Etimes x y)|z==a||z==b=z|u==b=a|u==a=b|otherwise=Etimes a b
    where(o,a,b,z,u)=(optimize,o x,o y,Enum 0,Enum 1)
