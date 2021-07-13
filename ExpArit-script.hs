factorial :: Integer -> Integer -- Unbounded
factorial x
    | x == 0 = 1
    | x > 0 = x * factorial (x-1)
    
fact :: Int -> Int  -- Bounded
fact x
    | x == 0 = 1
    | x > 0 = x * fact (x-1)
    
-- probar factorial 21 vs fact 21

infixl 6 :+:, :-:
infixl 7 :*:, :/:

data Expr a =   N a  
              | Expr a :+: Expr a  
              | Expr a :-: Expr a  
              | Expr a :*: Expr a  
              | Expr a :/: Expr a 
              deriving Eq
              
ejem1 = N 5 :+: N 3 :+: N 4             
ejem2 = N 5 :+: N 8 :*: N 2
a = N 3:+: N 2 
--      N 5 :+: (N 8 :*: N 2)  ==> 21
--  NO (N 5 :+: N 8) :*: N 2 ==> 26          
   
evaluar :: Fractional a => Expr a -> a   
evaluar (N x) = x
evaluar (e1 :+: e2) = evaluar e1 + evaluar e2     
evaluar (e1 :-: e2) = evaluar e1 - evaluar e2
evaluar (e1 :*: e2) = evaluar e1 * evaluar e2
evaluar (e1 :/: e2) = evaluar e1 / evaluar e2

-- evaluar :: Integral a => Expr a -> a
-- evaluar (e1 :/: e2) = evaluar e1 `div` evaluar e2 

-- Ejercicios: (
-- 1.- Hacer (Exp a) instancia de la clase Show
-- 2.- Hacer (Exp a) instancia de la clase Eq usando evaluar
-- 3.- Hacer (Exp a) instancia de la clase Num       

instance Show Expr a where
    show a
        = case a of
            (N x) -> x
            (x :+: y) -> (show x) ++ "+" ++ (show y)
            (x :-: y) -> (show x) ++ "-" ++ (show y)
            (x :*: y) -> (show x) ++ "*" ++ (show y)
            (x :/: y) -> (show x) ++ "/" ++ (show y)