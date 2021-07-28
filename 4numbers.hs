module Main where

data Op = Add | Sub | Mul | Div

allOps :: [Op]
allOps = [Add, Sub, Mul, Div]


data Expr = Val Double | Complex Op Expr Expr


operator :: Op -> Double -> Double -> Double
operator Add = (+)
operator Sub = (-)
operator Mul = (*)
operator Div = (/)


eval :: Expr -> Double
eval (Val x) = x
eval (Complex op a b) = operator op (eval a) (eval b)


instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"


instance Show Expr where
    show (Val x) = show (floor x)
    show (Complex op a b) = "(" ++ show a ++ show op ++ show b ++ ")"


toFourNumbers :: Integral a => a -> [Expr]
toFourNumbers x = map (Val . fromIntegral) [x `div` 1000, x `div` 100 `mod` 10, x `div` 10 `mod` 10, x `mod` 10]


possibleExprs :: [Expr] -> [Expr]
possibleExprs [] = []
possibleExprs [x] = [x]
possibleExprs (x:xs) =
    let subExprs = possibleExprs xs
    in concatMap (\other -> map (\op -> Complex op x other) allOps) subExprs ++
       concatMap (\other -> map (\op -> Complex op other x) allOps) subExprs


solveExact :: Integral a => a -> Maybe Expr
solveExact x =
    let possibleSolutions = possibleExprs (toFourNumbers x)
    in case filter ((== 10) . eval) possibleSolutions of
        [] -> Nothing
        x:_ -> Just x


printAll :: (Integral a, Show a) => (a -> Maybe Expr) -> IO ()
printAll solver =
    printOne solver 0 where
        showSolution :: Maybe Expr -> String
        showSolution Nothing = "no solution"
        showSolution (Just x) = show x
        printOne :: (Integral a, Show a) => (a -> Maybe Expr) -> a -> IO ()
        printOne _ 10000 = pure ()
        printOne solver x = do
            putStrLn (show x ++ " -> " ++ showSolution (solver x))
            printOne solver (x + 1)


main :: IO ()
main = printAll solveExact
