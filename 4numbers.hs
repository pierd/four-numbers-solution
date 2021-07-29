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


splits :: [a] -> [([a], [a])]
splits l = ([], l) : nextsplit [] l where
    nextsplit :: [a] -> [a] -> [([a], [a])]
    nextsplit _ [] = []
    nextsplit left (h:t) = (left ++ [h], t) : nextsplit (left ++ [h]) t


permutations :: [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
permutations (x:xs) =
    let
        subPermutations = permutations xs
        joinSplits (left, right) = left ++ [x] ++ right
        insertInBetween lst = map joinSplits (splits lst)
    in concatMap insertInBetween subPermutations


possibleExprs :: [Expr] -> [Expr]
possibleExprs l = concatMap possibleExprsInOrder (permutations l) where
    possibleExprsInOrder :: [Expr] -> [Expr]
    possibleExprsInOrder [] = []
    possibleExprsInOrder [x] = [x]
    possibleExprsInOrder (x:xs) =
        let subExprs = possibleExprsInOrder xs
        in concatMap (\other -> map (\op -> Complex op x other) allOps) subExprs ++
           concatMap (\other -> map (\op -> Complex op other x) allOps) subExprs


solveExact :: Integral a => a -> Maybe Expr
solveExact x =
    let possibleSolutions = possibleExprs (toFourNumbers x)
    in case filter ((== 10) . eval) possibleSolutions of
        [] -> Nothing
        x:_ -> Just x


solveClosest :: Integral a => a -> Maybe Expr
solveClosest x =
    foldr cons Nothing possibleSolutions where
        possibleSolutions = possibleExprs (toFourNumbers x)
        cons :: Expr -> Maybe Expr -> Maybe Expr
        cons candidate Nothing = Just candidate
        cons candidate (Just best) =
            case (abs (10 - eval candidate), abs (10 - eval best)) of
                (c, b) -> Just (if isNaN b || c < b then candidate else best)


printAll :: (Integral a, Show a) => (a -> Maybe Expr) -> IO ()
printAll solver =
    printOne solver 0 where
        showSolution :: Maybe Expr -> String
        showSolution Nothing = "no solution"
        showSolution (Just x) = show (eval x) ++ " = " ++ show x
        printOne :: (Integral a, Show a) => (a -> Maybe Expr) -> a -> IO ()
        printOne _ 10000 = pure ()
        printOne solver x = do
            putStrLn (show x ++ " -> " ++ showSolution (solver x))
            printOne solver (x + 1)


main :: IO ()
main = printAll solveClosest
