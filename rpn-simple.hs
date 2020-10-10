-- Calculadora RPN interactiva en haskell.
import Control.Monad (when)
import System.Exit (exitSuccess)

data ShowMode = YesShow | NoShow deriving Eq

main = do
    putStrLn "RPNico interactive RPN calculator, 2020, Nicolás Kuschinski"
    putStrLn "Simple version"
    putStrLn "Type what you want to calculate, RPN notation; close with q"
    putStrLn "To turn off showing the current state of the stack, type \"-s\", to activate, use \"s\""
    ciclo [] YesShow

ciclo :: [String] -> ShowMode -> IO()
ciclo stack mode = do
    linea <- getLine
    when (linea == "s") $ ciclo stack YesShow
    when (linea == "-s") $ ciclo stack NoShow
    when (linea == "q") exitSuccess
    let stackahora = compressStack $ stack ++ words linea
    when (mode == YesShow) $ putStrLn . unwords $ stackahora
    ciclo stackahora mode

-- forzar a usar punto flotante al convertir a números
read' :: String-> Float
read' = read

-- Lo anterior obliga a definir división entera y módulos en puntos flotantes
x `div'` y = truncate x `div` truncate y
x `mod'` y = truncate x `mod` truncate y

-- Pegar las ordenes al stack y luego ejecutar todo el chow.

-- El motor del programa, un compresor de strings
compressStack :: [String] -> [String]
compressStack = reverse . foldl compressor []
    where
    compressor xs "sum" = show (sum $ map read' xs) : []
    compressor xs "prod" = show (product (map read' xs)) : []
    compressor xs "mean" = show (mean (map read' xs)) : []
    compressor xs "var" = show (var (map read' xs)) : []
    compressor (x:xs) u -- operaciones unarias
        | u == "!" = show (product [1..(truncate $ read' x)]) :xs
        | u == "log" = show (log $read' x) :xs
        | u == "sin" = show (sin $read' x) :xs
        | u == "cos" = show (cos $read' x) :xs
        | u == "tan" = show (tan $read' x) :xs
        | u == "cot" = show ((/) 1 $ tan . read' $x) :xs
        | u == "exp" = show (exp $read' x) :xs
        | u == "trunc" = show (truncate $read x) :xs
    compressor (x:y:xs) b -- operaciones binarias
        | b == "+"  = show (read' y + read' x) : xs
        | b == "-"  = show (read' y - read' x) : xs
        | b == "/"  = show (read' y / read' x) : xs
        | b == "//" = show (read y `div'` read x) : xs
        | b == "%"  = show (read y `mod'` read x) : xs
        | b == "*" || b=="x"  = show (read' y * read' x) : xs
        | b == "^" || b=="**" = show (read' y ** read' x) : xs
    compressor nums "pi" = show pi:nums
    compressor nums "e"  = show (exp 1) : nums
    compressor nums num  = num:nums

moveUp :: Int -> [String] -> [String]
moveUp n st = drop n st ++ take n st

mean xs = sum xs / (realToFrac $ length xs)

var xs = mean $ map (\x -> (x - laMedia) **2) xs -- Divide entre n, no entre (n-1)
    where laMedia = mean xs
