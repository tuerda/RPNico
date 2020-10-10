-- Calculadora RPN interactiva en haskell.
-- Hace cálculos RPN manipulando múltiples stacks.

-- import Control.Monad.State -- para tratar como estado los stacks ocultos
import Data.List (findIndex)
import Control.Monad (when)
import System.Exit (exitSuccess)
import System.Environment (getArgs)
import Text.Read (readMaybe)

----------------------
-- tipos
----------------------

data ShowMode = YesShow | NoShow deriving Eq
type Stack = [String]
type Linea = [String] -- una linea es lo que sucede cuando le aplicamos words a una entrada.
data Variable = Variable { getName :: String
                         , getValue :: Stack
                         }
type Vars = [Variable] -- El estado son stacks y sus nombres.

----------------------
-- ciclo principal
----------------------

main = do
    putStrLn "RPN calculation language"
    putStrLn "Nicolás Kuschinski"
    ciclo [ Variable "main" [] ] YesShow --Empezamos con una variable llamada "main" que tiene un stack vacío

ciclo :: Vars -> ShowMode -> IO()
ciclo vars mode = do
    linea <- getLine
    when (linea == "s") $ ciclo vars YesShow
    when (linea == "-s") $ ciclo vars NoShow
    when (linea == "q") exitSuccess
    let estadoAhora = manejar (words linea) vars
    when (mode == YesShow) $ imprimir estadoAhora -- Muestra el primer stack o un error
    case estadoAhora of
         Nothing -> ciclo vars mode
         (Just xs) -> ciclo xs mode

imprimir :: Maybe Vars -> IO()
imprimir Nothing = putStrLn "Unrecognized command"
imprimir (Just x) = putStrLn . unwords . getValue . head $ x

---------------------------------------------
-- Procesar ordenes
---------------------------------------------

-- manejar procesará las ordenes
manejar :: Linea -> Vars -> Maybe Vars
manejar [] vars = comprimeEsto [] vars

manejar linea vars
    | esOrden $ last linea = Just $ handler linea vars
    | esValido $ last linea = comprimeEsto linea vars -- Si no terminamos con una orden entonces solo procesamos como calculadora.
    | otherwise = Nothing

handler :: Linea -> Vars -> Vars --Recibimos las instrucciones, dividimos en orden (si hay) y sobre qué operamos
handler linea vars =
    let orden      = last linea
        operando   = case linea of
                          (x:[]) -> Nothing
                          xs -> Just $ init xs
    in procesar orden operando vars

procesar :: String -> Maybe [String] -> Vars -> Vars
procesar _ Nothing vars = vars -- si no viene completa la orden, no se hace nada.
procesar "copy" (Just (nombre:_)) vars = copiar nombre vars
procesar "name" (Just (nombrenuevo:_)) vars = nombrar nombrenuevo vars
procesar "swap" (Just (nombre:_)) vars = intercambiar nombre vars
procesar "del" (Just (nombre:_)) vars = borrar nombre vars
procesar "noeval" (Just restoDeLinea) vars = (Variable nombreCabeza cuerpo) : tail vars -- solo agregar sin evaluaar nada.
    where
    nombreCabeza = getName $ head vars
    cuerpo = (getValue $ head vars) ++ restoDeLinea

----------------------------------------------
-- Las operaciones sobre el stack
----------------------------------------------

buscarIndice :: String -> Vars -> Maybe Int
buscarIndice st vars = findIndex (\x -> (getName x) == st) vars

-- pegar el stack nombrado en frente del stack actual
copiar :: String -> Vars -> Vars
copiar st vars = case buscarIndice st vars of
                      Nothing -> vars
                      (Just index) -> (Variable (getName $ head vars) ((getValue $ head vars) ++ (getValue $ vars !! index))) : tail vars

-- nombrar el stack actual
nombrar :: String -> Vars -> Vars
nombrar st vars = Variable st (getValue $ head vars) : tail vars

-- intercambiar el stack actual con el stack nombrado (si se nombra uno inexistente se inicia uno nuevo)
intercambiar :: String -> Vars -> Vars
intercambiar st vars = case buscarIndice st vars of
                      Nothing -> Variable st [] : vars
                      (Just index) -> (vars !! index) : (take index vars) ++ (drop (index + 1) vars)

-- borrar el stack nombrado (por ejemplo para liberar el nombre
borrar :: String -> Vars -> Vars
borrar st vars = case buscarIndice st vars of
                      Nothing -> vars
                      (Just index) -> (take index vars) ++ (drop (index + 1) vars)


--------------------------------------------------------
-- La parte que es calculadora
--------------------------------------------------------

-- Realiza los cálculos en un stack específico:
-- Pegar las ordenes al stack antes de llamar.
compressStack :: Stack -> Stack
compressStack = reverse . foldl compressor []
    where
    compressor (x:xs) "move" = moveUp (read x) xs -- Mover n elementos del stack al inicio. Para uso avanzado.
    compressor xs "sum" = show (sum $ map read' xs) : []
    compressor xs "prod" = show (product (map read' xs)) : []
    compressor xs "mean" = show (mean $ map read' xs) : []
    compressor xs "var" = show (var $ map read' xs) : []
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

--------------------------------------------------------
-- Funciones auxiliares para esto:
--------------------------------------------------------

-- Pasa la el primer getValue de un Vars por compressStack
comprimeEsto :: Linea -> Vars -> Maybe Vars
comprimeEsto linea vars = Just $ Variable nombre comprimido : tail vars
    where
    nombre = getName $ head vars
    comprimido = compressStack $ (getValue $ head vars) ++ linea


-- forzar a usar punto flotante al convertir a números
read' :: String-> Float
read' = read

-- Lo anterior obliga a definir división entera y módulos en puntos flotantes
x `div'` y = truncate x `div` truncate y
x `mod'` y = truncate x `mod` truncate y

mean xs = sum xs / (realToFrac $ length xs)

var xs = mean $ map (\x -> (x - laMedia) ** 2) xs -- Divide entre n, no entre (n-1)
    where laMedia = mean xs

moveUp :: Int -> [String] -> [String]
moveUp n st = drop n st ++ take n st

esNumero :: String -> Maybe Double -- Manera de verificar si un string corresponde con un número
esNumero x = readMaybe x

esOrden x = x `elem` ["copy", "name", "swap", "del", "noeval"] -- Las ordenes para operar sobre stacks y no sobre números

esValido x = (x `elem` ["move", "sum", "prod", "mean", "var", "!", "log", "sin", "sin", "cos", "tan", "cot", "exp", "trunc", "+", "-", "/", "//", "%", "*", "x", "^", "**", "pi", "e"]) || (esNumero x /= Nothing) -- números y ordenes para operar sobre ellos.
