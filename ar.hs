module AR
    (crearTabla, ejecutar, ejecutarConsulta, TablaBaseBD (..))
where

import BOp
import Predicado
import PredicadoSimple
import ExprBuilder
import Consulta
import Utils
import Data.List
import Control.Monad (liftM2)



data TablaBaseBD = TablaBD { nombreTabla :: String, tabla :: TablaBase }
type BD = [TablaBaseBD]



-- funciones para ejecutar consultas
cargarTabla :: BD -> String -> TablaBase
cargarTabla [] nombre = error ("No existe la tabla "++nombre)
cargarTabla (t:ts) nombre
    | nombreTabla t == nombre = tabla t
    | otherwise = cargarTabla ts nombre

esColumna :: String -> Columna -> Bool
esColumna s c = s == c || s == (nombreColumna c)

columna :: Columna -> [Columna] -> Columna
columna col [] = error ("No existe la columna '"++col++"'")
columna col (c:cs) = if nombreColumna col == nombreColumna c then c else columna col cs

columnaTupla :: Columna -> [Columna] -> Tupla -> Atributo
columnaTupla col []  _ = error ("No existe la columna '"++col++"'")
columnaTupla col (c:cs) (f:fs) = if esColumna col c then f else columnaTupla col cs fs

columnasTupla :: [Columna] -> [Columna] -> Tupla -> [Atributo]
columnasTupla [] _ _ = []
columnasTupla (c:cs) cols tups = (columnaTupla c cols tups):(columnasTupla cs cols tups)



ejecutarConsulta :: BD -> Consulta -> Consulta
ejecutarConsulta bd q =
    let
        tabla t = Tau $ cargarTabla bd t

        seleccion p (Tau t) =
            let
                fc tup = \bop c1 c2 -> resolverBOp bop (columnaTupla c1 (columnas t) tup) (columnaTupla c2 (columnas t) tup)
                fv tup = \bop c v -> resolverBOp bop (columnaTupla c (columnas t) tup) v
                fp tup = resolverPredicado (fc tup) (fv tup) p
            in
                Tau (Tabla (columnas t) (filter (\tup -> fp tup) (tuplas t)))
        proyeccion fs (Tau t) = Tau $ Tabla ((\c -> columna c (columnas t)) <$> fs) (nub $ (\tup -> columnasTupla fs (columnas t) tup) <$> (tuplas t))

        nuevoNombreTabla t1 t2 = nombreTabla t1 ++ "_" ++ nombreTabla t2

        prodCartesiano (Tau t1) (Tau t2) = Tau $ Tabla ((columnas t1) ++ (columnas t2)) (liftM2 (++) (tuplas t1) (tuplas t2))

        prodNatural (Tau t1) (Tau t2) = seleccion (foldr1 And [ Simple (Col Eq i j) | i <- columnas t1, j <- columnas t2, nombreColumna i == nombreColumna j ]) (prodCartesiano (Tau t1) (Tau t2))

        dominiosCompatibles t1 t2 = length (columnas t1) == length (columnas t2) && length (columnas t1) == length [ i | i <- (columnas t1) , j <- (columnas t2), (nombreColumna i) == (nombreColumna j) ]

        union (Tau t1) (Tau t2) | dominiosCompatibles t1 t2 = Tau $ Tabla (nombreColumna <$> columnas t1) (nub $ (tuplas t1) ++ (tuplas t2))
                                | otherwise = error "Unión: dominios distintos"

        interseccion (Tau t1) (Tau t2) | dominiosCompatibles t1 t2 = Tau $ Tabla (nombreColumna <$> columnas t1) [ i | i <- tuplas t1, j <- tuplas t2, i == j]
                                       | otherwise = error "Intersección: dominios distintos"

        diferencia (Tau t1) (Tau t2) | dominiosCompatibles t1 t2 = Tau $ Tabla (nombreColumna <$> columnas t1) $ filter (not . flip elem (tuplas t2)) $ tuplas t1

    in foldConsulta (tabla) (Tau) (seleccion) (proyeccion) (prodCartesiano) (prodNatural) (union) (interseccion) (diferencia) q



-- funciones para crear la BD
crearTabla :: String -> [String] -> [[String]] -> TablaBaseBD
crearTabla n c t = TablaBD n (Tabla c t)


-- función para ejecutar consultas sobre BD
ejecutar :: BD -> String -> Consulta
ejecutar bd q = ejecutarConsulta bd $ read q :: Consulta
