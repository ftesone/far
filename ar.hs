module AR
    (crearRelacion, ejecutar, ejecutarConsulta, RelacionBaseBD (..))
where

import BOp
import Predicado
import PredicadoSimple
import ExprBuilder
import Consulta
import Utils
import Data.List
import Control.Monad (liftM2)



data RelacionBaseBD = RelacionBD { nombreRelacion :: String, relacion :: RelacionBase }
type BD = [RelacionBaseBD]



-- funciones para ejecutar consultas
cargarRelacion :: BD -> String -> RelacionBase
cargarRelacion [] nombre = error ("No existe la relación "++nombre)
cargarRelacion (r:rs) nombre
    | nombreRelacion r == nombre = relacion r
    | otherwise = cargarRelacion rs nombre

esAtributo :: String -> Atributo -> Bool
esAtributo s a = s == a || s == (nombreAtributo a)

atributo :: Atributo -> [Atributo] -> Atributo
atributo att [] = error ("No existe el atributo '"++att++"'")
atributo att (a:as) = if nombreAtributo att == nombreAtributo a then a else atributo att as

atributoTupla :: Atributo -> [Atributo] -> Tupla -> Atributo
atributoTupla att []  _ = error ("No existe el atributo '"++att++"'")
atributoTupla att (a:as) (f:fs) = if esAtributo att a then f else atributoTupla att as fs

atributosTupla :: [Atributo] -> [Atributo] -> Tupla -> [Atributo]
atributosTupla [] _ _ = []
atributosTupla (a:as) atts tups = (atributoTupla a atts tups):(atributosTupla as atts tups)



ejecutarConsulta :: BD -> Consulta -> Consulta
ejecutarConsulta bd q =
    let
        relacion r = Tau $ cargarRelacion bd r

        seleccion p (Tau r) =
            let
                fc tup = \bop c1 c2 -> resolverBOp bop (atributoTupla c1 (atributos r) tup) (atributoTupla c2 (atributos r) tup)
                fv tup = \bop c v -> resolverBOp bop (atributoTupla c (atributos r) tup) v
                fp tup = resolverPredicado (fc tup) (fv tup) p
            in
                Tau (Relacion (atributos r) (filter (\tup -> fp tup) (tuplas r)))
        proyeccion fs (Tau r) = Tau $ Relacion ((\c -> atributo c (atributos r)) <$> fs) (nub $ (\tup -> atributosTupla fs (atributos r) tup) <$> (tuplas r))

        renombre n (Tau r) = Tau $ Relacion ((\c -> n ++ (dropWhile (/='.') c)) <$> (atributos r)) (tuplas r)

        nuevoNombreRelacion r1 r2 = nombreRelacion r1 ++ "_" ++ nombreRelacion r2

        prodCartesiano (Tau r1) (Tau r2) = Tau $ Relacion ((atributos r1) ++ (atributos r2)) (liftM2 (++) (tuplas r1) (tuplas r2))

        prodNatural (Tau r1) (Tau r2) = seleccion (foldr1 And [ Simple (Col Eq i j) | i <- atributos r1, j <- atributos r2, nombreAtributo i == nombreAtributo j ]) (prodCartesiano (Tau r1) (Tau r2))

        dominiosCompatibles r1 r2 = length (atributos r1) == length [ i | i <- (atributos r1) , j <- (atributos r2), (nombreAtributo i) == (nombreAtributo j) ]

        union (Tau r1) (Tau r2) | dominiosCompatibles r1 r2 = Tau $ Relacion (nombreAtributo <$> atributos r1) (nub $ (tuplas r1) ++ (tuplas r2))
                                | otherwise = error "Unión: dominios distintos"

        interseccion (Tau r1) (Tau r2) | dominiosCompatibles r1 r2 = Tau $ Relacion (nombreAtributo <$> atributos r1) [ i | i <- tuplas r1, j <- tuplas r2, i == j]
                                       | otherwise = error "Intersección: dominios distintos"

        diferencia (Tau r1) (Tau r2) | dominiosCompatibles r1 r2 = Tau $ Relacion (nombreAtributo <$> atributos r1) $ filter (not . flip elem (tuplas r2)) $ tuplas r1

    in foldConsulta (relacion) (Tau) (seleccion) (proyeccion) (renombre) (prodCartesiano) (prodNatural) (union) (interseccion) (diferencia) q



-- funciones para crear la BD
crearRelacion :: String -> [String] -> [[String]] -> RelacionBaseBD
crearRelacion n c t = RelacionBD n (Relacion c t)


-- función para ejecutar consultas sobre BD
ejecutar :: BD -> String -> Consulta
ejecutar bd q = ejecutarConsulta bd $ read q :: Consulta
