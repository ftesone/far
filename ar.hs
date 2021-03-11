module AR
    (crearRelacionBD, ejecutar, ejecutarConsulta, RelacionBaseBD (..))
where

import BOp
import Predicado
import PredicadoSimple
import ExprBuilder
import Consulta
import Utils
import Data.List



data RelacionBaseBD = RelacionBD { nombreRelacion :: String, relacion :: RelacionBase }
type BD = [RelacionBaseBD]



-- funciones para ejecutar consultas
cargarRelacion :: BD -> String -> RelacionBase
cargarRelacion [] nombre = error ("No existe la relación "++nombre)
cargarRelacion (r:rs) nombre
    | nombreRelacion r == nombre = relacion r
    | otherwise = cargarRelacion rs nombre

atributo :: Atributo -> [(Atributo, Atributo)] -> (Atributo, Atributo)
atributo att [] = error ("No existe el atributo '"++att++"'")
atributo att (a:as) = if att == fst a || att == snd a then a else atributo att as

atributoTupla :: Atributo -> [(Atributo, Atributo)] -> Tupla -> Atributo
atributoTupla att []  _ = error ("No existe el atributo '"++att++"'")
atributoTupla att (a:as) (f:fs) = if att == fst a || att == snd a then f else atributoTupla att as fs

posAtributoTupla :: Atributo -> [(Atributo,Atributo)] -> Int
posAtributoTupla att [] = error ("No existe el atributo '"++att++"'")
posAtributoTupla att as =
    let
        posAtributoTupla' n att [] = error ("No existe el atributo '"++att++"'")
        posAtributoTupla' n att (a:as) = if att == fst a || att == snd a then n else posAtributoTupla' (n+1) att as
    in posAtributoTupla' 0 att as

atributosTupla :: [Atributo] -> [(Atributo, Atributo)] -> Tupla -> [Atributo]
atributosTupla [] _ _ = []
atributosTupla atts enc tup =
    let
        poss = (flip posAtributoTupla enc) <$> atts
        posAtributosTupla [] _ = []
        posAtributosTupla (p:ps) tup = (tup !! p):(posAtributosTupla ps tup)
    in posAtributosTupla poss tup
    -- (atributoTupla a as tup):(atributosTupla po as tup)



ejecutarConsulta :: BD -> Consulta -> Consulta
ejecutarConsulta bd q =
    let
        relacion r = Tau $ cargarRelacion bd r

        -- funciones para resolver
        fc ats tup = \bop c1 c2 -> resolverBOp bop (atributoTupla c1 ats tup) (atributoTupla c2 ats tup)
        fv ats tup = \bop c v -> resolverBOp bop (atributoTupla c ats tup) v
        fp ats tup p = resolverPredicado (fc ats tup) (fv ats tup) p

        seleccion p (Tau r) = Tau (Relacion (atributos r) (filter (\tup -> fp (atributos r) tup p) (tuplas r)))

        proyeccion as (Tau r) = Tau $ Relacion ((\a -> atributo a (atributos r)) <$> as) (nub $ (\tup -> atributosTupla as (atributos r) tup) <$> (tuplas r))

        renombre n (Tau r) = Tau $ Relacion ((\c -> (n++"."++(snd c), snd c)) <$> (atributos r)) (tuplas r)

        nuevoNombreRelacion r1 r2 = nombreRelacion r1 ++ "_" ++ nombreRelacion r2

        prodCartesiano (Tau r1) (Tau r2) = Tau $ Relacion ((atributos r1) ++ (atributos r2)) ((++) <$> (tuplas r1) <*> (tuplas r2))

        prodNatural (Tau r1) (Tau r2) =
            let
                appendCond p t1 [] = []
                appendCond p t1 (t2:t2s) = let t = t1++t2 in if p t then t:(appendCond p t1 t2s) else appendCond p t1 t2s
                prodCartesianoCond p [] _ = []
                prodCartesianoCond p (t1:t1s) t2s = (appendCond p t1 t2s)++(prodCartesianoCond p t1s t2s)
                predicadoAttrsComun = foldr1 And [ Simple (Col Eq (fst i) (fst j)) | i <- atributos r1, j <- atributos r2, (snd i) == (snd j) ]
                atributosRs = (atributos r1)++(atributos r2)
            in Tau $ Relacion (atributosRs) (prodCartesianoCond (\t -> fp (atributosRs) t predicadoAttrsComun) (tuplas r1) (tuplas r2))

        dominiosCompatibles r1 r2 = length (atributos r1) == length [ i | i <- (atributos r1) , j <- (atributos r2), (snd i) == (snd j) ]

        union (Tau r1) (Tau r2) | dominiosCompatibles r1 r2 = Tau $ Relacion ((\a -> (snd a, snd a)) <$> (atributos r1)) (nub $ (tuplas r1) ++ (tuplas r2))
                                | otherwise = error "Unión: dominios distintos"

        interseccion (Tau r1) (Tau r2) | dominiosCompatibles r1 r2 = Tau $ Relacion ((\a -> (snd a, snd a)) <$> (atributos r1)) [ i | i <- tuplas r1, j <- tuplas r2, i == j]
                                       | otherwise = error "Intersección: dominios distintos"

        diferencia (Tau r1) (Tau r2) | dominiosCompatibles r1 r2 = Tau $ Relacion ((\a -> (snd a, snd a)) <$> (atributos r1)) $ filter (not . flip elem (tuplas r2)) $ tuplas r1

    in foldConsulta (relacion) (Tau) (seleccion) (proyeccion) (renombre) (prodCartesiano) (prodNatural) (union) (interseccion) (diferencia) q



-- funciones para crear la BD
crearRelacionBD :: String -> [String] -> [[String]] -> RelacionBaseBD
crearRelacionBD n a t = RelacionBD n $ crearRelacion ((\s -> (n++"."++s, s)) <$> a) t


-- función para ejecutar consultas sobre BD
ejecutar :: BD -> String -> Consulta
ejecutar bd q = ejecutarConsulta bd $ read q :: Consulta
