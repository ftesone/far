import System.Directory
import System.Environment
import Data.List
import Utils
import Consulta
import AR



nombresRelaciones :: Consulta -> [String]
nombresRelaciones (Taux r) = [r]
nombresRelaciones (Sigma _ r) = nombresRelaciones r
nombresRelaciones (Pi _ r) = nombresRelaciones r
nombresRelaciones (Rho _ r) = nombresRelaciones r
nombresRelaciones (X r1 r2) = nombresRelaciones r1 ++ nombresRelaciones r2
nombresRelaciones (NX r1 r2) = nombresRelaciones r1 ++ nombresRelaciones r2
nombresRelaciones (U r1 r2) = nombresRelaciones r1 ++ nombresRelaciones r2
nombresRelaciones (I r1 r2) = nombresRelaciones r1 ++ nombresRelaciones r2
nombresRelaciones (D r1 r2) = nombresRelaciones r1 ++ nombresRelaciones r2



crearEstructurasRelacion :: [String] -> [String] -> [(String,String)]
crearEstructurasRelacion [] _ = []
crearEstructurasRelacion _ [] = []
crearEstructurasRelacion (n:ns) (t:ts) = (n,t):(crearEstructurasRelacion ns ts)

crearRelacionDeCsv :: String -> String -> RelacionBaseBD
crearRelacionDeCsv n csvStr =
    let
        csvLines = lines csvStr
        attrLine = head csvLines
        tupLines = tail csvLines
    in crearRelacionBD n (csvLineToList attrLine) (csvLineToList <$> tupLines)



main = do
    args <- getArgs
    if length args /= 2
        then error "El primer parámetro debe ser un directorio, el segundo la consulta"
        else do
            -- obtener argumentos, consulta y relaciones
            let dir = args !! 0
            let consulta = crearConsulta $ args !! 1
            let relaciones = nub $ nombresRelaciones consulta
            -- leer archivos a utilizar
            let archivos = (\fn -> dir ++ "/" ++ fn ++ ".csv") <$> relaciones
            tups <- mapM (\arch -> readFile arch) archivos
            -- crear relaciones y definir la BD
            let datosRelaciones = crearEstructurasRelacion relaciones tups
            let bd = (\dt -> crearRelacionDeCsv (fst dt) (snd dt)) <$> datosRelaciones
            -- ejecutar y mostrar la consulta
            putStr $ show $ ejecutarConsulta bd consulta
