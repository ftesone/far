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



main = do
    args <- getArgs
    if length args /= 2
        then error "El primer parámetro debe ser un directorio, el segundo la consulta"
        else do
            let (dir,consulta) = (head args, crearConsulta $ head $ tail args) -- (String, Consulta)
            let tablas = nombresRelaciones consulta -- ([String])
            let archivos = (\fn -> dir ++ "/" ++ fn ++ ".csv") <$> tablas -- [(String, String)]
            tups <- mapM (\arch -> readFile arch) archivos
            let datosRelaciones = crearEstructurasRelacion tablas tups
            let bd = (\dt -> crearRelacion (fst dt) ((\c -> (fst dt)++"."++c) <$> (csvLineToList $ head $ lines $ snd dt)) (csvLineToList <$> (tail $ lines $ snd dt))) <$> datosRelaciones
            putStr $ show $ ejecutarConsulta bd consulta
