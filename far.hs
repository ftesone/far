import System.Directory
import System.Environment
import Data.List
import Utils
import Consulta
import AR



nombresTablas :: Consulta -> [String]
nombresTablas (Taux t) = [t]
nombresTablas (Sigma _ t) = nombresTablas t
nombresTablas (Pi _ t) = nombresTablas t
nombresTablas (Rho _ t) = nombresTablas t
nombresTablas (X t1 t2) = nombresTablas t1 ++ nombresTablas t2
nombresTablas (NX t1 t2) = nombresTablas t1 ++ nombresTablas t2
nombresTablas (U t1 t2) = nombresTablas t1 ++ nombresTablas t2
nombresTablas (I t1 t2) = nombresTablas t1 ++ nombresTablas t2
nombresTablas (D t1 t2) = nombresTablas t1 ++ nombresTablas t2



crearEstructurasTabla :: [String] -> [String] -> [(String,String)]
crearEstructurasTabla [] _ = []
crearEstructurasTabla _ [] = []
crearEstructurasTabla (n:ns) (t:ts) = (n,t):(crearEstructurasTabla ns ts)



main = do
    args <- getArgs
    if length args /= 2
        then error "El primer parámetro debe ser un directorio, el segundo la consulta"
        else do
            let (dir,consulta) = (head args, crearConsulta $ head $ tail args) -- (String, Consulta)
            let tablas = nombresTablas consulta -- ([String])
            let archivos = (\fn -> dir ++ "/" ++ fn ++ ".csv") <$> tablas -- [(String, String)]
            tups <- mapM (\arch -> readFile arch) archivos
            let datosTablas = crearEstructurasTabla tablas tups
            let bd = (\dt -> crearTabla (fst dt) ((\c -> (fst dt)++"."++c) <$> (csvLineToList $ head $ lines $ snd dt)) (csvLineToList <$> (tail $ lines $ snd dt))) <$> datosTablas
            putStr $ show $ ejecutarConsulta bd consulta
