import Text.Show.Functions()

main :: IO ()
main = return ()

type Receta = Pais -> Pais

data Pais = CrearPais {
        ingreso :: Float,
        poblacionActivaSectorPublico :: Float,
        poblacionActivaSectorPrivado :: Float,
        recursosNaturales :: [String],
        deudaConFmi :: Float
} deriving (Show)

namibia :: Pais
namibia = CrearPais 4140 4000000 650000 ["Mineria","Ecoturismo"] 50000000

zambia :: Pais
zambia = CrearPais 41000 4000 4000 (crearListaInfinita "Hola") 123245

-- Auxiliares

crearListaInfinita :: String -> [String]
crearListaInfinita unString = repeat unString

cambiarDeuda :: Float -> Receta
cambiarDeuda millonesDeDolares unPais = unPais { deudaConFmi = deudaConFmi unPais + millonesDeDolares}  

modificarTrabajoSectorPublico :: Float -> Receta
modificarTrabajoSectorPublico unaCantidad unPais = unPais { poblacionActivaSectorPublico = poblacionActivaSectorPublico unPais + unaCantidad}

modificarIngresoPerCapita :: Float -> Receta
modificarIngresoPerCapita unPorcentaje unPais = unPais {ingreso = ingreso unPais * unPorcentaje}

reducirIngresoPerCapita :: Receta
reducirIngresoPerCapita unPais 
    | poblacionActivaSectorPublico unPais > 100 = modificarIngresoPerCapita 0.8 unPais
    | otherwise = modificarIngresoPerCapita 0.85 unPais

eliminarRecursoNatural :: String -> Receta
eliminarRecursoNatural unRecursoNatural unPais = unPais {recursosNaturales = filter (/=unRecursoNatural).recursosNaturales $ unPais} 

calcularPBI :: Pais -> Float
calcularPBI unPais = ingreso unPais * (poblacionActivaSectorPrivado unPais + poblacionActivaSectorPublico unPais) 

--- Parcial 

prestamo :: Float -> Receta
prestamo millonesDeDolares unPais = cambiarDeuda ( 1.5 * millonesDeDolares) unPais

reducirTrabajoSectorPublico :: Float -> Receta
reducirTrabajoSectorPublico cantidadPuestosDeTrabajo unPais = reducirIngresoPerCapita.modificarTrabajoSectorPublico (-cantidadPuestosDeTrabajo) $ unPais

entregarRecursoNatural :: Receta
entregarRecursoNatural unPais = cambiarDeuda (-2000000).eliminarRecursoNatural (head.recursosNaturales $ unPais) $ unPais

blindaje :: Receta
blindaje unPais = modificarTrabajoSectorPublico (-500).cambiarDeuda (calcularPBI unPais) $ unPais 

prestarYExpropiarMineria :: Receta
prestarYExpropiarMineria unPais = eliminarRecursoNatural "Mineria".cambiarDeuda (-2000000) $ unPais

--- Punto 3.b prestarYExpropiarMineria namibia. 

paisesQueZafan :: [Pais] -> [Pais]
paisesQueZafan unosPaises = filter (any (=="Petroleo").recursosNaturales) $ unosPaises 

totalDeudaAFavorFmi :: [Pais] -> Float
totalDeudaAFavorFmi unosPaises = sum.map deudaConFmi $ unosPaises

estaOrdenadaDeMenorAMayor :: [Receta] -> Pais -> Bool
estaOrdenadaDeMenorAMayor [] _  = True
estaOrdenadaDeMenorAMayor (cabeza : cola) unPais 
        | calcularPBI unPais > (calcularPBI.cabeza $ unPais) = estaOrdenadaDeMenorAMayor cola (cabeza unPais)  
        | otherwise = False


--- Teoria
-- 1)El 4.a funciona, dado que any termina cuando encuentra un elemento que cumpla con su condicion, por lo tanto no espera a que termine de cargar la lista inifnita,
-- dado que trabajo de la forma lazy evaluation. Una vez que encuentra un elemento que haya cumplido con la condicion, devuelve True.

{--2) El 4.b tambien funcionado dado que la lista infinita de recursos no es necesaria para calcular la totalDeudaAFavorFmi , por lo tanto, no va a esperar
a que termine de cargar, simplemente cuando reciba el pais, va a calcular su deuda. 
--}