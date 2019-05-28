import Text.Show.Functions
data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)
type Situacion = [Aspecto]

data Gema = UnaGema{
    nombre::String,
    fuerza::Int,
    personalidad::(Situacion->Situacion)
}deriving(Show)

gemaVidente = UnaGema{
    nombre="Gema Vidente",
    fuerza=2,
    personalidad = (vidente)
}
gemaRelajada::Float->Gema
gemaRelajada nivelDeRelajacion = UnaGema{
    nombre="Gema Relajada",
    fuerza=3,
    personalidad = (relajada nivelDeRelajacion)
}
vidente::Situacion->Situacion
vidente unaSituacion =
    modificarSituacion (modificarSituacion unaSituacion incertidumbre ((grado incertidumbre)/2)) tension ((grado tension)-10)

relajada::Float->Situacion->Situacion
relajada nivelDeRelajacion unaSituacion =
    modificarSituacion (modificarSituacion unaSituacion tension ((grado tension)-30)) peligro ((grado peligro)+nivelDeRelajacion)

tension=UnAspecto{
    tipoDeAspecto = "tension",
    grado= 1
}
incertidumbre=UnAspecto{
    tipoDeAspecto = "incertidumbre",
    grado= 1
}
peligro=UnAspecto{
    tipoDeAspecto = "peligro",
    grado= 1
}
mejorAspecto::Aspecto->Aspecto->Bool
mejorAspecto mejor peor = grado mejor > grado peor

mismoAspecto:: Aspecto->Aspecto->Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2

buscarAspecto :: Aspecto->Situacion->Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)

buscarAspectoDeTipo:: String->Situacion->Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)

reemplazarAspecto:: Situacion->Aspecto->Situacion
reemplazarAspecto situacion aspectoBuscado  =
    aspectoBuscado : (filter (not.mismoAspecto aspectoBuscado) situacion)
--1.a)
modificarAspecto::(Float->Float)->Aspecto->Aspecto
modificarAspecto funcion unAspecto=
    alterarGrado (funcion (grado unAspecto)) unAspecto

alterarGrado:: Float->Aspecto->Aspecto
alterarGrado cambio unAspecto =
    unAspecto{grado=cambio}

--b)

mejorSituacion:: Situacion->Situacion->Bool
mejorSituacion unaSituacion otraSituacion =
    all(aspectoEnSituacion otraSituacion) unaSituacion

aspectoEnSituacion::Situacion->Aspecto->Bool
aspectoEnSituacion unaSituacion unAspecto=
    mejorAspecto unAspecto.buscarAspecto unAspecto $unaSituacion
--Prueba
--mejorSituacion [(tension{tipoDeAspecto="tension",grado=1}),(peligro{tipoDeAspecto="peligro",grado=2})] [(tension{tipoDeAspecto="tension",grado=2}),(peligro{tipoDeAspecto="peligro",grado=3})]

--c)
modificarSituacion::Situacion->Aspecto->Float->Situacion
modificarSituacion unaSituacion aspectoAModificar gradoNuevo=
    reemplazarAspecto unaSituacion.alterarGrado gradoNuevo. buscarAspecto aspectoAModificar $unaSituacion

--3)
esMejorGema :: Gema->Gema->Situacion->Bool
esMejorGema unaGema otraGema unaSituacion =
    fuerza unaGema > fuerza otraGema &&  mejorSituacion (personalidad unaGema unaSituacion) (personalidad otraGema unaSituacion)

fusion:: Gema->Gema->Situacion->Gema
fusion unaGema otraGema unaSituacion = 
    UnaGema{
        nombre=nombreFusionado unaGema otraGema,
        fuerza=fuerzaFusionada unaSituacion unaGema otraGema,
        personalidad = personalidadFusionada unaGema otraGema 
    }

fuerzaFusionada::Situacion->Gema->Gema->Int
fuerzaFusionada unaSituacion unaGema otraGema
    |mejorSituacion (personalidadFusionada unaGema otraGema unaSituacion) (personalidad unaGema unaSituacion) && mejorSituacion (personalidadFusionada unaGema otraGema unaSituacion) (personalidad otraGema unaSituacion) = (fuerza unaGema + fuerza otraGema)*10
    |otherwise = (fuerzaDominante unaGema otraGema)*7

fuerzaDominante:: Gema->Gema->Int
fuerzaDominante unaGema otraGema
    |mayorFuerza unaGema otraGema = fuerza unaGema
    |otherwise = fuerza otraGema

mayorFuerza::Gema->Gema->Bool
mayorFuerza unaGema otraGema = fuerza unaGema > fuerza otraGema

nombreFusionado::Gema->Gema->String
nombreFusionado unaGema otraGema 
    |nombre unaGema == nombre otraGema = nombre unaGema
    |otherwise = nombre unaGema ++" "++ nombre otraGema

personalidadFusionada::Gema->Gema->Situacion->Situacion
personalidadFusionada unaGema otraGema unaSituacion =
    personalidad unaGema (personalidad otraGema (situacionMenosDiez unaSituacion))

situacionMenosDiez :: Situacion->Situacion
situacionMenosDiez unaSituacion = 
    map (alterarGrado (-10)) unaSituacion