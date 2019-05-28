import Text.Show.Functions
data Persona = UnaPersona{
    nombre::String,
    dinero::Float,
    suerte::Int,
    factores::[(String,Int)]
}deriving(Show)

data Juego = UnJuego{
    nombreJuego::String,
    gananciaAPartirDeMonto :: Float->Float,
    criterios:: [(Persona->Bool)]
}deriving(Show)


ruleta= UnJuego{
    nombreJuego="ruleta",
    gananciaAPartirDeMonto = gananciaConApuesta,
    criterios = [(tieneSuerte (>80))]
}

maquinita::Float->Juego
maquinita jackpot= UnJuego{
    nombreJuego="Maquinita",
    gananciaAPartirDeMonto = gananciaConJackpot jackpot,
    criterios = [(tieneSuerte (>95)),(tienePaciencia)]
}

--Personas de ejemplo

diegui= UnaPersona{
    nombre="Diego",
    dinero= 5,
    suerte=9,
    factores=[("amuleto",10),("manos magicas",100)]
}

pipo= UnaPersona{
    nombre="Pipo",
    dinero= 10,
    suerte=96,
    factores=[("inteligencia",1),("paciencia",100)]
}

gananciaConApuesta :: Float->Float
gananciaConApuesta apuesta = apuesta * 37

gananciaConJackpot :: Float->Float->Float
gananciaConJackpot jackpot apuesta = jackpot + apuesta

suerteTotalDe :: Persona->Int
suerteTotalDe unaPersona 
    |any(tiene "amuleto") (factores unaPersona) = suerteAumentada unaPersona 
    |otherwise = suerte unaPersona

tiene :: String->(String,Int)->Bool
tiene factor (x,y) = x==factor && y>0

tienePaciencia :: Persona->Bool
tienePaciencia unaPersona =
    any (tiene "paciencia") (factores unaPersona)

suerteAumentada:: Persona->Int
suerteAumentada unaPersona =
    (*suerte unaPersona).(!!0).map (snd).filter (tiene "amuleto") $(factores unaPersona)

tieneSuerte :: (Int->Bool)->Persona->Bool
tieneSuerte funcion= funcion.suerteTotalDe

puedeGanar::Juego->Persona->Bool
puedeGanar unJuego unaPersona=
    all (cumpleCriterio unaPersona) (criterios unJuego)

cumpleCriterio:: Persona->(Persona->Bool)->Bool
cumpleCriterio unaPersona criterioDeJuego =
    criterioDeJuego unaPersona


jugadasSucesivas::Persona->Float->[Juego]->Float
jugadasSucesivas unaPersona apuesta listaDeJuegos=
    foldl (ganancia unaPersona) apuesta listaDeJuegos

ganancia:: Persona->Float->Juego->Float
ganancia unaPersona apuesta unJuego
    |puedeGanar unJuego unaPersona = gananciaAPartirDeMonto unJuego apuesta
    |otherwise = apuesta


jugadasSucesivasConRecursividad:: Persona->Float->[Juego]->Float
jugadasSucesivasConRecursividad unaPersona apuesta []= apuesta
jugadasSucesivasConRecursividad unaPersona apuesta listaDeJuegos=
    jugadasSucesivasConRecursividad unaPersona (ganancia unaPersona apuesta (head listaDeJuegos)) (tail listaDeJuegos)

--Ejemplo de uso
-- jugadasSucesivasConRecursividad diegui 5 [ruleta,(maquinita 5),ruleta] ==> 6845.0

jugadoresQueNoPeganUna::[Persona]->[Juego]->[Persona]
jugadoresQueNoPeganUna listaPersonas listaJuegos =
    foldl (noPuedenGanarJuegos) listaPersonas listaJuegos

noPuedenGanarJuegos::[Persona]->Juego->[Persona]
noPuedenGanarJuegos listaPersonas unJuego=
    filter(not.puedeGanar unJuego) listaPersonas


juegaConApuesta::Persona->Juego->Float->Persona
juegaConApuesta unaPersona unJuego apuesta=
    seGanaOSePierde (personaAposto unaPersona apuesta) unJuego apuesta

seGanaOSePierde::Persona->Juego->Float->Persona
seGanaOSePierde unaPersona unJuego apuesta
    |puedeGanar unJuego unaPersona = personaConGanancia unaPersona unJuego apuesta
    |otherwise = unaPersona

personaConGanancia :: Persona -> Juego -> Float ->Persona
personaConGanancia unaPersona unJuego apuesta =
    unaPersona{dinero = (dinero unaPersona) + ganancia unaPersona apuesta unJuego}

personaAposto :: Persona->Float->Persona
personaAposto unaPersona apuesta =
    unaPersona{dinero=(dinero unaPersona)-apuesta}

