import Text.Show.Functions
data Planeta = UnPlaneta{
    nombre::String,
    posicionEnElEspacio::Posicion,
    relacionTiempo::(Float->Float)
}deriving(Show)

urano = UnPlaneta{
    nombre="urano",
    posicionEnElEspacio=(1,2,3),
    relacionTiempo = (\anios->anios * 6.5)
    }

jupiter = UnPlaneta{
    nombre="jupiter",
    posicionEnElEspacio=(3,2,4),
    relacionTiempo = (\anios->anios * 9.2)
}
marte = UnPlaneta{
    nombre="marte",
    posicionEnElEspacio=(11,9,8),
    relacionTiempo = (\anios->anios * 6.2)
}


posicion :: Planeta->Posicion
posicion (UnPlaneta _ p _)= p
tiempo::Planeta->(Float->Float)
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float,Float,Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

data Astronauta = UnAstronauta{
    nombreAstronauta :: String,
    edadTerrestre :: Float,
    planetaEnDondeEsta:: Planeta
}deriving(Show)

diegui = UnAstronauta {
    nombreAstronauta = "Diegui",
    edadTerrestre= 20,
    planetaEnDondeEsta = jupiter
}
pipo = UnAstronauta {
    nombreAstronauta = "Pipo",
    edadTerrestre= 20,
    planetaEnDondeEsta = urano
}
pepe = UnAstronauta {
    nombreAstronauta = "Pepe",
    edadTerrestre= 10,
    planetaEnDondeEsta = jupiter
}

perdido1 = UnAstronauta {
    nombreAstronauta = "Perdido1",
    edadTerrestre= 10,
    planetaEnDondeEsta = urano
}
perdido2 = UnAstronauta {
    nombreAstronauta = "Perdido2",
    edadTerrestre= 89,
    planetaEnDondeEsta = marte
}

nombreAstro (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p

--1.a)
distanciaEntre :: Planeta->Planeta->Float
distanciaEntre unPlaneta otroPlaneta =
    (**(1/2)).sumaDeVectoresEntreCoordenadas unPlaneta $otroPlaneta 


sumaDeVectoresEntreCoordenadas::Planeta->Planeta->Float
sumaDeVectoresEntreCoordenadas unPlaneta otroPlaneta =
     (restaDeCoordenadasAlCuadrado (unaCoordenada 'z' unPlaneta) (unaCoordenada 'z' otroPlaneta)) + 
     (restaDeCoordenadasAlCuadrado (unaCoordenada 'y' unPlaneta) (unaCoordenada 'y' otroPlaneta)) +
     (restaDeCoordenadasAlCuadrado (unaCoordenada 'x' unPlaneta) (unaCoordenada 'x' otroPlaneta))
    

restaDeCoordenadasAlCuadrado :: Float->Float->Float
restaDeCoordenadasAlCuadrado unaCoordenada otraCoordenada =
    (**2)(unaCoordenada - otraCoordenada)


unaCoordenada:: Char->Planeta->Float
unaCoordenada coordenada unPlaneta 
    |coordenada == 'x' = coordX.posicion $ unPlaneta
    |coordenada == 'y' = coordY.posicion $ unPlaneta
    |coordenada == 'z' = coordZ.posicion $ unPlaneta

tiempoDeViajeEntre :: Planeta->Planeta->Float->Float
tiempoDeViajeEntre  unPlaneta otroPlaneta velocidad =
    (/velocidad).distanciaEntre unPlaneta $otroPlaneta

pasarTiempo::Astronauta->Float->Astronauta
pasarTiempo unAstronauta añosAPasar=
    modificarEdad unAstronauta.tiempo (planeta unAstronauta) $añosAPasar

modificarEdad:: Astronauta->Float->Astronauta
modificarEdad unAstronauta añosPasados=
    unAstronauta{edadTerrestre = (edad unAstronauta) + añosPasados}

type Nave = Planeta->Planeta->Float

naveVieja::Int->Nave
naveVieja tanquesDeOxigeno planetaDestino planetaOrigen 
    |tanquesDeOxigeno<6= 10
    |otherwise = 7

naveFuturista::Nave
naveFuturista planetaDestino planetaOrigen = 0

realizarViaje:: Nave->Planeta->Astronauta->Astronauta
realizarViaje unaNave planetaDestino unAstronauta =
    cambiarPlaneta planetaDestino.pasarTiempo unAstronauta.tiempoDeViajeEntre planetaDestino (planeta unAstronauta).unaNave planetaDestino.planeta $unAstronauta 

cambiarPlaneta::Planeta->Astronauta->Astronauta
cambiarPlaneta unPlaneta unAstronauta =
    unAstronauta{planetaEnDondeEsta = unPlaneta}

rescatarAUnAstronauta:: Astronauta->[Astronauta]->Nave->[Astronauta]
rescatarAUnAstronauta unAstronauta tripulacion unaNave  =
    map(tiempoDeEspera unaNave (planeta unAstronauta) (planeta.last $tripulacion)).agregarATripulacion unAstronauta $tripulacion


tiempoDeEspera:: Nave->Planeta->Planeta->Astronauta->Astronauta
tiempoDeEspera unaNave planetaDestino planetaOrigen  unAstronauta =
    modificarEdad unAstronauta.(2*).tiempoDeViajeEntre planetaDestino planetaOrigen.unaNave planetaDestino $planetaOrigen

agregarATripulacion::Astronauta->[Astronauta]->[Astronauta]
agregarATripulacion unAstronauta tripulacion = unAstronauta:tripulacion

rescateGrupal::[Astronauta]->[Astronauta]->Nave->[Astronauta]
rescateGrupal tripulacion astronautasVarados unaNave =
    filter(puedeSerRescatado unaNave tripulacion) astronautasVarados

puedeSerRescatado:: Nave->[Astronauta]->Astronauta->Bool
puedeSerRescatado unaNave tripulacion unAstronautaVarado =
    all(menorANoventa).rescatarAUnAstronauta unAstronautaVarado tripulacion $unaNave 

menorANoventa::Astronauta->Bool
menorANoventa unAstronautaVarado = (<90)(edad unAstronautaVarado)


f:: (Num b,Ord b)=>(b->a->b)->b->(Int->a->Bool)->[a]->Bool
f a b c = any((>b).a b).filter (c 10)

    


