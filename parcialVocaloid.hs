import Text.Show.Functions

data Cantante = UnCantante{
    nombre::String,
    repertorio :: [Cancion],
    fama::Int
}deriving(Show,Eq)

data Cancion = UnaCancion{
    titulo::String,
    duracion::Int
}deriving(Show,Eq)

megurineLuka = UnCantante{
    nombre= "megurineLuka",
    repertorio = [(UnaCancion{titulo="nightFever", duracion=4}),(UnaCancion{titulo="foreverYoung", duracion=5})],
    fama=0
}
hatsuneMiku = UnCantante{
    nombre= "hatsuneMiku",
    repertorio = [(UnaCancion{titulo="tellYourWorld", duracion=4})],
    fama=5100
}
gumi = UnCantante{
    nombre= "gumi",
    repertorio = [(UnaCancion{titulo="tellYourWorld", duracion=5}),(UnaCancion{titulo="foreverYoung", duracion=4})],
    fama=0
}
seeU = UnCantante{
    nombre= "seeU",
    repertorio = [(UnaCancion{titulo="novemberRain", duracion=6}),(UnaCancion{titulo="nightFever", duracion=5})],
    fama=0
}
kaito = UnCantante{
    nombre= "kaito",
    repertorio = [],
    fama=0
}

esNovedoso :: Cantante->Bool
esNovedoso unCantante = cantidadDeCancionesQueSabe unCantante && duracionTotalDeCanciones unCantante 15

cantidadDeCancionesQueSabe :: Cantante->Bool
cantidadDeCancionesQueSabe unCantante = (>=2)(length (repertorio unCantante))

duracionTotalDeCanciones :: Cantante->Int->Bool
duracionTotalDeCanciones unCantante duracionMinima = (<=duracionMinima)(sum(map duraciones (repertorio unCantante)))

duraciones::Cancion->Int
duraciones unaCancion = (duracion unaCancion)

esAcelerado::Cantante->Bool
esAcelerado unCantante = not(any (duraMenosDeCuatro) (repertorio unCantante))

duraMenosDeCuatro::Cancion->Bool
duraMenosDeCuatro unaCancion = (>4)(duraciones unaCancion)

conciertoGigante:: Cantante->Int->Int->Bool
conciertoGigante unCantante cantidadMinimaDeCanciones duracionMinimaCanciones= 
    ((>=cantidadMinimaDeCanciones)(length (repertorio unCantante)) && not(duracionTotalDeCanciones unCantante duracionMinimaCanciones)) 

conciertoMediano::Cantante->Int->Bool
conciertoMediano unCantante duracionMaximaCanciones =
    (duracionTotalDeCanciones unCantante duracionMaximaCanciones) 

conciertoPequeño:: Cantante->Int->Bool
conciertoPequeño unCantante duracionMinimaCancion=
    (any(>duracionMinimaCancion).map (duraciones) $ (repertorio unCantante)) 

mikuExpo :: Cantante->Cantante
mikuExpo unCantante   
    |conciertoGigante unCantante 2 6 = unCantante{fama=(fama unCantante)+2000}
    |otherwise=unCantante


vocalektVisions::Cantante->Cantante
vocalektVisions unCantante 
    |conciertoMediano unCantante 9 = unCantante{fama=(fama unCantante)+1000}
    |otherwise= unCantante


magicalMirai :: Cantante->Cantante
magicalMirai unCantante
    |conciertoGigante unCantante 3 10 = unCantante{fama=(fama unCantante)+3000}
    |otherwise = unCantante


mikuFest::Cantante->Cantante
mikuFest unCantante
    |conciertoPequeño unCantante 4 = unCantante{fama=(fama unCantante)+100}
    |otherwise= unCantante

puedeParticiparDelConcierto:: (Cantante->Cantante)->Cantante->Bool
puedeParticiparDelConcierto concierto unCantante = unCantante /= (concierto unCantante) || unCantante == hatsuneMiku

vocaloidMasFamoso::[Cantante]->[(Cantante->Cantante)]->Cantante
vocaloidMasFamoso listaDeCantantes listaDeConciertos=
     foldl1 (mayorFama).map (multiplicarFamaPorRepertorio).foldl (irAConcierto) listaDeCantantes $ listaDeConciertos 

irAConcierto::[Cantante]->(Cantante->Cantante)->[Cantante]
irAConcierto listaDeCantantes unConcierto =
    map (aplicarFamaDeConcierto unConcierto) listaDeCantantes
    
aplicarFamaDeConcierto:: (Cantante->Cantante) ->Cantante ->Cantante
aplicarFamaDeConcierto unConcierto unCantante 
    |puedeParticiparDelConcierto unConcierto unCantante = unConcierto unCantante
    |otherwise = unCantante


multiplicarFamaPorRepertorio:: Cantante->Cantante
multiplicarFamaPorRepertorio unCantante = 
    unCantante{fama=(fama unCantante)*(length(repertorio unCantante))}

mayorFama::Cantante->Cantante->Cantante
mayorFama unCantante otroCantante
    |fama unCantante > fama otroCantante = unCantante
    |otherwise = otroCantante


unicoQueParticipaEnUnConcierto::(Cantante->Cantante)->Cantante->[Cantante]->Bool
unicoQueParticipaEnUnConcierto unConcierto unCantante listaDeCantantes = 
    (puedeParticiparDelConcierto unConcierto unCantante) && not(algunoParticipa unConcierto listaDeCantantes)

algunoParticipa:: (Cantante->Cantante)->[Cantante]->Bool
algunoParticipa unConcierto listaDeCantantes =
    any(puedeParticiparDelConcierto unConcierto) listaDeCantantes