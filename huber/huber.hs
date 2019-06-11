import Text.Show.Functions
data Chofer = UnChofer{
    nombreChofer::String,
    kilometraje::Float,
    viajesRealizados::[Viaje],
    condicionParaTomarViaje::(Viaje->Bool)
}deriving (Show)

type Fecha = Int

data Viaje = UnViaje{
    fecha:: Fecha,
    cliente:: Cliente,
    costo::Float
}deriving (Show)

data Cliente = UnCliente{
    nombreCliente::String,
    barrio :: String
}deriving (Show)

viajeSaleMasDe::Float->Viaje->Bool
viajeSaleMasDe precio unViaje =
    costo unViaje >= precio

cantidadDeLetrasDelNombreDelClienteMayorA:: Int->Viaje->Bool
cantidadDeLetrasDelNombreDelClienteMayorA letras unViaje=
    (>letras).length.nombreCliente.cliente $unViaje

clienteViveEn::String->Viaje->Bool
clienteViveEn unBarrio unViaje =
    (== unBarrio).barrio.cliente $unViaje

lucas =UnCliente{
    nombreCliente = "Lucas",
    barrio ="Victoria"
}
daniel = UnChofer{
    nombreChofer = "Daniel",
    kilometraje = 23500,
    viajesRealizados = [(UnViaje{fecha = 20042017, cliente = lucas, costo = 150})],
    condicionParaTomarViaje = (not.clienteViveEn "Olivos")
}
alejandra = UnChofer{
    nombreChofer = "Alejandra",
    kilometraje = 180000,
    viajesRealizados = [],
    condicionParaTomarViaje = (\_->True)
}

choferPuedeTomarElViaje:: Viaje->Chofer->Bool
choferPuedeTomarElViaje unViaje unChofer =
    condicionParaTomarViaje unChofer unViaje

liquidacionDeViajes::Chofer->Float
liquidacionDeViajes unChofer = 
    sum.costosDeViajes.viajesRealizados $unChofer

costosDeViajes:: [Viaje]->[Float]
costosDeViajes listaDeViajes =
    map (costos) listaDeViajes

costos::Viaje->Float
costos unViaje =
    costo unViaje

realizarUnViaje :: Viaje->[Chofer]->Chofer
realizarUnViaje unViaje listaDeChoferes =
    efectuarViaje unViaje.choferConMenosViajes.filter (choferPuedeTomarElViaje unViaje) $listaDeChoferes

choferConMenosViajes :: [Chofer]->Chofer
choferConMenosViajes listaDeChoferes = 
    foldl1 (tieneMenosViajesRealizados) $listaDeChoferes

tieneMenosViajesRealizados :: Chofer->Chofer->Chofer
tieneMenosViajesRealizados unChofer otroChofer 
    |tieneMenosViajes unChofer otroChofer = unChofer
    |otherwise = otroChofer

tieneMenosViajes :: Chofer->Chofer->Bool
tieneMenosViajes unChofer otroChofer =
    (<cantidadDeViajes otroChofer).cantidadDeViajes $unChofer

cantidadDeViajes :: Chofer->Int
cantidadDeViajes unChofer =
    length.viajesRealizados $unChofer

efectuarViaje :: Viaje->Chofer->Chofer
efectuarViaje unViaje unChofer =
    unChofer{viajesRealizados = [unViaje] ++ viajesRealizados unChofer}


nitoInfy = UnChofer{
    nombreChofer = "Nito Infy",
    kilometraje = 70000,
    viajesRealizados = repetirViaje (UnViaje{fecha = 11032017, cliente = lucas, costo = 50}),
    condicionParaTomarViaje = (cantidadDeLetrasDelNombreDelClienteMayorA 3)
}

repetirViaje :: Viaje -> [Viaje]
repetirViaje unViaje = 
    unViaje : repetirViaje unViaje