data Chofer = UnChofer{
    nombreChofer::String,
    kilometraje::Float,
    viajesRealizados::[Viaje],
    condicionParaTomarViaje::(Viaje->Bool)
}

type Fecha = Int

data Viaje = UnViaje{
    fecha:: Fecha,
    cliente:: Cliente,
    costo::Float
}

data Cliente = UnCliente{
    nombreCliente::String,
    barrio :: String
}

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
    viajesRealizados = [UnViaje{fecha = 20042017, cliente = lucas, costo = 150}],
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

liquidacionDeViajes::Chofer->Int
liquidacionDeViajes unChofer = 
    sum.viajesRealizados $unChofer