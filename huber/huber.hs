data Chofer = UnChofer{
    nombreChofer::String,
    kilometraje::Float,
    viajesRealizados::Int
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
    direccion :: String
}

