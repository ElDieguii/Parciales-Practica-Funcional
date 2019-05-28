type Requisito = Propiedad->Bool
type Busqueda = [Requisito]

data Propiedad = Depto{
    ambientes::Int,
    superficie::Int,
    precio:: Float,
    barrio::String
}deriving(Show,Eq)

data Usuario = Persona{
    mail::String,
    busqueda::Busqueda
}

ordenarSegun:: (a->a->Bool)->[a]->[a]
ordenarSegun _ []=[]
ordenarSegun criterio (x:xs)=
    (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs

between x y z = x<=z && y>=z

propiedadesDeEjemplo = 
    [Depto{ambientes=3,superficie=80, precio=12500, barrio="Palermo"},Depto{ambientes=1,superficie=45, precio=10000, barrio="Villa Urquiza"},
    Depto{ambientes=2,superficie=50, precio=16000, barrio="Palermo"},Depto{ambientes=1,superficie=45, precio=10500, barrio="Recoleta"}]

mayorSegun::Ord b =>(a->b)->a->a->Bool
mayorSegun funcion valorUno valorDos= 
    (>funcion valorDos).funcion $valorUno 

--1.b)
--ordenarSegun ( mayorSegun (length)) ["hola", "holaa", "holaaa"] => ["holaaa", "holaa", "hola"]

--2.a)
departamento1 = Depto{
    ambientes = 1,
    superficie = 7899,
    precio = 9500,
    barrio = "Recoleta"
}
departamento2 = Depto{
    ambientes = 3,
    superficie = 4567,
    precio = 8001,
    barrio = "Palermo"
}
departamento3 = Depto{
    ambientes = 1,
    superficie = 2345,
    precio = 10000,
    barrio = "Recoleta"
}
departamento4 = Depto{
    ambientes = 2,
    superficie = 1234,
    precio = 9700,
    barrio = "Palermo"
}

ubicadoEn:: [String]->Propiedad->Bool
ubicadoEn listaDeBarrios unDepartamento =
    elem (barrio unDepartamento) listaDeBarrios 


cumpleRango:: (Num a, Ord a)=>(Propiedad->a)->a->a->Propiedad->Bool
cumpleRango funcion unNumero otroNumero =
    between unNumero otroNumero.funcion

atributoPrecio :: Propiedad->Float
atributoPrecio unaPropiedad = precio unaPropiedad

atributoAmbiente :: Propiedad->Int
atributoAmbiente unaPropiedad = ambientes unaPropiedad

atributoSuperficie :: Propiedad ->Int
atributoSuperficie unaPropiedad = superficie unaPropiedad

--3.a)
cumpleBusqueda:: Busqueda->Propiedad->Bool
cumpleBusqueda requisitosDeBusqueda unaPropiedad  =
    all(cumpleRequisito unaPropiedad) requisitosDeBusqueda

cumpleRequisito:: Propiedad->(Propiedad->Bool)->Bool
cumpleRequisito unaPropiedad unRequisito = unRequisito unaPropiedad
    
--cumpleBusqueda departamento [(cumplaRango atributoPrecio 10 20),(ubicadoEn ["Villa Del Parque", "Almagro", "Caballito"])] ==> FALSE

--3.b)
buscar:: Busqueda->(Propiedad->Propiedad->Bool)->[Propiedad]->[Propiedad]
buscar listaDeRequisitos unCriterio listaDePropiedades=
    ordenarSegun unCriterio (propiedadesQueCumplenRequisitos listaDePropiedades listaDeRequisitos)

propiedadesQueCumplenRequisitos:: [Propiedad]->Busqueda->[Propiedad]
propiedadesQueCumplenRequisitos listaDePropiedades listaDeRequisitos =
    filter(cumpleBusqueda listaDeRequisitos) listaDePropiedades


--3.c) 
--buscar ([(ubicadoEn ["Palermo","Recoleta"]), (cumpleRango atributoAmbiente 1 2), (cumpleRango atributoPrecio 8000 15000)]) (mayorSegun (atributoSuperficie)) [departamento1,departamento2, departamento3,departamento4]

--4)

diegui = Persona{
    mail = "diegovivona@hotmail.com",
    busqueda = [(ubicadoEn ["Recoleta"]), (cumpleRango atributoAmbiente 1 2), (cumpleRango atributoPrecio 8000 15000)]
}

pipo = Persona{
    mail = "pipo@hotmail.com",
    busqueda = [(ubicadoEn ["Villa Urquiza"]), (cumpleRango atributoAmbiente 2 3), (cumpleRango atributoPrecio 10000 11000)]
}

mailsDePersonasInteresadas:: Propiedad->[Usuario]->[String]
mailsDePersonasInteresadas unaPropiedad =
    map (mostrarMail).filter (aux unaPropiedad) 

aux::Propiedad->Usuario->Bool
aux unaPropiedad unaPersona =
    cumpleBusqueda (busqueda unaPersona) unaPropiedad

mostrarMail :: Usuario->String
mostrarMail unaPersona = mail unaPersona

