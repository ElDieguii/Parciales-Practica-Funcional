--Series = [("los soprano", 6, 1999, "HBO"),("lost", 6, 2004, "ABC"),("4400", 4, 2004, "CBS"),("United States of Tara", 3, 2009, "Dreamworks"),("V", 3, 2009, "Warner Bross"),("dr house", 7, 2004, "Universal")] 
--Actores = [("Ken Leung", ["lost", "los soprano"]),("Joel Gretsch", ["4400", "V", "United States of Tara"]),("James Gandolfini", ["los soprano"]),("Elizabeth Mitchell", ["dr house", "V", "lost"])]  
type Series = [(String,Int,Int,String)]
type UnaSerie = (String,Int,Int,String)
type Actores = [(String,[String])]
type UnActor = (String,[String])
serie (s,_,_,_)=s
temporadas (_,t,_,_)=t
anioComienzo (_, _, a, _) = a 
cadenaTV (_, _, _, c) = c

nombreActor = fst 
seriesDeActor = snd 
find criterio = head . filter criterio

datosDe :: String->Series->UnaSerie
datosDe serieBuscada listaDeSeries=
    head.filter (obtenerDatos serieBuscada) $listaDeSeries

obtenerDatos :: String->UnaSerie->Bool
obtenerDatos serieBuscada unaSerie=
    (serieBuscada==(serie unaSerie))

listaDeActoresDe::String->Actores->[String]
listaDeActoresDe serie listaDeActores=
    nombresDeLosActores.filter (actoresQueActuaronEn serie) $listaDeActores

actoresQueActuaronEn:: String->UnActor->Bool
actoresQueActuaronEn serie unActor =
    any (==serie) (snd unActor)

nombresDeLosActores :: Actores->[String]
nombresDeLosActores listaDeActores = 
    map (nombreActor) listaDeActores

quienesActuaronEn :: String->String->Actores->[String]
quienesActuaronEn unaSerie otraSerie listaDeActores =
    filter (actuaEnAmbas.listaDeActoresDe unaSerie $listaDeActores).listaDeActoresDe otraSerie $listaDeActores

actuaEnAmbas:: [String]->String->Bool
actuaEnAmbas listaDeActores unActor =
    elem unActor listaDeActores

anioDeComienzoDe::Series->String->Int
anioDeComienzoDe series unaSerie =
    anioComienzo.datosDe unaSerie $series

seriesOrdenadas:: [String]->Series->Bool
seriesOrdenadas [] _ =True
seriesOrdenadas listaSeries series
    |all (mayorRespectoAlAnio (head listaSeries) series).tail $listaSeries  = seriesOrdenadas (tail listaSeries) series
    |otherwise = False
    
mayorRespectoAlAnio :: String->Series->String->Bool
mayorRespectoAlAnio unaSerie series otraSerie  =
    anioDeComienzoDe series unaSerie  <= anioDeComienzoDe series otraSerie 

queSeriesCumplen :: (UnaSerie->Bool)->Series->[String]
queSeriesCumplen unCriterio = map serie.filter unCriterio

temporadasMasDe :: Int-> UnaSerie -> Bool
temporadasMasDe numero unaSerie =
    (numero<).temporadas $unaSerie

actoresMasDe :: Int-> Actores->UnaSerie -> Bool
actoresMasDe numero actores unaSerie =
    (numero<).length.listaDeActoresDe (serie unaSerie) $actores

tituloMenorA:: Int->UnaSerie->Bool
tituloMenorA numero unaSerie = 
    (numero>).length.serie $unaSerie 

promedioGeneral:: (UnaSerie->Int)->Series->Int
promedioGeneral criterio listaDeSeries =
    ((sum.map criterio $listaDeSeries )`div`).length $listaDeSeries


funcionHeavy:: Ord d=>(a->b)->(a->b)->[a]->d -> d->[b]    
funcionHeavy a b c d e
    |d>e = map a c
    |otherwise = map b c

