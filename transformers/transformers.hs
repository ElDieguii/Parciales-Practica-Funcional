import Text.Show.Functions
data Autobot = Robot String (Int,Int,Int) ((Int,Int,Int)->(Int,Int)) | Vehiculo String (Int,Int) deriving(Show)

optimus = Robot "Optimus Prime" (20,20,10) optimusTransformacion
optimusTransformacion (_,v,r) = (v * 5, r * 2)
    
jazz = Robot "Jazz" (8,35,3) jazzTransformacion
jazzTransformacion (_,v,r) = (v * 6, r * 3)
    
wheeljack = Robot "Wheeljack" (11,30,4) wheeljackTransformacion
wheeljackTransformacion (_,v,r) = (v * 4, r * 3)
    
bumblebee = Robot "Bumblebee" (10,33,5) bumblebeeTransformacion
bumblebeeTransformacion (_,v,r) = (v * 4, r * 2)
    
autobots = [ optimus, jazz, wheeljack, bumblebee ]

mayorSegun::(Num a, Ord a)=>(a->a->a)->a->a->a
mayorSegun funcion unValor otroValor
    |(funcion otroValor unValor  >=).funcion unValor $otroValor = otroValor
    |otherwise = unValor
    
atributoNombre::Autobot->String
atributoNombre (Vehiculo nombre _) = nombre
atributoNombre (Robot nombre _ _)= nombre

atributoCapacidades::Autobot->(Int,Int,Int)
atributoCapacidades (Vehiculo _ (v,r)) = (0,v,r)
atributoCapacidades (Robot _ (f,v,r) _)= (f,v,r)

atributoFuerza::(Int,Int,Int)->Int
atributoFuerza (f,v,r)=f

atributoVelocidad::(Int,Int,Int)->Int
atributoVelocidad (f,v,r)=v

atributoResistencia::(Int,Int,Int)->Int
atributoResistencia (f,v,r)=r

transformar::Autobot->Autobot
transformar (Robot nombre (f,v,r) funcion) = (Vehiculo nombre (funcion (f,v,r)))

velocidadContra:: Autobot->Autobot->Int
velocidadContra unAutobot otroAutobot =
    velocidadAlterada unAutobot.max 0.restaDeAtributos unAutobot $otroAutobot

restaDeAtributos::Autobot->Autobot->Int
restaDeAtributos unAutobot otroAutobot=
    (atributoFuerza.atributoCapacidades $otroAutobot) -(atributoResistencia.atributoCapacidades $unAutobot)

velocidadAlterada::Autobot->Int->Int
velocidadAlterada unAutobot cambioDeVelocidad =
    (atributoVelocidad.atributoCapacidades $unAutobot) -(cambioDeVelocidad)

elMasRapido::Autobot->Autobot->Autobot
elMasRapido unAutobot otroAutobot
    |elPrimeroEsMasRapido unAutobot otroAutobot= unAutobot
    |otherwise = otroAutobot

elPrimeroEsMasRapido::Autobot->Autobot->Bool
elPrimeroEsMasRapido unAutobot otroAutobot =
    (<velocidadContra unAutobot otroAutobot).velocidadContra otroAutobot $unAutobot

domina:: Autobot->Autobot->Bool
domina unAutobot otroAutobot =
    all (venceEnVelocidad unAutobot.transformar $unAutobot) [otroAutobot, (transformar otroAutobot)]


venceEnVelocidad:: Autobot->Autobot->Autobot->Bool
venceEnVelocidad unRobot unVehiculo otroAutobot = 
    --(elPrimeroEsMasRapido unRobot otroAutobot) && (elPrimeroEsMasRapido unVehiculo otroAutobot) -- OPCION 1
    all(flip elPrimeroEsMasRapido otroAutobot) [unRobot, unVehiculo]  --OPCION 2