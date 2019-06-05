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

transformar::Autobot->Autobot
transformar (Robot nombre (f,v,r) funcion) = (Vehiculo nombre (funcion (f,v,r)))
