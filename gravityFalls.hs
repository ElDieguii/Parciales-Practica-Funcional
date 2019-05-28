import Text.Show.Functions
data Criatura = UnaCriatura{
    peligrosidad :: Int,
    puedeDeshacerseDeElla :: (Persona->Bool)
}deriving(Show)

data Persona = UnaPersona{
    edad::Int,
    items::[String],
    experiencia::Int
}deriving(Show,Eq)

diegui = UnaPersona{
    edad=20,
    items=["soplador de hojas"],
    experiencia=0
}

elSiempreDetras ::Criatura
elSiempreDetras = UnaCriatura{
    peligrosidad = 0,
    puedeDeshacerseDeElla = (\_->False)
}

gnomos::Int->Criatura
gnomos cantidad = UnaCriatura{
    peligrosidad= 2^cantidad,
    puedeDeshacerseDeElla = tieneItem "soplador de hojas"
}

fantasma:: Int->(Persona->Bool)->Criatura
fantasma nivelDePoder asuntoPendiente = UnaCriatura{
    peligrosidad = nivelDePoder * 20,
    puedeDeshacerseDeElla = asuntoPendiente
}

enfrentarConCriatura:: Persona->Criatura->Persona
enfrentarConCriatura unaPersona unaCriatura 
    |gana unaPersona unaCriatura = aumentarExperiencia (peligrosidad unaCriatura) unaPersona
    |otherwise = aumentarExperiencia 1 unaPersona

tieneItem :: String->Persona->Bool
tieneItem item unaPersona =
    elem item (items unaPersona)

aumentarExperiencia:: Int->Persona->Persona
aumentarExperiencia aumento unaPersona =
    unaPersona{experiencia = (experiencia unaPersona)+ aumento}

gana::Persona->Criatura->Bool
gana unaPersona unaCriatura = 
    puedeDeshacerseDeElla unaCriatura unaPersona

enfrentamientosSucesivos::Persona->[Criatura]->Persona
enfrentamientosSucesivos unaPersona criaturas =
    foldl enfrentarConCriatura unaPersona criaturas

--enfrentamientosSucesivos diegui [elSiempreDetras, gnomos 10, fantasma 3 (\diegui-> edad diegui >13 && tieneItem "disfraz de oveja" diegui), fantasma 1 (\diegui-> experiencia diegui > 10)]
--------------
--UnaPersona {edad = 20, items = ["soplador de hojas"], experiencia = 1046}

zipWithIf::(a->b->b)->(b->Bool)->[a]->[b]->[b]
zipWithIf _ _ [] _ = []
zipWithIf _ _ _ [] = []

zipWithIf operacion condicion (x:xs) (y:ys)
    |condicion y = operacion x y : zipWithIf operacion condicion (xs) (ys)
    |otherwise = y: zipWithIf operacion condicion (x:xs) (ys)


abecedarioDesde::Char->[Char]
abecedarioDesde letra =
    [letra] ++ (filter(>letra) ['a'..'z']) ++ (filter(<letra) ['a'..'z']) ++ []

desencriptarLetra :: Char->Char->Char
desencriptarLetra letraClave letraADesencriptar =
    ['a'..'z']!!(posicionEnAbecedario letraADesencriptar (abecedarioDesde letraClave))

posicionEnAbecedario::Char->[Char]->Int
posicionEnAbecedario letraADesencriptar abecedarioDesdeClave =
    length.takeWhile (/=letraADesencriptar) $ abecedarioDesdeClave


cesar::Char->String->String
cesar letraClave palabraEncriptada=
    zipWithIf (desencriptarLetra) (perteneceAlAbc) (emparejarCantidadDeLetras [letraClave] palabraEncriptada) palabraEncriptada


perteneceAlAbc::Char->Bool
perteneceAlAbc letraADesencriptar = elem letraADesencriptar ['a'..'z']

--consultaMultiple:: String->[Char]->String
consultaMultiple palabraEncriptada []= ""
consultaMultiple palabraEncriptada abecedario=
    cesar (head abecedario) palabraEncriptada ++ ", " ++ consultaMultiple palabraEncriptada (tail abecedario)

vigenere::[Char]->String->String
vigenere palabraClave palabraEncriptada =
    zipWithIf (desencriptarLetra) (perteneceAlAbc) (emparejarCantidadDeLetras palabraClave palabraEncriptada ) palabraEncriptada

repiteInfinitamente :: String->String
repiteInfinitamente clave = clave ++ repiteInfinitamente clave

palabraALetra :: [Char]->[Char]
palabraALetra []= []
palabraALetra palabra = (head palabra):palabraALetra (tail palabra)

emparejarCantidadDeLetras :: [Char]->[Char]->[Char]
emparejarCantidadDeLetras clave encriptado =
    palabraALetra.take (length encriptado) $(repiteInfinitamente clave)