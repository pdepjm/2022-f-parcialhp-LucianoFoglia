
-- Nombre: Foglia, Luciano
-- Legajo: 203624-1

-- Parcial Jueves 09 de Junio de 2022
-- Paradigmas de Programación UTN.BA Jueves Mañana

module Library where
import PdePreludat

parcial = "acá"

-- Ejercicio 1)

{-
    A) Modelar los postres. Un mismo postre puede tener muchos sabores, tiene un peso y se sirve a cierta temperatura.
    Por ejemplo, un bizcocho borracho de fruta y crema de 100 gramos servido a 25°C.
-}

data Postre = UnPostre{
    sabores :: [String],
    peso :: Number,
    temperatura :: Number
}deriving (Show,Eq)

prueba = UnPostre ["fruta","menta"] 100 25
prueba2 = UnPostre ["fruta","menta"] 100 25
otro = UnPostre ["chocolate"] 150 0
tarta = UnPostre ["manzana"] 250 15
bizcocho = UnPostre ["alcohol","fruta","crema"] 100 25

-- B) Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código existente. Por ahora existen los siguientes:
type Hechizo = Postre -> Postre

modificarTemp :: (Number -> Number) -> Postre -> Postre
modificarTemp modificador postre = postre{temperatura = modificador (temperatura postre)}

modificarPeso :: (Number -> Number) -> Postre -> Postre
modificarPeso modificador postre = postre{peso = modificador (peso postre)}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre{sabores = sabor:sabores postre}

-- Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.
incendio :: Hechizo
incendio = modificarPeso (*0.95) . modificarTemp (+1)

--Immobulus: congela el postre, llevando su temperatura a 0.
inmobulus :: Hechizo
inmobulus = modificarTemp (const 0)

-- Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus sabores el sabor “concentrado”. Además, pierde 10% de su peso.
wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado" . modificarPeso (*0.9)

-- Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado.
-- Lo tome como que el numero que le pasa es el porcentaje que quiere bajar
diffindo :: Number -> Hechizo
diffindo num = modificarPeso (*(1 - (num/100)))

-- Riddikulus: Requiere como información adicional un sabor y lo agrega a los sabores que tiene un postre, pero invertido.
riddikulus :: String -> Hechizo
riddikulus sabor = (agregarSabor . reverse) sabor

-- Avada kedavra: Hace lo mismo que el immobulus pero además hace que el postre pierda todos sus sabores.
avadaKedavra :: Hechizo
avadaKedavra postre = inmobulus postre{sabores = []}

{-
    C) Dado un conjunto de postres en la mesa, saber si hacerles un determinado hechizo los dejará listos 
    (un postre está listo cuando pesa algo más que cero, tiene algún sabor y además no está congelado).
    Por ejemplo, si en la mesa está el bizcocho mencionado anteriormente y una tarta de melaza de 0 grados 
    y 50 gramos, y les hago el hechizo incendio, quedan listos, pero si les hago el hechizo riddikulus con el sabor
    “nomil” no, porque la tarta sigue congelada.  
-}
postresListos :: [Postre] -> [Postre]
postresListos = filter estaListo

postresListosPostHechizo :: Hechizo -> [Postre] -> [Postre]
postresListosPostHechizo hechizo = postresListos . map hechizo 

estaListo :: Postre -> Bool
estaListo postre = cantSabores postre > 0 && peso postre > 0 && temperatura postre > 0

cantSabores :: Postre -> Number
cantSabores = length . sabores

-- D) Dado un conjunto de postres en la mesa, conocer el peso promedio de los postres listos. 
pesoPromedioPostresListos :: [Postre] -> Number
pesoPromedioPostresListos = (promedio . map peso . postresListos)

promedio :: [Number] -> Number
promedio lista = sum lista / length lista

-- Ejercicio 2) Magos 
-- De un mago se conocen sus hechizos aprendidos y la cantidad de horrorcruxes que tiene.

data Mago = UnMago{
    hechizos :: [Hechizo],
    cantHorrorCruxes :: Number
}deriving (Show,Eq)

messi = UnMago [incendio,wingardiumLeviosa] 7
cRonaldo = UnMago [diffindo 15,riddikulus "limon",inmobulus] 5

{-
    A) Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un hechizo sobre un
    postre (se espera obtener el mago). 
    Cuando un mago practica con un hechizo, lo agrega a sus hechizos aprendidos. 
    Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada kedavra” al postre, entonces suma un horrorcrux.
-}
practicarHechizo :: Hechizo -> Postre -> Mago -> Mago
practicarHechizo hechizo postre = agregarHechizo hechizo . sumarHorrorCruxSiCumple hechizo postre

sumarHorrorCruxSiCumple :: Hechizo -> Postre -> Mago -> Mago
sumarHorrorCruxSiCumple hechizo postre mago
    | hechizo postre == avadaKedavra postre = sumarHorrorCrux mago
    | otherwise = mago

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago{hechizos = hechizo:hechizos mago}

sumarHorrorCrux :: Mago -> Mago
sumarHorrorCrux mago = mago{cantHorrorCruxes = cantHorrorCruxes mago + 1}

-- B) Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al postre con más cantidad de sabores luego de usarlo.
mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre = masCantidaDeSabores postre . hechizos

masCantidaDeSabores :: Postre -> [Hechizo] -> Hechizo
-- Supongo que no existen los magos que no sepan hechizos
masCantidaDeSabores _ [a] = a
masCantidaDeSabores postre (h1:h2:hs)
    | (cantSabores.h1) postre > (cantSabores.h2) postre = masCantidaDeSabores postre (h1:hs)
    | otherwise = masCantidaDeSabores postre (h2:hs)

-- 3) Infinita Magia
-- A) Construir una lista infinita de postres, y construir un mago con infinitos hechizos.
infinitosPostres :: [Postre]
infinitosPostres = tarta:infinitosPostres

merlin = UnMago infinitosHechizos 5

infinitosHechizos :: [Hechizo]
infinitosHechizos = incendio:inmobulus:infinitosHechizos

{-
    B) Suponiendo que hay una mesa con infinitos postres, y pregunto si algún hechizo los deja listos ¿Existe alguna consulta que pueda
    hacer para que me sepa dar una respuesta? Justificar conceptualmente.

    RTA: Tomo como que la funcion recibiria una lista de hechizos y una lista de postres. En este caso, lazy evaluation nos ayudaria solo
    si es falso ya que para saber si todos quedan listos, deberia utilizar un any para la lista de hechizos y un all para la lista de postres
    entonces tendria que leer la lista entera de postres, si es verdadero que ese hechizo los deja listos (algo imposible si es infinita). 
    Por otro lado, si todos los hechizos devuelven por lo menos un postre que no este listo,  entonces devolveria False. Ya que todos los 
    all devolverian False. Y el any tambien daria False
-}

{-
    C) Suponiendo que un mago tiene infinitos hechizos ¿Existe algún caso en el que se puede encontrar al mejor hechizo? Justificar conceptualmente.

    RTA: En este caso, lazy evaluation no nos ayudaria ya que para saber el mejor hechizo debe leer la lista entera hasta que quede uno solo. Por lo que
    si es infinita, por mas que vayas sacando hechizos, nunca va a quedar uno solo. No hay ningun caso en el que se pueda encontrar el mejor hechizo.
-}