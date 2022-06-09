
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

-- B) Modelar los hechizos, sabiendo que deberían poderse agregar más sin modificar el código existente. Por ahora existen los siguientes:
type Hechizo = Postre -> Postre

modificarTemp :: (Number -> Number) -> Postre -> Postre
modificarTemp modificar postre = postre{temperatura = modificar (temperatura postre)}

modificarpeso :: (Number -> Number) -> Postre -> Postre
modificarpeso modificar postre = postre{peso = modificar (peso postre)}

agregarSabor :: String -> Postre -> Postre
agregarSabor sabor postre = postre{sabores = sabor:sabores postre}

-- Incendio: calienta el postre 1 grado y lo hace perder 5% de su peso.
incendio :: Hechizo
incendio = modificarpeso (*0.95) . modificarTemp (+1)

--Immobulus: congela el postre, llevando su temperatura a 0.
inmobulus :: Hechizo
inmobulus = modificarTemp (const 0)

-- Wingardium Leviosa: levanta el postre en el aire y lo deja caer, lo que agrega a sus sabores el sabor “concentrado”. Además, pierde 10% de su peso.
wingardiumLeviosa :: Hechizo
wingardiumLeviosa = agregarSabor "concentrado" . modificarpeso (*0.9)

-- Diffindo: Corta el postre, disminuyendo su peso en el porcentaje indicado.
-- Lo tome como que el numero que le pasa es el porcentaje que quiere bajar
diffindo :: Number -> Hechizo
diffindo num = modificarpeso (*(1 - (num/100)))

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
pesoPromedio :: [Postre] -> Number
pesoPromedio postres = ((sum. map peso . postresListos) postres) / ((length . postresListos) postres)

-- Ejercicio 2) Magos 
-- De un mago se conocen sus hechizos aprendidos y la cantidad de horrorcruxes que tiene.

{-
    A) Hacer que un mago asista a la clase de defensa contra las cocinas oscuras y practique con un hechizo sobre un
    postre (se espera obtener el mago). 
    Cuando un mago practica con un hechizo, lo agrega a sus hechizos aprendidos. 
    Además si el resultado de usar el hechizo en el postre es el mismo que aplicarle “avada kedavra” al postre, entonces suma un horrorcrux.
-}

data Mago = UnMago{
    hechizos :: [Hechizo],
    cantHorrorCruxes :: Number
}deriving (Show,Eq)

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago{hechizos = hechizo:hechizos mago}

sumarHorrorCrux :: Mago -> Mago
sumarHorrorCrux mago = mago{cantHorrorCruxes = cantHorrorCruxes mago + 1}

practicarHechizo :: Postre -> Hechizo -> Mago -> Mago
practicarHechizo postre hechizo = agregarHechizo hechizo . sumarHorrorCruxSiCumple postre hechizo

sumarHorrorCruxSiCumple :: Postre -> Hechizo -> Mago -> Mago
sumarHorrorCruxSiCumple postre hechizo mago
    | hechizo postre == avadaKedavra postre = sumarHorrorCrux mago
    | otherwise = mago

-- B) Dado un postre y un mago obtener su mejor hechizo, que es aquel de sus hechizos que deja al postre con más cantidad de sabores luego de usarlo.
mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = masCantidaDeSabores postre (hechizos mago)


masCantidaDeSabores :: Postre -> [Hechizo] -> Hechizo
masCantidaDeSabores _ [a] = a
masCantidaDeSabores postre (h1:h2:hs)
    | (cantSabores.h1) postre > (cantSabores.h2) postre = masCantidaDeSabores postre (h1:hs)
    | otherwise = masCantidaDeSabores postre (h2:hs)

-- 3) Infinita Magia
-- A) Construir una lista infinita de postres, y construir un mago con infinitos hechizos.
infinitosPostres :: [Postre]
infinitosPostres = tarta:infinitosPostres

infinitosHechizos :: [Hechizo]
infinitosHechizos = incendio:infinitosHechizos

merlin = UnMago infinitosHechizos 