	{--------
	Ejercicio 3
------}

--Elevar num a su exponente
--elevarArreglo :: [a]
elevarPotencia value potencia = elevatePow value potencia
	where
		elevatePow value potencia 
			| potencia == 0 = 1
			| otherwise = value * (elevatePow value (potencia - 1) )  

--Obtener longitud de arreglo
getLongitudArr arr = longitudArr arr
	where
		longitudArr [] = 0
		longitudArr [x] = 1
		longitudArr (x:arr) = longitudArr (arr) + 1 

--tomar esa cantidad de elementos del arreglo
takear cant arr = toma cant arr
	where
		toma _ [] = []
		toma cant (y:arr) 
			| cant == 0 = []
			|	otherwise = y:(toma (cant - 1) arr)

--Obtener el índice, y por ende, si existe
indexOf arr encontrar = index arr encontrar 0
	where 
		index [] _ x = (-1)
		index (x:arr) encontrar n
			| (takear (getLongitudArr encontrar) (x:arr)) == encontrar = n
			| otherwise = (index arr encontrar (n + 1) )

--get último elemento de arreglo
myLast (x:arr) = miUltimoElemento x arr
	where
		miUltimoElemento x [] = x
		miUltimoElemento _ (y:arr) = miUltimoElemento y arr

--invertir el orden del arreglo
reversar arr = invertir arr
	where
		invertir [] = []
		invertir [x] = [x]
		invertir (x:arr) = 
			myLast(x:arr):invertir( takear ((getLongitudArr (x:arr)) - 1) (x:arr))

--Sumar todos los elems de un array
suma [] = 0
suma (x:arr) = x + (suma arr)

--Map function, adaptada
mapiar _ [] = []
mapiar fn (x:arr) = (fn x):(mapiar fn arr) 

elevarArreglo [] = []	
elevarArreglo (x:arr) = ((elevarPotencia 10 (getLongitudArr (x:arr) - 1) ) * x):(elevarArreglo arr) 

-----FINAL EJERCICIO 3
unirElems [] = 0
unirElems arr = suma (elevarArreglo arr)

{--------
	Ejercicio 4
------}

--eliminar cantidad de elementos en arr
eliminar arr value = elim arr value
	where
		elim [] _ = []
		elim (y:arr) value 
			| y == value = arr
			| otherwise = y:(elim arr value)		

getMenorElemento (x:arr) = menorElemento x arr
	where 
	menorElemento x [] = x
	menorElemento x (y:arr)
		| x < y = menorElemento x arr
		| otherwise = menorElemento y arr

--ordenar arreglo
ordenar arr = sorting arr
	where
		sorting [] = []
		sorting arr = 
			(getMenorElemento arr):(sorting 
			(eliminar arr (getMenorElemento arr) ) )

--get duplicados en arreglo
getDups arr = getDuplicados arr
  where
    getDuplicados []  = []
    getDuplicados [x] = []
    getDuplicados (x:y:arr)
      | x == y = x :(getDuplicados (arr))
      | otherwise = (getDuplicados (y:arr))
 
concatenarArrs arr1 arr2 = arr1 ++ arr2

ordenarArrs arr1 arr2 = ordenar (concatenarArrs arr1 arr2)

interseccion arr1 arr2 = (getDups (ordenarArrs arr1 arr2) )	

existsInside arr1 arr2 = exisIn arr1 arr2
	where
		exisIn arr1 arr2
			| (ordenar arr1) == (interseccion arr1 arr2) = True
			| otherwise = False