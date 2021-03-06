import Debug.Trace

square x = x * x

fib 0 = 0
fib 1 = 1
fib x = (fib (x-2) + (fib (x-1)))

primerArreglo (x:arr) = x

segundoArreglo(_:x:_) = x

suma [] = 0
suma (x:arr) = x + (suma arr)

mapiar _ [] = []
mapiar fn (x:arr) = (fn x):(mapiar fn arr) 

swap (x,y) = (y, x)

--Ejercicios

filtrar _ [] = []

filtrar fn (x:arr) 
	| (fn x) == True = x:(filtrar fn arr)
	| otherwise = (filtrar fn arr)

--Altra cosa
nthElement :: [a] -> Int -> Maybe a 
nthElement [] a = Nothing
nthElement (x:xs) a | a <= 0 = Nothing
                    | a == 1 = Just x
                    | a > 1 = nthElement xs (a-1)

getMenorElemento (x:arr) = menorElemento x arr
	where 
	menorElemento x [] = x
	menorElemento x (y:arr)
		| x < y = menorElemento x arr
		| otherwise = menorElemento y arr

getMayorElemento (x:arr) = mayorElemento x arr
	where 
	mayorElemento x [] = x
	mayorElemento x (y:arr)
		| x > y = mayorElemento x arr
		| otherwise = mayorElemento y arr

getLongitudArr arr = longitudArr arr
	where
		longitudArr [] = 0
		longitudArr [x] = 1
		longitudArr (x:arr) = longitudArr (arr) + 1 

getPromedio arr = promedio arr
	where
		promedio [] = 0
		promedio arr = (suma arr) / (getLongitudArr arr)

findElemento arr y = findElem arr y
	where
		findElem [] y = False
		findElem (x:arr) y 
			| (x == y) = True
			| otherwise = findElem arr y

{-
	
indexOf arr y = findElem arr y
	where
		findElem [] y = False
		findElem (x:arr) y 
			| (x == y) = True
			| otherwise = findElem arr y
-}

takear cant arr = toma cant arr
	where
		toma _ [] = []
		toma cant (y:arr) 
			| cant == 0 = []
			|	otherwise = y:(toma (cant - 1) arr)
--			|	otherwise = y:(trace("Arr: " ++ show (arr)) (toma (cant - 1) arr))

findSubstring cadA cadB = findSub cadA cadB 
	where
		findSub _ [] = False
		findSub cadA (y:cadB)
			| takear (getLongitudArr cadA) (y:cadB) == cadA = True
			| otherwise = findSub cadA cadB 

removerArr cant arr = remover cant arr
	where
		remover _ [] = []
		remover cant (y:arr)
			| cant == 0 = (y:arr)
			| cant == 1 = arr
			| otherwise = remover (cant - 1) arr	

--Ejercicio 6
reemplazar target mark value = reemp target mark value
	where
		reemp [] _ [] = []
		reemp (y:target) mark value
			| (takear (getLongitudArr value) (y:target) ) == mark = 
				value ++ (removerArr (getLongitudArr value) (y:target) ) 
			| otherwise = y:(reemp target mark value) 

--Ejercicio 7
eliminar arr value = elim arr value
	where
		elim [] _ = []
		elim (y:arr) value 
			| y == value = arr
			| otherwise = y:(elim arr value)		

ordenar arr = sorting arr
	where
		sorting [] = []
		sorting arr = 
			(getMenorElemento arr):(sorting 
			(eliminar arr (getMenorElemento arr) ) )

removeDups arr = remDups arr
	where
		remDups [] = []
		remDups [x] = [x]
		remDups (x:y:arr)  
			| x == y = remDups (y:arr)
			| otherwise = x:remDups(y:arr)

orderUnir arr1 arr2 = ordenar(concatenar (ordenar arr1) (ordenar arr2)) 

unionWithoutDups arr1 arr2 = removeDups (orderUnir arr1 arr2) 

--Bono personal
comer index cadFrom cadAt = eat index cadFrom cadAt
	where
		eat _ _ [] = []
		eat index cadFrom cadAt
			| index == 0 = 
				cadFrom ++ (removerArr (getLongitudArr cadFrom) cadAt)
			| otherwise = (takear index cadAt) ++ cadFrom
				++ (removerArr ((getLongitudArr cadFrom) + index) cadAt)

--concatenar arr value = arr++[value]
concatenar arr value = arr++value

{-
partir (value:arr) = partire value arr
	where
		partire _ [] = []
		partire value (y:x:arr)
			| value /= ' ' && y /= ' ' = (value++y)++(partire x arr)
			-}

myLast (x:arr) = miUltimoElemento x arr
	where
		--miUltimoElemento [] [] = []
		--miUltimoElemento [] [] = []
		miUltimoElemento x [] = x
		miUltimoElemento _ (y:arr) = miUltimoElemento y arr

reversar arr = invertir arr
	where
		invertir [] = []
		invertir [x] = [x]
		invertir (x:arr) = 
			myLast(x:arr):invertir( takear ((getLongitudArr (x:arr)) - 1) (x:arr))

palindromo arr = palindro arr
	where
		palindro arr
			| arr == (reversar arr) = True
			| otherwise = False