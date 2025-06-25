

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun materiasing (lista) 
  (cond ((endp lista) 0) 
    ((consp (car lista)) 
      (+ 1 
        (materiasing (cdr lista)))) 
    (t 
      (materiasing (cdr lista))) ) ) 

(defun notamenores (lista atomo) 
  (cond ((endp lista) 0) 
    ( 
      (and 
        (numberp (cadar lista)) 
        (< (cadar lista) atomo) (/= (cadar lista) 0)) 
      (+ 1 
        (notamenores (cdr lista) atomo))) 
    (t 
      (notamenores (cdr lista) atomo)) ) ) 

(defun materiasnoaprobadas (lista) 
  (cond ((endp lista) nil) 
    ( 
      (and (< (cadar lista) 4) (/= (cadar lista) 0)) 
      (cons (caar lista) 
        (materiasnoaprobadas (cdr lista))) ) 
    (t 
      (materiasnoaprobadas (cdr lista))) ) ) 

(print 
  (materiasing '(("lengua" 5)("mates" 0)("computacion" 8)("economia" 3) ("frances" 2)))) 

(print 
  (notamenores '(("lengua" 5)("mates" 0)("computacion" 8)("economia" 3) ("frances" 2)) 4)) 

(print 
  (materiasnoaprobadas '(("lengua" 5)("mates" 0)("computacion" 8)("economia" 3) ("frances" 2)))) ; Definir una función, la que a partir de las dos listas que serán ingresadas por el operador, determin cantidad de días en las que las cantidades de pasajeros son iguales en la misma posición de la lista. Po ejemplo, si listal contiene (120 210 315 478 532 620) y lista2 contiene ( 540 210 820 478 710 900 420), cantidad de días en que las dos listas tienen la misma cantidad de pasajeros para el mismo día es 2.
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun diasiguales (lista1 lista2) 
  (cond 
    ( 
      (or (endp lista1) (endp lista2)) 0) 
    ( 
      (and 
        (numberp (car lista1)) 
        (numberp (car lista2)) 
        (= (car lista1) (car lista2))) 
      (+ 1 
        (diasiguales (cdr lista1) (cdr lista2)))) 
    (t 
      (diasiguales (cdr lista1) (cdr lista2))) ) ) 

(print 
  (diasiguales '(23 432 34 54 12) '(23 234 34 23 12))) ; con 2 listas heterogeneas verificar si dos elementos en la misma posicion de ambas listas son pares, si es asi, crear una lista que contenga sublistas que contengan tales numeros.
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun numpares (lista1 lista2) 
  (cond 
    ( 
      (or (endp lista1) (endp lista2)) nil) 
    ( 
      (and 
        (numberp (car lista1)) 
        (numberp (car lista2)) (evenp (car lista1)) (evenp (car lista2))) 
      (cons 
        (list (car lista1) (car lista2)) 
        (numpares (cdr lista1) (cdr lista2)))) 
    (t 
      (numpares (cdr lista1) (cdr lista2))) ) ) 

(print 
  (numpares '(24 "as" 65 22) '(44 78 "asd" 12))) ; crear una funcion predicado que recorra y verifique si una lista heterogenea posee todas sus sublistas con longitud mayor a 2.
 

(defun mayor2 (lista) 
  (cond ((endp lista) t) 
    ( 
      (and (consp (car lista)) 
        (equal (length (car lista)) 2)) (mayor2 (cdr lista))) (t nil) ) ) ; crear una funcion con mapcar, que recibe la lista y la variable, que cree una lista que contenga sublistas en donde cada sublista tiene forma (mensaje cantPacientes1).
 

(defun pacientes (lista variable) (mapcar ) ) ; La cantidad de pasajeros que llegaron a la terminal en el mes de marzo y abril se registraron en dos listas diferentes de la siguiente manera:
 ; Pasajeros_Marzo (cada elemento de la lista corresponde al total de pasajeros que ingresaron a la terminal en 1 dia, por lo que esta lista tendra 31 elementos)
 ; Pasajeros_Abril (cada elemento de la lista corresponde al total de pasajeros que ingresaron a la terminal 1 dia, por lo que esta lista tendra 30 elementos)
 ; Se tiene tambien una variable con la cantidad de pasajeros que se espera que ingrese a la terminal.
 ; 1- Definir una funcion la que a partir de dos listas que seran ingresadas por el operador, determine la cantidad de dias en las que cantidades de pasajeros son iguales en la misma posicion de la lista.
 ; Por ejemplo si la lista 1 contiene (120 210 315 478 532 620) y la lista 2 contiene (540 210 820 478 710 500 420) la cantidad de dias en la que las dos listas tienen la misma cantidad de pasajeros para el mismo dia es 2.
 

(defun igualcantidad (lista1 lista2) 
  (cond 
    ( 
      (or (endp lista1) (endp lista2)) 0) 
    ( 
      (and 
        (numberp (car lista1)) 
        (numberp (car lista2)) 
        (equal (car lista1) (car lista2))) 
      (+ 1 
        (igualcantidad(cdr lista1) (cdr lista2)))) 
    (t 
      (igualcantidad (cdr lista1) (cdr lista2))) ) ) ; 2- Desarrollar una función utilizando mapcar, la que a partir de la lista conteniendo los pasajeros en el mes de marzo y la variable que contiene un valor esperado, ambos ingresados como parámetros, devuelva una nuevaLista formada por sublistas. Cada sublista contendrá un mensaje y la cantidad de pasajeros en el día. La lista deberá tener la siguiente forma: ((canP1 mensaje)) ((cantP2 mensaje)..)
 ; Donde: cantP1, cantP2, será la cantidad de pasajeros que ingresan a la terminal cada día. El mensaje será "pocos" cuando la cantidad de pasajeros de un determinado día sea menor al valor esperado. "Esperado" cuando la cantidad de pasajeros de un determinado día sea igual al valor esperado. Y "Muchos" cuando la cantidad de pasajeros sea mayor al valor esperado.
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun cantidadesperada (lista numero) 
  (mapcar 
    (lambda (x) 
      (cond 
        ( 
          (or (not(numberp x)) (not(numberp numero))) nil) 
        ((< x numero) (list x "pocos")) 
        ( (equal x numero) (list x "esperado")) (t (list x "muchos")) ) ) lista) ) ; (print (cantidadesperada '(23 34 456 23 54) 23))
 ; 3-Se estima que el 30% de los pasajeros viajan más de una vez al año. Desarrollar una función usando mapcar, la que a partir de la lista conteniendo los pasajeros del mes de abril, la que será ingresada como parámetro, devuelva una nueva lista, conteniendo la cantidad de pasajeros que viajan más de una vez al año. Recordar que el porcentaje se calcula de la siguiente manera (30 * total de pasajeros en 1 día/100).
 

(defun 30porciento (lista) 
  (mapcar 
    (lambda (x) 
      (if 
        (> x (/ (* x 30) 100)) x)) lista) ) ; (print(30porciento '(23 34 456 23 54)))
 ; 4-Desarrollar una función predicado la que a partir de una lista heterogénea (puede contener cualquier elemento) ingresada como parámetro evalúe si todos los elementos son numéricos que posee son par.
 

(defun espar (lista) 
  (cond ((endp lista) t) 
    ( 
      (and(numberp (car lista)) (evenp (car lista))) (espar (cdr lista))) (t nil) ) ) ; 5-Desarrolla una función la que a partir de dos listas heterogénea (puede contener cualquier elemento) ambas ingresadas como parámetro, devuelva una nueva lista formada por sublistas, cada sublitas contendrá elementos a igual posición que sean listas
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun juntar (lista1 lista2) 
  (cond 
    ( 
      (or (endp lista1) (endp lista2)) nil) 
    ( 
      (cons 
        (list (car lista1) (car lista2) ) 
        (juntar (cdr lista1) (cdr lista2)))) 
    (t 
      (juntar (cdr lista1) (cdr lista2))) ) ) 

(print 
  (juntar '(23 "asd" 456 23 nil) '(23 34 456 ("asdf" 34) 54 89)))