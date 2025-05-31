; Las máximas temperaturas del mes de enero se registraron en una lista contenida en la variable max_enero. Definir una función que permita el ingreso por teclado de dicha lista y pueda resolver cada una de las siguientes situaciones.
 ; • Se desea conocer la cantidad de días que se registró una temperatura menor a los 38°.
 ; • Se desea conocer el valor de la temperatura promedio de dicho mes. Definir una función que reciba como parámetro la lista y devuelva el valor deseado.
 ; • Se desea saber si las temperaturas registradas en la lista están ordenadas en forma ascendente (para ello definir una función predicado que verifique lo solicitado)
 ; • Se desea conocer cuales son las diferentes temperaturas registradas. Para ello defina una función que devuelva una lista sin las temperaturas repetidas de la lista original. Si el elemento está repetido debe aparecer una sola vez.
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(setq max_enero '(38 37 36 38 43 32 34 40 39 31 30 43 28)) ; (setq max_enero '(18 19 20 21 22 23 24 25 26 27 28)) 
 

(defun menor_38 (lista) 
  (cond ((endp lista) 0) 
    ((< (car lista) 38) 
      (+ 1 
        (menor_38 (cdr lista)))) 
    (t 
      (menor_38 (cdr lista))))) ; (print(menor_38 max_enero)) 
 

(defun suma (lista) 
  (cond ((endp lista) 0) 
    ((numberp (car lista)) 
      (+ (car lista) (suma (cdr lista)))) (t (suma(cdr lista)))) ) 

(defun promedio (lista) 
  ( / (suma lista) (length lista)) ) ; (print(promedio max_enero)) 
 

(defun ascendente (lista) 
  (cond ((endp lista) t) 
    ((endp (cdr lista)) t) 
    ( 
      (< (car lista) (cadr lista)) 
      (ascendente (cdr lista))) (t nil))) ; (print (ascendente max_enero))
 

(defun sin-repetir (lista) 
  (cond ((endp lista) nil) 
    ( 
      (equal 
        (member (car lista) (cdr lista)) nil) 
      (cons (car lista) 
        (sin-repetir (cdr lista))) ) 
    (t 
      (sin-repetir (cdr lista))) )) ; (print (sin-repetir max_enero)) 
 ; A partir de una lista con las Notas de los parciales de un alumno, que es ingresada por el operador, definir una función predicado llamada Aprobado. La función debe evaluar si la materia está aprobada. Una materia está aprobada si todas las notas son 4 o valores mayores a 4
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun ingresar-lista () 
  (print "Ingrese la lista de notas") (setq notas (read)) ) ; (print(ingresar-lista)) 
 

(defun predicado (lista) 
  (cond ((endp lista) t) 
    ((>= (car lista) 4) 
      (predicado (cdr lista))) (t nil) ) ) ; (print (predicado (ingresar-lista))) 
 ; A partir de una lista heterogénea que es ingresada por el operador, definir una función que permita el ingreso de dicha lista y pueda resolver cada una de las siguientes situaciones:
 ; • Definir una función que permita devolver una lista cuyos elementos serán sublistas. Cada sublista estará conformada por el elemento que sea una sublista de la lista ingresada por el operador junto con su longitud.
 ; • A partir de una lista heterogénea que es ingresada como parámetro, definir una función que devuelva una lista cuyos elementos son el resultado de evaluar uno a uno si cada uno elemento de la lista ingresada como parámetro es una sublista. 
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun ingresar-lista() 
  (print "Ingrese una lista") (setq lista (read)) ) 

(defun crear-sublistas (lista) 
  (cond ((endp lista) nil) 
    ((consp (car lista)) 
      (cons 
        (list (car lista) (length(car lista))) 
        (crear-sublistas (cdr lista))) ) 
    (t 
      (crear-sublistas (cdr lista))) ) ) ; (print (crear-sublistas (ingresar-lista))) 
 

(defun es-lista (lista) 
  (cond ((endp lista) nil) 
    ((not(endp lista)) 
      (cons (consp (car lista)) 
        (es-lista (cdr lista)))) 
    (t 
      (es-lista (cdr lista))) ) ) ; (print (es-lista(ingresar-lista))) 
 ; A partir de dos listas ingresadas como parámetros, definir una función que devuelva una nueva lista donde cada elemento sea el resultado de la diferencia de los elementos de la lista 1 con los elementos de la lista 2 que se encuentren en la misma posición
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun ingresar-lista() 
  (print "Ingrese una lista") (setq lista (read)) ) 

(defun diferencia-lista (lista1 lista2) 
  (cond 
    ( 
      (and(endp lista1) (endp lista2)) nil) 
    ( 
      (and 
        (numberp (car lista1)) 
        (numberp (car lista2))) 
      (cons 
        (- (car lista1) (car lista2)) 
        (diferencia-lista (cdr lista1) (cdr lista2))) ) 
    (t 
      (diferencia-lista (cdr lista1) (cdr lista2))) ) ) ; (print (diferencia-lista (ingresar-lista) (ingresar-lista)))