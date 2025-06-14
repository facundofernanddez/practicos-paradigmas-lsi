; Actividad Nº 3.
 ; Definir una función; la que a partir de una lista ingresada por el operador; devuelva una nueva lista cuyos elementos sean el resultado de evaluar si cada elemento de la lista original es o no un elemento numérico
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun es-numero (lista) 
  (mapcar 'numberp lista) ) ; Actividad Nº 4:
 ; Realizar los cambios necesarios en la función definida en la Actividad N° 3, de tal manera que el resultado de cada evaluación realizada devuelva la leyenda SI o NO.
 

(defun es-numero2 (lista) 
  (mapcar 
    (lambda (x) 
      (if (numberp x) "SI" "NO")) lista) ) ; (print (es-numero2 '(23 a 34 lsd 34 32)))
 ; Actividad Nº 5:
 ; Definir una función que solicite al operador el ingreso de una lista no vacía y un número entero, de tal manera que devuelva una lista formada por sublistas. Cada sublista estará formada por el elemento de la lista original junto con su potencia ( el exponte de la potencia será el número entero ingresado por el operador)
 

(defun potencia () 
  (print "Ingrese una lista no vacia") (setq lista (read)) 
  (print "Ingrese un numero") (setq numero (read)) 
  (mapcar 
    (lambda (x) 
      (if (numberp x) 
        (list x (expt x numero)))) lista) ) ; (print(potencia))
 ; Actividad Nº 6
 ; Definir una función la que a partir de una lista heterogénea ingresada por el operador, devuelva una nueva lista formada por sublistas. Cada sublista será el resultado de comparar el elemento de la lista con 0 (cero) y tendrá el siguiente formato: ( X signo 0), donde
 ; • X: será el elemento de la lista original
 ; • Signo: será <, > o = dependiendo si el valor del elemento de la lista original es mayor, menor o igual a cero.
 ; • 0: será una constante que se corresponderá al valor cero 
 

(defun comparar (lista) 
  (mapcar (lambda (x) )) )