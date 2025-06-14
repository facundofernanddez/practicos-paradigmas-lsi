

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
  (diasiguales '(23 432 34 54 12) '(23 234 34 23 12)))