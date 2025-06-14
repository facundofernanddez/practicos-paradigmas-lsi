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