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