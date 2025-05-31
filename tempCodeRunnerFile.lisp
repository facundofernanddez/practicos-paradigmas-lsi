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
      (diferencia-lista (cdr lista1) (cdr lista2))) ) ) 

(print
  (diferencia-lista (ingresar-lista) (ingresar-lista)))