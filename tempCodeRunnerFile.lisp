(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun ingresar-lista () 
  (print "Ingrese la lista de notas") (setq notas (read)) ) ; (print(ingresar-lista)) 
 

(defun predicado (lista) 
  (cond ((endp lista) t) 
    ((>= (car lista) 4) 
      (predicado (cdr lista))) (t nil) ) ) 

(print 
  (predicado (ingresar-lista)))