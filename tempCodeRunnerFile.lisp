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
      (es-lista (cdr lista))) ) ) 

(print 
  (es-lista(ingresar-lista)))