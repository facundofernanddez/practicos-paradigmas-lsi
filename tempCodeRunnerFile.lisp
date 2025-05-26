(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun ingresar-lista() 
  (print "Ingrese una lista") (setq lista (read)) ) 

(setq lista-nueva '()) 

(defun crear-sublistas (lista) 
  (cond 
    ((endp lista) lista-nueva) 
    ((consp(car lista)) 
      (push 
        (list (car lista) 
          (setq numero (length(car lista)))) lista-nueva) 
      (crear-sublistas (cdr lista))) 
    (t 
      (crear-sublistas (cdr lista))) ) ) 

(print 
  (crear-sublistas (ingresar-lista)))