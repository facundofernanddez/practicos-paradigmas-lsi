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
      (crear-sublistas (cdr lista))) ) ) ; (print (crear-sublistas (ingresar-lista)))
 

(defun es-lista (lista) 
  (cond 
    ((endp lista) lista-nueva) 
    ((consp (car lista)) 
      (push (consp (car lista)) lista-nueva) 
      (es-lista (cdr lista))) 
    (t 
      (push (consp (car lista)) lista-nueva) 
      (es-lista (cdr lista))) ) ) 

(print 
  (es-lista(ingresar-lista)))