(defun armo (A B) 
  (cond 
    ( 
      (and (consp A) (consp B)) (append A B)) 
    ( 
      (and (consp A) (not (consp B))) (list A B)) 
    ( 
      (and (not (consp A)) (consp B)) (cons A B)) (T (list A B)))) 

(print 
  (armo (member 3' (+ 3 4)) (numberp 4)))