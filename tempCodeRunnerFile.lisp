(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun suma (lista) 
  (cond ((endp lista) 0) 
    ((numberp (car lista)) 
      (+ (car lista) (suma (cdr lista))) ) 
    (t (suma (cdr lista)))) ) 

(defun mas-grande (lista) 
  (cond ((endp lista) nil) 
    ((endp (cdr lista)) (car lista)) 
    (t 
      (max (car lista) 
        (mas-grande (cdr lista)))) ) ) ; (print (mas-grande '(23 4 5 78 90 120))) 
 

(defun mas-chico (lista) 
  (cond ((endp lista) nil) 
    ((endp (cdr lista)) (car lista)) 
    (t 
      (min (car lista) 
        (mas-chico (cdr lista)))) ) ) 

(print 
  (mas-chico '(23 4 5 78 90 120))) 