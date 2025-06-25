

(defun treselementos (lista) 
  (cond ((endp lista) t) 
    ( 
      (and (consp (car lista)) 
        (= 
          (length (car lista) 3))) 
      (treselementos(cdr lista))) (t nil) ) ) ; ((1 232 A) (0 349834 B))
 ; car (1 232 A)
 ; cdr, (232 A)
 ; cdr, (A)
 ; car A
 

(defun alfayuno (lista) 
  (cond ((endp lista) 0) 
    ( 
      (and 
        (equal (caar lista) 1) 
        (equal (caddar lista) "A")) 
      (+ 1 
        (alfayuno (cdr lista)))) 
    (t 
      (alfayuno (cdr lista))) ) ) 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun solointenso (lista) 
  (mapcar 
    (lambda (x) 
      (if (numberp (cadr x)) (cadr x))) lista)) ; (print (solointenso '((1 232 "A") (0 349834 "B"))))
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun soloalfa (lista) 
  (cond ((endp lista) nil) 
    ( 
      (and (atom (caddar lista)) 
        (equal (caddar lista) "A")) 
      (cons (car lista) 
        (soloalfa (cdr lista)))) 
    (t 
      (soloalfa (cdr lista))) ) ) ; (print (soloalfa '((1 232 "A") (0 349834 "B") (-1 345 "A") (1 78 "B")) ))
 

(defun eval (lista) 
  (mapcar 
    (lambda (x) 
      (cond 
        ( 
          (not(numberp (car x))) nil) 
        ((= (car x) 1) "0011") 
        ((= (car x) -1) "0000") (t "1001") )) lista) ) 

(defun clasificarOnda (lista) 
  (mapcar 
    (lambda (x) 
      (cond 
        ( 
          (not (numberp (car x))) nil) 
        ((= (car x) -1) '0000) ((= (car x) 1) '0011) (t '1001))) lista)) 

(print 
  (eval '((1 232 "A") (0 349834 "B") (-1 345 "A") (1 78 "B")))) ; TEMA 3
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun predicado (lista) 
  (cond ((endp lista) t) 
    ( 
      (and (consp (car lista)) (= (length lista) 3)) 
      (predicado (cdr lista))) (t nil) ) ) 

(defun reemplazar (lista) 
  (mapcar 
    (lambda (x) 
      (cond 
        ((equal (caddr x) "P") 
          (list (car x) (cadr x) "Pendiente")) 
        ( 
          (equal (caddr x) "EP") 
          (list (car x) (cadr x) "En proceso")) 
        ((equal (caddr x) "F") 
          (list (car x) (cadr x) "Finalizado")) )) lista)) ; (print (reemplazar '(("pintar" 43 "P") ("dibujar" 23 "EP") ("matear" 98 "F")))) 
 

(defun igualatiempo (lista numero) 
  (cond ((endp lista) 0) 
    ( 
      (and (consp (car lista)) 
        (numberp (cadar lista)) 
        (equal (cadar lista) numero)) 
      (+ 1 
        (igualatiempo (cdr lista) numero))) 
    (t 
      (igualatiempo (cdr lista) numero)) ) ) 

(defun finalizados (lista) 
  (cond ((endp lista) nil) 
    ( 
      (and (consp (car lista)) 
        (equal (caddar lista) "F")) 
      (cons (car lista) 
        (finalizados (cdr lista)))) 
    (t 
      (finalizados (cdr lista))) ) ) ; (print (finalizados '(("pintar" 43 "F") ("dibujar" 23 "EP") ("matear" 98 "F"))))
 

(defun nombres (lista) 
  (mapcar 
    (lambda (x) 
      (if (atom (car x)) (car x))) lista) ) ; (print (nombres '(("pintar" 43 "F") ("dibujar" 23 "EP") ("matear" 98 "F"))))
 

(defun atiempo (lista numero) 
  (mapcar 
    (lambda (x) 
      (cond 
        ( 
          (or 
            (not (numberp numero)) 
            (not (numberp (cadr x)))) nil) 
        ( 
          (equal (cadr x) numero) "Tiempo justo") 
        ((< (cadr x) numero) "tiempo-temprano") (t "tiempo-excedido") ) ) lista) ) ; (print (atiempo '(("pintar" 43 "F") ("dibujar" 23 "EP") ("matear" 98 "F")) 43))
