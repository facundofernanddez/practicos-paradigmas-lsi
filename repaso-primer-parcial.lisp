

(defun armo (A B) 
  (cond 
    ( 
      (and (consp A) (consp B)) (append A B)) 
    ( 
      (and (consp A) (not (consp B))) (list A B)) 
    ( 
      (and (not (consp A)) (consp B)) (cons A B)) (T (list A B)))) 

(print 
  (armo (member 3' (+ 3 4)) (numberp 4))) ; Definir una función llamada ecuación, la cual permite el ingreso por teclado de las variables necesarias para poder resolver la siguiente función de la resolvente
 ; (b + √(b^2 - 4ac)) / 2a
 

(defun ecuacion () 
  (print "Ingrese el valor de a: ") 
  (let ((a (read))) 
    (print "Ingrese el valor de b: ") 
    (let ((b (read))) 
      (print "Ingrese el valor de c: ") 
      (let ((c (read))) 
        (if 
          (and (numberp a) (> a 0) (numberp b) (> b 0) (numberp c) (> c 0)) 
          (let 
            ( 
              (resultado 
                (/ 
                  (+ b 
                    (sqrt 
                      (- (expt b 2 (* 4 a c)))) (* 2 a)))) 
              (format t "El resultado de la ecuación es: ~a" resultado)) 
            (print "Valores inválidos. Ingrese números positivos.")) ) ) ) )) ; Suponiendo que el kilo de azúcar está a $80 y el kilo de café a $1500. Tener en cuenta que un cuarto del monto de dinero que se dispone, se destina para comprar azúcar y el resto para comprar café.
 ; a- Definir una función; la que a partir del monto de dinero ingresado por el operador; devuelva en una lista el monto ingresado por el operador y la cantidad de kilos de azúcar que se puede comprar
 ; b- Definir una función predicado, la que a partir del monto ingresado como parámetro, evalúe si se puede comprar al menos 2 kilos de café.
 

(defun azucar (monto) 
  (if 
    (and (numberp monto) (> monto 0)) 
    (let 
      ( 
        (monto-azucar (* 0.25 monto)) 
        (kilos-azucar (/ monto-azucar 80))) 
      (list monto kilos-azucar)) 
    (print "Monto inválido. Ingrese un número positivo."))) 

(defun cafe (monto) 
  (if 
    (and (numberp monto) (> monto 0)) 
    (let 
      ( 
        (monto-cafe (* 0.75 monto)) 
        (kilos-cafe (/ monto-cafe 1500))) (> kilos-cafe 2)))) ; Se registran los valores del nivel del rio, cada una en una variable diferente, las que son ingresadas por el operador. Desarrollar las funciones necesarias para obtener
 ; a. La dispersión del nivel del rio. Siendo la dispersión, la diferencia entre el valor más alto y el más bajo. Estos valores deben ser ingresados.
 ; b. Determinar si esta dispersión corresponde a días parejos, locos o normales.
 ; • Son días parejos si la dispersión es chica (menos de 30 cm)
 ; • Son días locos si la dispersión es grande (más de un metro)
 ; • Son días normales si no son ni parejos ni locos.
 ; usar la funcion a.
 

(defun dispersion () 
  (print "Ingrese el nivel del río en el día 1: ") 
  (let ((nivel1 (read))) 
    (print "Ingrese el nivel del río en el día 2: ") 
    (let ((nivel2 (read))) 
      (print "Ingrese el nivel del río en el día 3: ") 
      (let ((nivel3 (read))) 
        (if 
          (and (numberp nivel1) (> nivel1 0) (numberp nivel2) (> nivel2 0) (numberp nivel3) (> nivel3 0)) 
          (let 
            ( 
              (maximo 
                (max nivel1 nivel2 nivel3)) 
              (minimo 
                (min nivel1 nivel2 nivel3))) (- maximo minimo)) 
          (print "Niveles inválidos. Ingrese números positivos.")) ) ) ) ) 

(defun clasificar-dias (dispersion) 
  (if 
    (and (numberp dispersión) (> dispersión 0)) 
    (cond 
      ((< dispersión 30) 
        (print "Días parejos.")) 
      ((> dispersión 100) (print "Días locos.")) 
      (t 
        (print "Días normales."))) 
    (print "Dispersión inválida. Ingrese un número positivo."))) ; Se necesita también saber el importe que se deberá pagar por la comida. El valor del cubierto para las personas mayores es de $2500 y para los menores es de $1500. A partir de la cantidad de personas mayores y de la cantidad de personas menores que son ingresadas como parámetro, determinar el total que se debe abonar teniendo en cuenta que:
 ; -Si la cantidad de personas mayores es <= 150, no habrá descuento.
 ; -Si la cantidad de personas mayores es > 150 y <= 200, al total a abonar se le descuenta un 8%.
 ; -Si la cantidad de personas mayores es > a 200, al total a abonar se le descuenta un 12%.
 

(defun costo-comida (mayores menores) 
  (if 
    (and (numberp mayores) (> mayores 0) (numberp menores) (> menores 0)) 
    (let 
      ( 
        (costo-mayores (* mayores 2500)) 
        (costo-menores (* menores 1500)) 
        (total 
          (+ costo-mayores costo-menores))) 
      (cond 
        ((<= mayores 150) total) 
        ( 
          (and (> mayores 150) (<= mayores 200)) (* total 0.92)) 
        ((> mayores 200) (* total 0.88))) ) 
    (print "Cantidad inválida. Ingrese números positivos.")))