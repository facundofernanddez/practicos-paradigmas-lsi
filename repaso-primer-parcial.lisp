

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

(defun Triangulo() 
  (let ((a) (b) (c)) 
    (print "Ingrese los lados abc ") (terpri) (setq a (read)) (setq b (read)) (setq c (read)) 
    (cond 
      ( 
        (not 
          (and(> (+ a b) c) (> (+ a c) b) (> (+ b c) a))) 
        (print "No cumple la condicion")) 
      ((= a b c) "Equilatero") 
      ( 
        (or (= a b) (= a c) (= b c)) "Isosceles") (t "Escaleno") ) ) ) ; Una empresa comercializa diferentes tipos de bidones de agua. El precio del bidón depende de la cantidad de agua que contenga. El bidón de 10 litros cuesta ($15.000, El bidón de 20 litros, cuesta el $28.000 y el bidón de 50 litros cuesta $70000, El monto total de la compra será ingresado por el operador.
 ; a) Definir una función, que permita ingresar el monto total de la compra por parte del operador y llame a
 ; cada una de las funciones que se solicitan a continuación:
 ; 1. Definir una función, la que a partir del monto ingresado como parámetro, devuelva una lista formada por sublistas. Cada sublista tendrá como primer elemento un texto que indique el tipo de bidón y como segundo elemento, la cantidad de bidones que se podrían comprar con el monto ingresado para ese tipo de bidón.
 ; 2. El monto total de la compra puede sufrir un descuento que dependerá del siguiente detalle:
 ; • Si el monto a abonar es menor o igual a $75.000, no sufre descuento
 ; • Si el monto a abonar varía entre $75.000 y $200.000, sufre un descuento de $30.000
 ; • Si el monto a abonar es mayor a $200.000, sufre un descuento de $50.000
 ; Definir una función, la que a partir del monto ingresado como parámetro, devuelva el nuevo monto a abonar.
 ; 3. Definir una función predicado, la que a partir del monto ingresado como parámetro, evalúe si con ese monto se pueden comprar 2 bidones de 10 Its y 4 bidones de 50 litros.
 

(defun bidones (monto) 
  (if 
    (and (numberp monto) (> monto 0)) 
    (let 
      ( 
        (bidon10 (/ monto 15000)) 
        (bidon20 (/ monto 28000)) 
        (bidon50 (/ monto 70000))) 
      (list 
        (list "Bidón de 10 litros" bidon10) 
        (list "Bidón de 20 litros" bidon20) 
        (list "Bidón de 50 litros" bidon50)) ) 
    (print "Monto inválido. Ingrese un número positivo."))) 

(defun descuento (monto) 
  (if 
    (and (numberp monto) (> monto 0)) 
    (cond 
      ((<= monto 75000) monto) 
      ( 
        (and (> monto 75000) (<= monto 200000)) (- monto 30000)) 
      ((> monto 200000) (- monto 50000))) 
    (print "Monto inválido. Ingrese un número positivo."))) 

(defun bidones-predicado (monto) 
  (if 
    (and (numberp monto) (> monto 0)) 
    (let 
      ( 
        (costo-bidones 
          (+ (* 2 15000) (* 4 70000))) 
        (cantidad-bidones 
          (/ monto costo-bidones))) 
      (> cantidad-bidones 0)) 
    (print "Monto inválido. Ingrese un número positivo."))) 

(defun main () 
  (print "Ingrese el monto total de la compra: ") 
  (let ((monto (read))) 
    (let 
      ( 
        (bidones-lista (bidones monto))) 
      (print "Lista de bidones:") 
      (dolist (bidon bidones-lista) (print bidon)) 
      (let 
        ( 
          (nuevo-monto (descuento monto))) 
        (print "Nuevo monto a abonar:") (print nuevo-monto) 
        (if 
          (bidones-predicado nuevo-monto) 
          (print "Se pueden comprar 2 bidones de 10 litros y 4 bidones de 50 litros.") 
          (print "No se pueden comprar 2 bidones de 10 litros y 4 bidones de 50 litros.")))))) ;Convertir la siguiente igualdad en una expresión Lisp. a * c-b* c= (a -b) * c
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(setq expresion '(= ((* (- a b) c)) (- (* a c) (* a b)))) ; A partir de la expresión obtenida en el pto a, determinar la cantidad de átomos y listas que tiene dicha expresión y detallar cuales son
 ; (teniendo en cuenta que siempre evaluamos en el primer nivel) atomos 1 listas 2 
 ; A partir de la expresión obtenida en el punto a, extraer el el primer átomo * utilizando las funciones que ud crea conveniente.
 

(print 
  (car 
    (car (car (cdr expresion))))) ; Se registran en una lista los 10 números que salieron en la lotería y el número favorito en una variable.
 ; a. Desarrollar una función que permita el ingreso por parte del operador de la lista y de la variable. La función debe poder llamar a cada una de las funciones solicitadas en los puntos b, c y d.
 ; b. Definir una función predicado; la que a partir de la lista que será ingresada como parámetro, evalúe si el primer y último elemento que se informa en la lista son números pares.
 ; C. Definir una función; la que a partir de la lista y el número favorito que serán ingresados como parámetros devuelva una nueva lista formada por dos sublistas, donde:
 ; • La primer sublista contendrá el texto "numero favorito" y el valor de la variable que registra este dato.
 ; • La segunda sublista contendrá el texto "numeros ganadores" y los elementos de la lista ingresada como parámetro. Ejemplo ("numeros ganadores" 23 15 61)
 ; d. Definir una función; la que a partir de la lista y el numero favorito que serán ingresados como parámetros devuelva el siguiente mensaje:
 ; "numero-ganador", si el favorito es igual al primer número de la lista
 ; "salió-último", si el favorito es igual al último número de la lista
 ; • "no-salió-favorito", si el número favorito no se encuentra en la lista.
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(setq lista '(30 23 87 49 29 18 57 92 34 94)) 

(defun predicado (lista) 
  (and 
    (equal 
      (mod (car(last lista)) 2) 0) 
    (equal (mod (car lista) 2) 0))) ; (print(predicado lista))
 

(defun formar-lista 
  (lista numero-favorito) 
  (list 
    (list "numero favorito" numero-favorito) 
    (cons "numero ganadores" lista)) ) ; (print 
 ; (formar-lista lista 8))
 

(defun mensaje (lista numero) 
  (cond 
    ( 
      (equal (car lista) numero) "numero-ganador") 
    ( 
      (equal (car(last lista)) numero) "salio-ultimo") 
    ( 
      (not (member numero lista)) "no-salio-favorito") ) ) ; (print(mensaje lista 0))
 

(defun principal () 
  (print "Ingrese una lista de numeros") (setq lista1 (read)) 
  (print "Ingrese un numero") (setq numero (read)) 
  (if 
    (or (not(consp lista1)) (not(numberp numero))) 
    (print "Ingrese valores correctos")) 
  (progn 
    (print (predicado lista1)) 
    (print 
      (formar-lista lista1 numero)) 
    (print 
      (mensaje lista1 numero)) ) ) ; (principal) 
 ; Tema 1
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(setq expresion2 '(* k 
  (expt (+ 1 (/ i m)) (* m t)))) ; atomos 2 y listas 1
 

(print 
  (car 
    (car 
      (cdr 
        (car 
          (cdr (cdr expresion2))))))) 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun imprimir-mensaje (dinero valorm3) 
  (cond 
    ( (< dinero valorm3) "monto insuficiente") 
    ( 
      (equal dinero (* valorm3 2)) "monto justo") 
    ( 
      (> dinero (/ valorm3 2)) "monto suficiente") ) ) 

(defun predicado 
  (dinero valorm3arena valorm3piedra) 
  (>= dinero 
    (+ (* valorm3arena 4) (* valorm3arena 1.5))) ) ; (print (predicado 43 34 32))
 

(defun formar-lista 
  (dinero valorm3arena valorm3piedra) 
  (list 
    (list "arena" dinero 
      (/ dinero valorm3arena)) 
    (list "piedra" dinero 
      (/ dinero valorm3piedra))) ) 

(print 
  (formar-lista 3000 32 45))