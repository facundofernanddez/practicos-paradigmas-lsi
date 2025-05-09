; A partir de un determinado monto en pesos, que será ingresado por operador, definir una función que devuelva la cantidad de dólares que se pueden comprar con el monto ingresado. 
 

(declaim 
  (sb-ext:muffle-conditions cl:warning)) 

(defun convertir-a-dolares () 
  (print "Ingrese el monto en pesos: ") 
  (let ((monto (read))) 
    (if 
      (and (numberp monto) (> monto 0)) 
      (let ((dolar-actual 1100)) ; Suponiendo un valor ficticio del dólar
 
        (print "El monto en dólares es: ~a" 
          (/ monto dolar-actual)) ) 
      (print "Monto inválido. Ingrese un número positivo.")) ) ) 
(convertir-a-dolares) ; Llamada a la función con un monto de ejemplo.
 ; Recordando la fórmula que permite calcular el volumen de un cilindro, definir una función que permita el ingreso por teclado de las variables necesarias para ejecutar dicha función y obtener el valor del volumen. 
 ; La fórmula es: V = π * r^2 * h, donde r es el radio y h es la altura del cilindro.
 

(defun calcular-volumen-cilindro () 
  (print "Ingrese el radio del cilindro: ") 
  (let ((radio (read))) 
    (print "Ingrese la altura del cilindro: ") 
    (let ((altura (read))) 
      (if 
        (and (numberp radio) (> radio 0) (numberp altura) (> altura 0)) 
        (format t "El volumen del cilindro es: ~a" 
          (* pi (expt radio 2) altura)) 
        (print "Valores inválidos. Ingrese números positivos.")) ) ) ) 

(calcular-volumen-cilindro) ; Llamada a la función con valores de ejemplo.
 ; Definir una función predicado llamada palíndromo, que indique si una lista ingresada por el operador, es una lista palíndromo (se lee igual de izquierda a derecha y de derecha a izquierda). Por ejemplo: ( I T A T I) es una lista palíndromo. No de manera recursiva.
 

(defun palindromo () 
  (print "Ingrese una lista: ") 
  (let ((lista (read))) 
    (if 
      (and (listp lista) (> (length lista) 0) 
        (equal lista (reverse lista))) 
      (print "La lista es un palíndromo.") 
      (print "La lista no es un palíndromo.")) 
    (print "Lista inválida. Ingrese una lista no vacía." ))) 
(palindromo) ; Llamada a la función con una lista de ejemplo.
 ; A partir de un determinado valor de temperatura, que será ingresado por el operador, definir una función llamada clima que me indique el estado del clima, teniendo en cuenta:
 ; Temperatura Clima
 ; < 0 Helado
 ; = 0 y < 10 Frio
 ; = 10 y < 20 Templado
 ; = 20 y < 30 Cálido
 ; >30 Abrasador 
 

(defun clima () 
  (print "Ingrese la temperatura: ") 
  (let 
    ((temperatura (read))) 
    (if 
      (and (numberp temperatura) (> temperatura -100)) 
      (cond 
        ((< temperatura 0) 
          (print "Clima: Helado")) 
        ( 
          (and (= temperatura 0) (< temperatura 10)) (print "Clima: Frío")) 
        ( 
          (and (= temperatura 10) (< temperatura 20)) 
          (print "Clima: Templado")) 
        ( 
          (and (= temperatura 20) (< temperatura 30)) 
          (print "Clima: Cálido")) 
        ((> temperatura 30) 
          (print "Clima: Abrasador")) ) 
      (print "Temperatura inválida. Ingrese un número válido.")) ) ) 
(clima) ; Llamada a la función con un valor de temperatura de ejemplo.
 ; Definir una función llamada mi-segundo, la que a partir de una lista y un átomo ingresados por el operador deberá devolver una nueva lista donde el átomo ocupará la segunda posición de la lista. (Recuerde que las posiciones comienzan a contarse desde la 0) 
 ; Por ejemplo: (mi-segundo '(1 2 3) 4) devolverá (1 4 2 3)
 

(defun mi-segundo () 
  (print "Ingrese una lista: ") 
  (let ((lista (read))) 
    (print "Ingrese un átomo: ") 
    (let ((atomo (read))) 
      (if 
        (and (atom atomo) (listp lista) (> (length lista) 0)) 
        (format t "La nueva lista es: ~a" 
          (append 
            (list (car lista) atomo) (cdr lista))) 
        (print "Lista inválida. Ingrese una lista no vacía.")) ) ) ) 
(mi-segundo) ; Llamada a la función con una lista y un átomo de ejemplo.
 ; Definir una función llamada clasifico-triángulos, la que a partir de los valores de los lados de un triángulo ingresados por el operador, clasifique el mismo en: isósceles, equilátero o escaleno. Tener en cuenta que todo triangulo debe cumplir que. “Un lado es menor que la suma de los otros dos y mayor que la diferencia”. 
 

(defun clasifico-triangulos () 
  (print "Ingrese el lado 1 del triángulo: ") 
  (let ((lado1 (read))) 
    (print "Ingrese el lado 2 del triángulo: ") 
    (let ((lado2 (read))) 
      (print "Ingrese el lado 3 del triángulo: ") 
      (let ((lado3 (read))) 
        (if 
          (and (numberp lado1) (> lado1 0) (numberp lado2) (> lado2 0) (numberp lado3) (> lado3 0) 
            (< lado1 (+ lado2 lado3)) 
            (< lado2 (+ lado1 lado3)) 
            (< lado3 (+ lado1 lado2)) 
            (> lado1 (- lado2 lado3)) 
            (> lado2 (- lado1 lado3)) 
            (> lado3 (- lado1 lado2))) 
          (cond 
            ( 
              (and (= lado1 lado2) (= lado2 lado3)) 
              (print "El triángulo es equilátero.")) 
            ( 
              (or (= lado1 lado2) (= lado2 lado3) (= lado1 lado3)) 
              (print "El triángulo es isósceles.")) 
            (t 
              (print "El triángulo es escaleno."))) ) 
        (print "Lados inválidos. Ingrese números positivos.")) ) ) ) 

(clasifico-triangulos) ; Llamada a la función con lados de ejemplo.
 ; Definir una función llamada mediano¸ la que a partir de tres valores numéricos ingresados por el operador, devuelva el valor mediano (puede ayudarse con las funciones max y min). El valor mediano será aquel que no es ni el mayor ni el menor de los tres valores ingresados.
 ; Por ejemplo: (mediano 1 9 5) devolverá 5.
 

(defun mediano () 
  (print "Ingrese el primer número: ") 
  (let ((num1 (read))) 
    (print "Ingrese el segundo número: ") 
    (let ((num2 (read))) 
      (print "Ingrese el tercer número: ") 
      (let ((num3 (read))) 
        (if 
          (and (numberp num1) (> num1 0) (numberp num2) (> num2 0) (numberp num3) (> num3 0)) 
          (let 
            ( 
              (maximo (max num1 num2 num3)) 
              (minimo (min num1 num2 num3))) 
            (format t "El valor mediano es: ~a" 
              (- (+ num1 num2 num3) maximo minimo))) 
          (print "Números inválidos. Ingrese números positivos.")) ) ) ) ) 
(mediano) ; Llamada a la función con números de ejemplo.
 ; La máxima temperatura de ayer y las máximas temperaturas de enero y febrero se registran en dos listas,
 ; • max_enero: que contendrá las máximas temperaturas registradas para cada uno de los días de enero
 ; • max_febrero: que contendrá las máximas temperaturas registradas para cada uno de los días de febrero
 ; definir una función predicado (solo devuelve T o NIL) que permita determinar si la temperatura máxima de ayer se registró también en enero o en febrero. (el valor atómico y las dos listas deben ser ingresadas por el operador)
 ; Por ejemplo: (temperatura_maxima 35 '(30 32 35 28) '(31 29 30)) devolverá T 
 

(defun temperatura_maxima () 
  (print "Ingrese la temperatura máxima de ayer: ") 
  (let 
    ( 
      (temp_max_ayer (read))) 
    (print "Ingrese la lista de temperaturas máximas de enero: ") 
    (let ((max_enero (read))) 
      (print "Ingrese la lista de temperaturas máximas de febrero: ") 
      (let 
        ((max_febrero (read))) 
        (if 
          (and 
            (numberp temp_max_ayer) (> temp_max_ayer 0) (listp max_enero) 
            (> (length max_enero) 0) (listp max_febrero) 
            (> (length max_febrero) 0) 
            (consp 
              (member temp_max_ayer max_enero)) 
            (consp 
              (member temp_max_ayer max_febrero )))))))) ; (if 
 ; (or 
 ; (member temp_max_ayer max_enero :test 'equal) 
 ; (member temp_max_ayer max_febrero :test 'equal)) 
 ; (print "La temperatura máxima de ayer se registró en enero o febrero.") 
 ; (print "La temperatura máxima de ayer no se registró en enero ni febrero.")) 
 ; (print "Valores inválidos. Ingrese números positivos y listas no vacías.")) ) ) ) ) 
 
(temperatura_maxima) ; Llamada a la función con un valor de temperatura y listas de ejemplo.
 ; Las máximas temperaturas para cada uno de los días del mes de Enero se registraron en una lista que se encuentra contenida en la variable max_temp. Definir una función que permita el ingreso de la misma y llame a cada una de las funciones desarrolladas en la Actividad 2.2. a,b,c y d. Recordar que cada item debe desarrollarse dentro de una función diferente ya que cada función permite devolver un único elemento.
 ; Por ejemplo: (max_temp '(30 32 35 28))
 ; (SETQ dias_enero '(1 2 3 4 5 6 7 8 9 10)) 
 ; (SETQ temp_promedio '(37 35 37 36 38 39 40 41 42 43)) 
 

(defun primer-funcion() 
  (print 
    (LIST (CAR dias_enero) (CAR temp_promedio)))) 

(defun segunda-funcion() 
  (print 
    (LIST 
      (CAR (LAST dias_enero)) 
      (CAR (LAST temp_promedio))))) 

(defun tercera-funcion() 
  (print 
    (APPEND dias_enero temp_promedio))) 

(defun cuarta-funcion() 
  (print 
    (LIST dias_enero temp_promedio))) 

(defun quinta-funcion() 
  (print 
    (APPEND 
      (BUTLAST (CDR temp_promedio)) 
      (BUTLAST (CDR dias_enero))))) ; averiguar que onda la funcion rest
 

(defun max_temperatura() 
  (print "Ingrese la lista de temperaturas máximas de enero: ") 
  (let ((max_temp (read))) 
    (if 
      (and (listp max_temp) 
        (> (length max_temp) 0)) 
      (progn (primer-funcion) (segunda-funcion) (tercera-funcion) (cuarta-funcion) (quinta-funcion)) 
      (print "Lista inválida. Ingrese una lista no vacía.")) ) ) 
(max_temperatura) ; Llamada a la función con una lista de ejemplo.
 ; Definir una función predicado (solo devuelve T o NIL sin print texto) para cada una de los ítems que se detallan a continuación. Cada función definida debe recibir como parámetro la lista contenida en la variable max_temp.
 ; a. Evaluar si la temperatura registrada el primer día está comprendida entre los 40 y 45 grados. 
 ; b. Evaluar si en alguno de los días del mes la máxima fue de 40.
 ; c. Evaluar si la temperatura del primer y último día son IGUALES. 
 

(defun primer-dia-entre-40-y-45 (max_temp) 
  (if 
    (and (listp max_temp) 
      (> (length max_temp) 0)) 
    (let 
      ( 
        (primer-dia (car max_temp))) 
      (and (> primer-dia 40) (< primer-dia 45))) nil)) 

(defun temperatura-40 (max_temp) 
  (if 
    (and (listp max_temp) 
      (> (length max_temp) 0)) 
    (member 40 max_temp :test 'equal) nil)) 

(defun primer-y-ultimo-dia-iguales (max_temp) 
  (if 
    (and (listp max_temp) 
      (> (length max_temp) 0)) 
    (let 
      ( 
        (primer-dia (car max_temp)) 
        (ultimo-dia (car (last max_temp)))) 
      (equal primer-dia ultimo-dia)) nil)) 

(primer-dia-entre-40-y-45 '(30 32 35 28)) ; Llamada a la función con una lista de ejemplo.
 

(temperatura-40 '(30 32 35 28)) ; Llamada a la función con una lista de ejemplo.
 

(primer-y-ultimo-dia-iguales '(30 32 35 28)) ; Llamada a la función con una lista de ejemplo.
 ; Definir una función llamada posición, que reciba como argumentos un elemento y una lista e indique la posición que ocupa el elemento en la lista.
 

(defun posicion (elemento lista) 
  (if 
    (and (atom elemento) (listp lista)) 
    (let 
      ( 
        (posicion 
          (position elemento lista :test 'equal))) 
      (if posicion 
        (format t "El elemento ~a se encuentra en la posición: ~a" elemento posicion) 
        (print "El elemento no se encuentra en la lista."))))) 

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