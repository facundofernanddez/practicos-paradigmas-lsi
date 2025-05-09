;Actividad 1

;Rectangulo
(+ (* 2 a) (* 2 b)) ;perimetro atomos 7 sublistas 2 longitud de lista 3
(* a b) ;area atomos 3 sublistas 0

;Cuadrado
(4 a) ;perimetro
(* a a) ;area

;Paralelogramo
(+ (2 a) (2 b)) ;perimetro
(* a b) ;area

;Triangulo
(+ a b c) ;perimetro
(/ (* a altura) 2) ;area

;Circunferencia
(* (* 2 pi) r) ;perimetro
(* pi (* r r)) ;area

;Triangulo rectangulo
(+ (* a a) (* b b)) ;perimetro
(/ (* a b) 2) ;area

;Trapecio
(* (/ (+ b1 b2) 2) h) ;area

;Cilindro

;Actividad 2
;Calcular la cantidad de dólares que podemos comprar con $ 52.300 teniendo en cuenta la cotización DÓLAR BANCO NACIÓN = 1044,50
(/ 52300 1044.50)

;Representar 250 has en m2, sabiendo que 1 ha = 10.000 m2
(* 250 10000)

;Las notas correspondientes a los exámenes finales de un alumno son las siguientes: 7, 5, 10 y 8. Calcular su promedio y que porcentaje de materias aprobadas tiene, considerando que el total de materias en la carrera es 25.
(round(/ (+ 7 5 10 8) 4))
(/ (* 100 4) 25)

; Calcular la cantidad de estantes que tiene una biblioteca, sabiendo que tengo ubicado 30 libros y que en cada estante entran 6 libros.
(/ 30 6)

; Calcular la cantidad de aros que tiene Macarena guardados en 4 cajas. Cada caja tiene 8 pares y en una de ellas tiene además un solo aro.
(+ (* (* 8 2) 3) 1)

; Calcular la cantidad de estampillas que tiene Felipe en su álbum. El mismo tiene 14 páginas y cada página tiene 2 filas con 9 estampillas en cada fila.
(* (* 2 9) 14)

; Calcular la cantidad de plata que tiene ahorrada Pedro si tiene 6 monedas de $10, 4 billetes de $50, 1 billete de $500 y 5 billetes de $100. Si luego gasta ¼ de su dinero, ¿cuánto dinero le queda?
(- (+ (* 6 10)(* 4 50) 500 (* 5 100)) (/ (+ (* 6 10)(* 4 50) 500 (* 5 100)) 4))

; En un tanque hay 357 litros de agua, en otro 49800 centilitros y el tercero 1765 litros. Si se reparte toda el agua en envases de 20 litros ¿Cuántos envases habrá?
(/ (+ (/ 49800 100) 1765 357) 20)

; En un almacén hay 62 sacos de papas. Cada saco pesa 85 kg. Si se venden la mitad de las papas ¿cuántos kilos quedarán sin vender?
(/ (* 62 85) 2)

; Durante su primer año la estación de bomberos recibió 40 alertas. Al año siguiente recibió el 20% más. ¿Cuántas alertas recibió en su corta historia?
(+ 40 (* 40 0.2))