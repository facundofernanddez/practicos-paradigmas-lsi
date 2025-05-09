(declaim (sb-ext:muffle-conditions cl:warning))

; A partir de las fórmulas geométricas expresadas como funciones Lisp en la Actividad 1, ¿qué funciones de acceso a listas utilizaría para extraer el signo + en aquellas fórmulas que la contiene? En el caso en que haya mas de un signo +, extraer solo el primero que aparece en la lista
(CAR '(+ 40 (* 40 0.2)))

; Las máximas temperaturas para cada uno de los días del mes de Enero se registraron en una lista que se encuentra contenida en la variable max_temp. Desarrollar las funciones necesarias para obtener los siguientes datos:
; a. ¿Qué temperatura se registra el primer día del mes?
; b. ¿Qué temperatura se registra el último día del mes?
; c. ¿Qué temperatura se registra el tercer día del mes?
; d. ¿Qué temperatura se registra el anteúltimo día del mes?
; e. Verificar si en alguno de los días del mes la máxima fue de 40.
(CAR max_temp)
(CAR(LAST max_temp))
(CADDR max_temp)
(CADR (REVERSE max_temp))
(CAR (MEMBER 40 max_temp))

; La cantidad de lluvia caída en el mes de Enero se registró también en una lista que se encuentra contenida en la variable lluvia_enero con el siguiente formato:
;       ( (dia lluvia_caida) (dia lluvia_caida) …………..)
; donde cada sublista contendrá en:
; • día: número de día del mes en que cayó la lluvia
; • lluvia_caida: la cantidad en mm/h de lluvia caída ese día
; Desarrollar las funciones necesarias para obtener los siguientes datos:
; a. ¿Cuál es el primer día que se informa la cantidad de lluvia caída?
; b. ¿Cuánto llovió el primer día que se informa?
; c. ¿Cuál es el último día que se informa la cantidad de lluvia caída?
; d. ¿Cuánto llovió el último día que se informa?
; e. Informar el día y la cantidad de lluvia caída el 4to día que se registra en la lista (devolver la sublista)
; f. Informar la cantidad de lluvia caída el 4to día que se registra en la lista.
; g. Evaluar si ( 10 9.5) se encuentra en la lista lluvia_enero
(CAR (CAR lluvia_enero))
(CADR (CAR lluvia_enero))
(CAR (CAR (LAST lluvia_enero)))
(CADR (CAR (LAST lluvia_enero)))
(NTH 3 lluvia_enero)
(CADR (nth 3 lluvia_enero))
(CAR (MEMBER '(10 9.5) lluvia_enero))

; Los días del mes de Enero y su temperatura promedio para cada uno de los días se almacenan en dos listas diferentes, cada una almacenada en su respectiva variable. Ellas son:
; • días_enero: contiene los números de los días en el mes de Enero ( 1 2 3 4….)
; • temp_promedio: contiene la temperatura promedio de cada día ( 37 35 37 …)
; Leer con atención el apunte funciones constructoras de listas y luego desarrollar las funciones necesarias para poder resolver los siguientes items
; a. Informar en una lista el primer día del mes junto con su temperatura promedio
; b. Informar en una lista el último día del mes junto con su temperatura promedio
; c. Devolver una nueva lista con todos los elementos de las dos listas. Los mismos deben quedar todos en el primer nivel.
; d. Devolver una nueva lista donde el primer elemento será la lista días_enero y el segundo elemento será la lista temp_promedio
; e. Devolver una nueva lista con todos los elementos de las dos listas. Los mismos deben quedar todos en el primer nivel.
; f. Informar en una lista todas las temperaturas promedios menos la primera y la última.

(declaim (sb-ext:muffle-conditions cl:warning))
(SETQ dias_enero '(1 2 3 4 5 6 7 8 9 10))
(SETQ temp_promedio '(37 35 37 36 38 39 40 41 42 43))

(print(LIST (CAR dias_enero) (CAR temp_promedio)))
(print(LIST (CAR (LAST dias_enero)) (CAR (LAST temp_promedio))))
(print(APPEND dias_enero temp_promedio))
(print(LIST dias_enero temp_promedio))
(print(APPEND (BUTLAST (CDR temp_promedio)) (BUTLAST (CDR dias_enero)))) ; averiguar que onda la funcion rest

