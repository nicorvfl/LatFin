# LatFin — Avances y visualizaciones

**Bienvenida**  
En el siguiente repositorio se podrá visualizar la toma de decisiones con cada campo del proyecto.
---

## Índice
- [Objetivo](#objetivo)
- [FlowChart](#FlowChart)
- [Tabla 1](#Tabla1)
- [Adherencia](#Adherencia)

---

## Objetivo
El objetivo de este repositorio es brindar (1) los scripts de código, (2) las tablas y (3) las figuras del proyecto.

---

## FlowChart
Esta visualización nos será de utilidad para observar el flujo de participantes que han pasado por lo largo y lo ancho de la intervención. La estructura del Flowchart respeta los siguientes puntos:
- Sujetos Screenados. 
- Sujetos Randomizados. ¿Cómo se decide quién está o no randomizado? Principalmente por dos condiciones, a saber: (a) que en la variable 'rdz_yn_scr' el valor corresponda a un "1" y que en la variable 'rdz' (que asigna a una rama de la intervención) el valor no sea nulo. Por otra parte, si ha colocado un "No" en la variable 'rdz_yn_scr' (que de ahora en más será "Randomization") en caso de que su valor sea "NULL". Asimismo se ha excluido a los participantes mal randomizados de Costa Rica, a saber: "321-154","321-156","321-157","321-159","321-163". Por otra parte, se han excluido los ID's "316-3","320-24","320-39","324-60","324-73" dado que cumplían con la condición de tener un "Sí" en Randomization y un valor nulo en rdz (que de ahora en más llamaremos "Arm"). 
- De los sujetos randomizados, se parte en (a) Flexibles y (b) Sistemáticos. 
- A partir de acá, el gráfico se abre y comienza a contabilizar la cantidad de evaluaciones que tiene cada participante. ¿Cómo se contabiliza esa cantidad? Para ello, creamos una función que coloca un "Completo" en caso de que al menos 3 tareas **no** sean un dato incompleto. 
  
(5) Este flow recorre hasta la evaluación del mes 24. 

## Tabla1

La tabla 1 está dividida en 3 grandes secciones: 
- Datos sociodemográficos y del estilo de vida.
- Datos clínicos y de laboratorio.
- Datos cognitivos.
  
Los datos de la tabla 1 corresponden todos al evento "baseline". Asimismo, se los divide por Arm en Flexibles y Sistemáticos. Se agrega también una tabla con ambos grupos juntos. 
Al momento, se presentan para las variables continuas (a) la media, (b) el rango y (c) el mínimo y el máximo. Para las variables categóricas o discretas se presenta (1) la cantidad (n) y (2) el porcentaje.  

De forma adicional, la librería utilizada calcula una prueba de hipótesis para cada brazo. Este p valor no es del todo confiable ya que la librería solicita especificar el test (ej. ANOVA vs Kruskall Wallis) de acuerdo al tipo de distribución. Dejo en sus manos la opción de hacerlo o no.


## Adherencia  

Construir la adherencia es un desafío dado que el constructo podría ser medido de muchas formas y son extensas las consideraciones posibles. Empezaré por la solución más "brusca" y rudimentaria.  

¿Cómo medir la adherencia al **ejercicio físico**?  
- Primero filtramos la base de datos en (1) personas randomizadas y (2) personas que estén del lado de la intervención sistemática.
- Luego vamos a calcular el total de sesiones de ejercicio físico. La variable está dividida en 4 días durante 48 semanas en el primer año y 52 semanas en el último año. En total, tenemos 100 semanes y 400 posibles días de entrenamiento en el gimnasio.
- De forma rudimentaria, vamos a decir que el total de sesiones posibles de un participante son 400. ¿Por qué rudimentario? Simplemente porque no estamos considerando la posibilidad de dropout.
- Entonces, vamos a calcular un total de reuniones (literalmente: la suma de la asistencia) y un porcentaje de asistencia (calculado como la suma total dividido la cantidad posible de reuniones multiplicado por 100).



