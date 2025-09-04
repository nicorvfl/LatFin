# LatFin — Avances y visualizaciones

**Bienvenida**  
En el siguiente repositorio se podrá visualizar la toma de decisiones con cada campo del proyecto.
---

## Índice
- [Objetivo](#objetivo)
- [FlowChart](#FlowChart)
- [Tabla 1](#Tabla1)
- [Requisitos](#requisitos)
- [Instalación y primer arranque](#instalación-y-primer-arranque)
- [Cómo reproducir los análisis](#cómo-reproducir-los-análisis)
- [Datos](#datos)


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
