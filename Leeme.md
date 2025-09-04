# LatFin — Avances y visualizaciones

**Bienvenida**  
En el siguiente repositorio se podrá visualizar la toma de decisiones con cada campo del proyecto.
---

## Índice
- [Objetivo](#objetivo)
- [FlowChart](#FlowChart)
- [Estructura del repositorio](#estructura-del-repositorio)
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
- A partir de acá, el gráfico se abre y comienza a contabilizar la cantidad de evaluaciones que tiene cada participante. ¿Cómo se contabiliza esa cantidad? Para ello, creamos una función que coloca un "Completo" en caso de que al menos 3 tareas **no** sean un dato incompleto. Por ejemplo:  

ID     | MMSE  |  DigitSpan  |  FreeRecall  |  EvaluacionCompleta  |  
312-2     23         4              12             Completa  
312-2     NA         4              12             Incompleta  
  
(5) Este flow recorre hasta la evaluación del mes 24. 


