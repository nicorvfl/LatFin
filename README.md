# LatFin — Avances y Visualizaciones

## Bienvenida
Este repositorio va a documentar algunas discuiones, el procesamiento y las visualizaciones clave del proyecto.  
Incluye scripts, tablas y figuras organizadas para su análisis y seguimiento.

---

## Índice
1. [Objetivo](#objetivo)
2. [Flowchart](#flowchart)
3. [Tabla 1](#tabla-1)
4. [Adherencia](#adherencia)

---

## Objetivo
El objetivo principal de este repositorio es centralizar y poner a disposición:

1. Scripts de procesamiento y análisis.
2. Tablas descriptivas y comparativas.
3. Figuras y visualizaciones generadas a partir de los datos del proyecto.

---

## Flowchart
La visualización del flujo de participantes contempla:

- **Sujetos screenados**.
- **Sujetos randomizados**  
  - **Criterios**:
    1. `rdz_yn_scr = 1`
    2. `rdz` (rama de intervención) no nulo.
  - **Excluidos**:
    - `rdz_yn_scr = No` y `rdz` nulo.
    - Participantes mal randomizados en Costa Rica:  
      ```
      321-154, 321-156, 321-157, 321-159, 321-163
      ```
    - Inconsistencias (Randomization = "Sí" pero `Arm` nulo):  
      ```
      316-3, 320-24, 320-39, 324-60, 324-73
      ```
- **Asignación a ramas**:
  - Flexibles.
  - Sistemáticos.
- **Evaluaciones**:
  - Una evaluación es "completa" si al menos 3 tareas no presentan datos incompletos.
  - Seguimiento desde *baseline* hasta el mes 24.
- **Indicador de seguimiento**:
  - Participantes con *baseline* y al menos una evaluación de seguimiento (6m, 12m o 24m).

---

## Tabla 1
La **Tabla 1** presenta la caracterización basal y está dividida en tres secciones:

1. Datos sociodemográficos y de estilo de vida.
2. Datos clínicos y de laboratorio.
3. Datos cognitivos.

**Características**:
- Datos del evento `baseline`.
- Comparación por **Arm** (*Flexibles* vs *Sistemáticos*) y total combinado.
- Variables continuas: media, rango, mínimo y máximo.
- Variables categóricas: frecuencia absoluta (n) y porcentaje.

**Nota metodológica**:  
El p-valor provisto por la librería es orientativo. La elección del estadístico debería basarse en la distribución de los datos.

---

## Adherencia
La adherencia se calcula para **Ejercicio físico** y **Reuniones grupales**.

### Ejercicio físico
- **Criterios de inclusión**:
  - Randomizados en rama sistemática.
- **Cálculo**:
  - Primer año: 4 sesiones/semana × 48 semanas = 192 sesiones.
  - Segundo año: 4 sesiones/semana × 52 semanas = 208 sesiones.
  - Total esperado: `192 + 208 = 400 sesiones`.
  - Fórmula:
    ```r
    Porcentaje = (sesiones_asistidas / 400) * 100
    ```
- **Limitación**: no considera *dropout* ni variaciones en seguimiento.


---

### Reuniones grupales
- **Estructura**:
  - Ejercicio físico: 4 sesiones.
  - Nutrición: 4 sesiones.
  - Estimulación cognitiva: 4 sesiones.
  - Coaching de salud: 4 sesiones.
  - Sesiones especiales:
    - 2 en el mes 5.
    - 1 mensual desde mes 6 hasta completar el primer año.
  - Segundo año: 12 sesiones adicionales.
- **Total esperado**: `38 sesiones`.
- **Fórmula**:
  ```r
  Porcentaje = (sesiones_asistidas / 38) * 100

---
