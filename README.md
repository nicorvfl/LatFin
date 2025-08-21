# LatFin — Avances y visualizaciones

**Bienvenida**  
Avances y visualizaciones

---

## Índice
- [Objetivo](#objetivo)
- [Estructura del repositorio](#estructura-del-repositorio)
- [Requisitos](#requisitos)
- [Instalación y primer arranque](#instalación-y-primer-arranque)
- [Cómo reproducir los análisis](#cómo-reproducir-los-análisis)
- [Datos](#datos)
- [Visualizaciones](#visualizaciones)
- [Avances (bitácora)](#avances-bitácora)
- [Entregables y estado](#entregables-y-estado)
- [Convenciones](#convenciones)
- [FAQ](#faq)
- [Créditos](#créditos)
- [Licencia](#licencia)

---

## Objetivo
Repositorio para organizar scripts, tablas y figuras del proyecto.  
La idea es documentar el flujo desde la limpieza de datos hasta las visualizaciones y resultados.

---

## Estructura del repositorio
```text
LatFin/
├─ data/            # datos crudos/derivados (no versionar crudos grandes)
│  └─ .gitkeep
├─ scripts/         # scripts de R (ETL, análisis, figuras)
├─ notebooks/       # Rmd / Quarto / Jupyter
├─ figs/            # figuras exportadas (png/pdf/svg)
├─ reports/         # reportes intermedios (HTML/PDF)
├─ src/             # funciones reutilizables (helpers)
├─ README.md
├─ .gitignore
└─ LatFin.Rproj     # proyecto de RStudio (opcional pero recomendado)

