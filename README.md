# Template reporte reproducibilidad

Link al reporte [**AQUÍ**](https://x0czz.github.io/Murua_Outerbridge/)

Este repositorio contiene una plantilla para el reporte de reproducibilidad del Trabajo 1 del curso [Investigación Social Abierta](https://cienciasocialabierta.cl/2026/). La plantilla está diseñada para ser clonada y modificada por cada estudiante, siguiendo el protocolo [IPO](https://lisacoes.com/protocolos/a-ipo-rep/) (IInput-Processing-Output) y utilizando el formato Quarto.

<img src="https://lisacoes.com/protocolos/a-ipo-rep/ipo-hex.png" alt="IPO" width="220" />

## Working tree del proyecto

Este proyecto se organiza de la siguiente manera: 

<!-- WORKING_TREE_START -->
```text
Murua_Outerbridge/
 |- .vscode/
 |  |- settings.json
 |- README.md
 |- explorar_filtro.R
 |- index.html
 |- index.log
 |- index.qmd
 |- index.tex
 |- input/
 |  |- bib/
 |  |  |- apa.csl
 |  |  |- ciencia_abierta.bib
 |  |- data/
 |  |  |- original/
 |  |  |  |- 0A_Listado_Variables_Global_ELSOC_v2022.xlsx
 |  |  |  |- ELSOC_Long.RData
 |  |  |  |- ELSOC_Long_2016_2023.RData
 |  |  |  |- ELSOC_Wide.RData
 |  |  |  |- ELSOC_Wide_2016_2023.RData
 |  |  |  |- Perfiles_Votante.R
 |  |  |  |- perfiles.RData
 |  |  |  |- perfiles_participacion.RData
 |  |  |- proc/
 |  |  |  |- ELSOC_selected_full.RDS
 |  |  |  |- ELSOC_selected_no_ola5.RDS
 |  |- images/
 |  |  |- carabineros barra.png
 |  |  |- carabineros serie.png
 |  |  |- congreso barra.png
 |  |  |- congreso serie.png
 |  |  |- judicial barra.png
 |  |  |- judicial serie.png
 |  |  |- partidos barra.png
 |  |  |- partidos serie.png
 |  |- original-code/
 |  |  |- og_proc.R
 |- libs/
 |  |- ocs.scss
 |- output/
 |  |- graphs/
 |  |  |- chunks/
 |  |  |  |- 003_final-c05_07-bar.png
 |  |  |  |- 004_final-c05_07-long.png
 |  |  |  |- 005_final-c05_05-bar.png
 |  |  |  |- 006_final-c05_05-long.png
 |  |  |  |- 007_final-c05_02-bar.png
 |  |  |  |- 008_final-c05_02-long.png
 |  |  |  |- 009_final-c05_03-bar.png
 |  |  |  |- 010_final-c05_03-long.png
 |  |  |  |- 011_graf-congreso-2023-6-4.png
 |  |  |  |- 012_graf-congreso-olas-7-5-alt.png
 |  |  |  |- 013_graf-congreso-olas-7-5.png
 |  |  |  |- 014_graf-poder-judicial-2023-sin-atricion.png
 |  |  |  |- 015_graf-poder-judicial-olas-7-5.png
 |  |  |  |- 016_graf-partidos-2023-sin-atricion.png
 |  |  |  |- 017_graf-partidos-olas-7-5.png
 |  |  |  |- 018_graf-carabineros-2023-muestra1-ola7.png
 |  |  |  |- 019_graf-carabineros-olas-7-5.png
 |  |- tables/
 |- processing/
 |  |- 04-cohesion-social.Rmd
 |  |- README-prod.md
 |  |- og_proc.R
 |  |- prod_analysis.Rmd
 |  |- prod_analysis.html
 |  |- prod_prep.Rmd
 |  |- prod_prep.html
 |- script visualizacion
 |- scripts/
 |  |- exportar_graficos_chunks.R
 |  |- update-working-tree.sh
```
<!-- WORKING_TREE_END -->

Este working tree incorpora las carpetas y archivos principales relevantes del repo (omite algunas) y se actualiza automáticamente al hacer commit mediante una github action que se encuentra definida en el archivo `.github/workflows/update-working-tree.yml`. El propósito de esta acción es mantener un registro actualizado de la estructura del proyecto, lo que facilita la navegación y organización de los archivos para los estudiantes.


