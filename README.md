# Template reporte reproducibilidad

Link al reporte [**AQUÍ**](https://data-soc.github.io/template-repro/)

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
 |  |- update-working-tree.sh
```
<!-- WORKING_TREE_END -->

Este working tree incorpora las carpetas y archivos principales relevantes del repo (omite algunas) y se actualiza automáticamente al hacer commit mediante una github action que se encuentra definida en el archivo `.github/workflows/update-working-tree.yml`. El propósito de esta acción es mantener un registro actualizado de la estructura del proyecto, lo que facilita la navegación y organización de los archivos para los estudiantes.


