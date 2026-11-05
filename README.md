# Global Dengue Dynamics & Suitability

Este repositorio contiene los análisis a escala global sobre la dinámica de transmisión del dengue, evaluando el índice de idoneidad climática (*Index P*), las tendencias temporales epidemiológicas de casos y su relación con indicadores socioeconómicos y demográficos por país.

## 📌 Estructura del Repositorio

El proyecto se encuentra organizado en las siguientes carpetas funcionales:

* **`01_factor_p/`**: Adaptación y ejecución del marco conceptual *Index P* para estimar mapas y series temporales mensuales de idoneidad climática de transmisión por *Aedes aegypti*.
* **`02_trend_analysis/`**: Estimación no paramétrica de tendencias temporales (Pendientes de Theil-Sen) para el *Index P* a nivel de país.
* **`03_dengue_cases_trend/`**: Procesamiento de datos epidemiológicos globales y cálculo de tendencias de incidencia de dengue por país.
* **`04_socioeconomic_indices/`**: Integración y procesamiento de indicadores socioeconómicos globales (PBI, Índice Gini, IDH, Alfabetización y Urbanización).
* **`05_global_mapping_and_analysis/`**: Integración de datos vectoriales SIG (`sf`), mapeo espacial coroplético global y gráficos de dispersión/loess intercontinentales.
* **`06_statistical_models_and_anova/`**: Modelado lineal multivariado (selección *stepwise* basada en AIC), análisis de coeficientes y comparaciones múltiples intercontinentales mediante ANOVA y prueba de Tukey HSD.

## ℹ️ Fuentes de Datos y Atribución

* **Index P (Idoneidad climática y scripts base):** 
  El cálculo del índice P y las funciones de manejo de datos se basan en la metodología y scripts desarrollados por **Taishi Nakase y colaboradores**. 
  * *Artículo de referencia:* Nakase, T., Giovanetti, M., Obolski, U. & Lourenço, J. (2023). Global transmission suitability maps for dengue virus transmitted by Aedes aegypti from 1981 to 2019. *Scientific Data*, 10, 275. [DOI: 10.1038/s41597-023-02170-7](https://doi.org/10.1038/s41597-023-02170-7)
  * *Repositorio de GitHub original:* [TaishiNakase/Index-P-estimation-and-applications](https://github.com/TaishiNakase/Index-P-estimation-and-applications)
* **Datos Epidemiológicos:** Registros históricos de casos de dengue obtenidos de la plataforma pública de acceso abierto **Open Dengue** (ventana temporal 1979–2023).

## 🛠️ Requisitos e Instalación

* **Lenguaje:** R (versión >= 4.2.0)
* **Entorno:** RStudio / R Projects
* **Paquetes clave recomendados:** `sf`, `terra`, `tidyverse`, `mblm`, `trend`, `ggrepel`, `patchwork`, `gt`

## 👤 Autora
* **Mariana Baricco**
