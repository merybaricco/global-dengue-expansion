# ==============================================================================
# Proyecto: Global Dengue Dynamics & Case Trends (Open Dengue)
# Análisis: Tendencias temporales de casos de dengue por país (1979-2023)
# Descripción: Procesamiento de datos de casos de dengue a escala global, 
#              transformación a formato largo y cálculo de la pendiente de 
#              Theil-Sen por país mediante el paquete 'trend'.
# 
# NOTA SOBRE LOS DATOS: 
#   Los datos de casos de dengue utilizados provienen de la plataforma de 
#   acceso abierto Open Dengue (ventana temporal 1979-2023). Estos datos 
#   corresponden a registros públicos disponibles en dicha plataforma y no a 
#   mediciones primarias de autoría propia.
# ==============================================================================

# 1. Carga de paquetes necesarios ----------------------------------------------
library(trend)    # Para la pendiente de Theil-Sen (sens.slope)
library(ggplot2)  # Para la visualización de gráficos
library(dplyr)    # Para manipulación y filtrado de datos
library(tidyr)    # Para transformar datos (pivot_longer)

# 2. Carga y preparación de datos ----------------------------------------------
# Nota: Ajusta la ruta de lectura según tu directorio de trabajo o proyecto
datos <- read.csv("C:/Users/meryb/Documents/doctorado/factor p/analisis/tasa_casos.csv", 
                  sep = ";", dec = ",") 

# Limpiar nombres de columnas (remover la "X" inicial de los años si la hubiera)
colnames(datos) <- gsub("^X", "", colnames(datos))  

# Convertir columnas de valores (de la segunda en adelante) a numéricas
datos[, 2:ncol(datos)] <- lapply(datos[, 2:ncol(datos)], as.numeric)

# 3. Transformación a formato largo (tidy data) --------------------------------
datos_largos <- datos %>%
  pivot_longer(cols = -pais, names_to = "Año", values_to = "Casos") %>%
  mutate(Año = as.numeric(Año))

# 4. Cálculo de la pendiente de Theil-Sen por país -----------------------------
pend_casos <- datos_largos %>%
  group_by(pais) %>%
  filter(!is.na(Casos)) %>%
  filter(n() >= 2) %>%  # Filtrar países con al menos 2 observaciones
  summarise(pendiente_casos = sens.slope(as.numeric(Casos), as.numeric(Año))$estimates) %>%
  ungroup()

print(pend_casos)

# 5. Visualización: Gráfico de barras de las tendencias de casos ---------------
p_casos <- ggplot(pend_casos, aes(x = reorder(pais, pendiente_casos), y = pendiente_casos)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Pendiente Theil-Sen de casos de Dengue por país (Open Dengue)",
    x = "País",
    y = "Pendiente de casos"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank()
  )

print(p_casos)

# Opcional: Guardar el gráfico generado
# ggsave("tendencia_theil_sen_casos_dengue.png", p_casos, width = 8, height = 10, dpi = 300)
