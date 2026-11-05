# ==============================================================================
# Proyecto: Global Dengue Transmission Suitability (Index P)
# Análisis: Cálculo de tendencias temporales (Pendiente de Theil-Sen) por país
# Descripción: Procesamiento de datos tabulares de Index P a escala global, 
#              transformación a formato largo, ajuste del estimador no paramétrico 
#              de Theil-Sen por país y generación de gráfico de barras ordenado.
# ==============================================================================

# 1. Carga de paquetes necesarios ----------------------------------------------
library(tidyverse)
library(mblm)
library(trend)

# 2. Carga y preparación de datos ----------------------------------------------
# Nota: Ajusta la ruta de lectura según tu directorio de trabajo o proyecto
valor_p <- read.csv("C:/Users/meryb/Documents/doctorado/factor p/analisis/valor_p.csv", 
                    sep = ";", dec = ",")

# Convertir columnas de valores (2 a 45) a numéricas
valor_p[, 2:45] <- apply(valor_p[, 2:45], 2, as.numeric)

# Limpiar nombres de columnas (remover la "X" inicial de los años si la hubiera)
colnames(valor_p) <- gsub("^X", "", colnames(valor_p))  

# 3. Transformación a formato largo (tidy data) --------------------------------
datos_largos <- valor_p %>%
  pivot_longer(cols = -pais, names_to = "Año", values_to = "P") %>%
  mutate(Año = as.numeric(Año))  # Convertir "Año" a número

# Filtrar países con suficientes datos para calcular tendencia (> 1 valor)
datos_filtrados <- datos_largos %>%
  group_by(pais) %>%
  filter(sum(!is.na(P)) > 1)  

# 4. Cálculo de la pendiente de Theil-Sen por país -----------------------------
pend_p <- datos_filtrados %>%
  group_by(pais) %>%
  summarise(
    pendiente_p = ifelse(n() > 1, coef(mblm(P ~ Año, data = cur_data()))[2], NA)
  )

print(pend_p)

# 5. Visualización: Gráfico de barras con pendiente de Theil-Sen ---------------
p_tendencia <- ggplot(pend_p, aes(x = reorder(pais, pendiente_p), y = pendiente_p, fill = pendiente_p)) +
  geom_bar(stat = "identity") +  
  coord_flip() +  # Girar para mejor visualización de los países
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Pendiente de Theil-Sen - Valor P por País", 
    x = "País", 
    y = "Pendiente (Tendencia temporal)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

print(p_tendencia)

# Opcional: Guardar el gráfico generado
# ggsave("tendencia_theil_sen_paises.png", p_tendencia, width = 8, height = 10, dpi = 300)
