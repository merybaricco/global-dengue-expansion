# ==============================================================================
# Proyecto: Global Dengue Dynamics & Suitability
# Análisis: Mapeo espacial global y análisis de relaciones bivariadas (Modelos y Patrones)
# Descripción: Integración de tablas de tendencias con capas vectoriales mundiales (sf),
#              generación de mapas coropléticos, diagramas de caja por continente y 
#              paneles de gráficos de dispersión con ajustes lineales (lm) y loess 
#              combinados mediante patchwork.
# ==============================================================================

# 1. Carga de paquetes necesarios ----------------------------------------------
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(patchwork)

# 2. Carga y unión de datos espaciales ------------------------------------------
# Nota: Ajusta la ruta del archivo shapefile según tu directorio de trabajo
mundo <- st_read("C:/Users/meryb/Documents/doctorado/factor p/qgis/paises_p.shp")

# Se asume que 'tabla_pendientes' ya está cargada en el entorno de R
mundo_join <- mundo %>%
  left_join(tabla_pendientes, by = "pais")


# ==============================================================================
# 3. MAPAS COROPLÉTICOS MUNDIALES
# ==============================================================================

# Mapa base del mundo
p_base <- ggplot(data = mundo) +
  geom_sf(color = "black", size = 0.1) +
  theme_minimal()
print(p_base)

# Urbanización media mundial
p_urban <- ggplot(data = mundo_join) +
  geom_sf(aes(fill = media_urban), color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  theme_minimal() +
  labs(fill = "Media urban", title = "Urbanización media mundial")
print(p_urban)

# Media de incidencias
p_med_inc <- ggplot(data = mundo_join) +
  geom_sf(aes(fill = media_incidencias), color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  theme_minimal() +
  labs(fill = "Media incidencias")
print(p_med_inc)

# Pendiente de incidencias (escala normal)
p_pend_inc <- ggplot(data = mundo_join) +
  geom_sf(aes(fill = pendiente_incidencias), color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  theme_minimal() +
  labs(fill = "Pendiente de incidencias")
print(p_pend_inc)

# Pendiente de incidencias (escala logarítmica)
p_pend_inc_log <- ggplot(data = mundo_join) +
  geom_sf(aes(fill = pendiente_incidencias), color = "black") +
  scale_fill_viridis_c(option = "plasma", trans = "log", na.value = "grey80") +
  theme_minimal() +
  labs(fill = "Dengue Incidence Trend")
print(p_pend_inc_log)

# Media del Índice Gini mundial
p_gini <- ggplot(data = mundo_join) +
  geom_sf(aes(fill = media_gini), color = "black") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  theme_minimal() +
  labs(fill = "Media gini", title = "Media de Índice Gini mundial")
print(p_gini)


# ==============================================================================
# 4. ANÁLISIS GRÁFICO Y ESTADÍSTICO POR CONTINENTE
# ==============================================================================

# Filtrar continentes de interés
tabla_filtrada <- tabla_pendientes %>%
  filter(!continente %in% c("africa", "america del norte"))

# Boxplot de tendencias de incidencia por continente
p_box <- ggplot(tabla_filtrada, aes(x = continente, y = pendiente_incidencias, fill = continente)) +
  geom_boxplot() +
  labs(x = "Continent", y = "Dengue Incidence Trend", title = "") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank()
  )
print(p_box)

# Relaciones bivariadas con modelos lineales por continente
p_rel_p <- ggplot(tabla_filtrada, aes(x = p_escalado, y = pendiente_incidencias, color = continente)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = pais), size = 3, hjust = 0.5, vjust = 0.5, check_overlap = TRUE) +  
  labs(title = "Relación entre índice P e incidencia de dengue", x = "Índice P", y = "Pendiente de incidencia") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_rel_p)

p_rel_gini <- ggplot(tabla_filtrada, aes(x = gini_escalado, y = pendiente_incidencias, color = continente)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = pais), size = 3, hjust = 0.5, vjust = 0.5, check_overlap = TRUE) +  
  labs(title = "Relación entre índice Gini e incidencia de dengue", x = "Índice Gini", y = "Pendiente de incidencia") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_rel_gini)

p_rel_pbi <- ggplot(tabla_filtrada, aes(x = pendiente_pbi, y = pendiente_incidencias, color = continente)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = pais), size = 3, hjust = 0.5, vjust = 0.5, check_overlap = TRUE) +  
  labs(title = "Relación entre Pendiente PBI e incidencia de dengue", x = "Pendiente PBI", y = "Pendiente de incidencia") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_rel_pbi)

p_rel_urban <- ggplot(tabla_filtrada, aes(x = pendiente_urban, y = pendiente_incidencias, color = continente)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_text(aes(label = pais), size = 3, hjust = 0.5, vjust = 0.5, check_overlap = TRUE) +  
  labs(title = "Relación entre Pendiente Urbanización e incidencia de dengue", x = "Pendiente Urbanización", y = "Pendiente de incidencia") +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p_rel_urban)


# ==============================================================================
# 5. PANEL MULTIPANEL COMBINADO (PATCHWORK 2x2)
# ==============================================================================

g1 <- ggplot(tabla_pendientes, aes(x = p_escalado, y = pendiente_incidencias)) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = pais), size = 4, max.overlaps = 100) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold") +
  labs(x = "P Index Trend", y = "Dengue Incidence Trend") +
  theme_minimal() +
  theme(panel.grid = element_blank())

g2 <- ggplot(tabla_pendientes, aes(x = urban_escalado, y = pendiente_incidencias)) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = pais), size = 4, max.overlaps = 100) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold") +
  labs(x = "Urban Trend", y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank())

g3 <- ggplot(tabla_pendientes, aes(x = gini_escalado, y = pendiente_incidencias)) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = pais), size = 4, max.overlaps = 100) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("text", x = -Inf, y = Inf, label = "C", hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold") +
  labs(x = "Gini Index", y = "Dengue Incidence Trend") +
  theme_minimal() +
  theme(panel.grid = element_blank())

g4 <- ggplot(tabla_pendientes, aes(x = pbi_escalado, y = pendiente_incidencias)) +
  geom_point(alpha = 0.3) +
  geom_text_repel(aes(label = pais), size = 4, max.overlaps = 100) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("text", x = -Inf, y = Inf, label = "D", hjust = -0.5, vjust = 1.5, size = 6, fontface = "bold") +
  labs(x = "GDP Trend", y = "") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Combinar en una cuadrícula 2x2 final
panel_final <- (g1 | g2) / (g3 | g4)
print(panel_final)
