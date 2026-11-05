# ==============================================================================
# Proyecto: Global Dengue Transmission Suitability (Index P)
# Descripción: Visualización de mapas de idoneidad de transmisión típica anual 
#              y series temporales mensuales (1981-2019) a escala global.
# Atribución / Créditos: 
#   Basado en los scripts de Taishi Nakase et al. 
#   Repositorio original: https://github.com/TaishiNakase/Index-P-estimation-and-applications
#   Artículo de referencia: Nakase, T., Giovanetti, M., Obolski, U. & Lourenço, J. (2023). 
#   Global transmission suitability maps for dengue virus transmitted by Aedes aegypti 
#   from 1981 to 2019. Scientific Data, 10, 275.
# ==============================================================================

# 1. Carga de paquetes necesarios ----------------------------------------------
library(raster)
library(tidyverse)
library(RColorBrewer)

# 2. Definir ruta al directorio local de los mapas de Index P ------------------
# Ajusta esta ruta según la ubicación de tus carpetas de datos globales
indexP_maps_path <- "C:/Users/meryb/Documents/doctorado/factor p/analisis/mapas_paises"


# ==============================================================================
# 3. FUNCIÓN: Mapas del año típico (12 meses)
# ==============================================================================

plot_typical_year_maps <- function(country) {
  typical_year_map_ext <- "_indexP_typical_year_mean_rasters.tif"
  file_name <- paste0(country, typical_year_map_ext)
  file_path <- file.path(indexP_maps_path, country, file_name)
  
  # Cargar el raster y convertirlo a dataframe para ggplot
  indexP_typical_year_mean_rasters <- raster::brick(file_path) %>% 
    rasterToPoints() %>% 
    as.data.frame()
  
  # Verificar columnas de coordenadas
  if (!("x" %in% names(indexP_typical_year_mean_rasters) & "y" %in% names(indexP_typical_year_mean_rasters))) {
    stop("No se encontraron columnas 'x' e 'y'. Verifica los nombres de las columnas.")
  }
  
  indexP_typical_year_mean_rasters <- indexP_typical_year_mean_rasters %>%
    mutate(x = as.numeric(x), y = as.numeric(y))
  
  layer_names <- names(indexP_typical_year_mean_rasters)[-(1:2)]
  
  if (length(layer_names) >= 12) {
    mth_mapping <- month.abb
    names(mth_mapping) <- layer_names[1:12]
  } else {
    stop("El raster no contiene las 12 capas esperadas.")
  }
  
  # Transformar datos a formato largo
  indexP_typical_year_mean_rasters <- indexP_typical_year_mean_rasters %>%
    gather(key = "layer", value = "indexP", all_of(layer_names[1:12])) %>% 
    mutate(layer = factor(mth_mapping[layer], levels = month.abb))

  # Cargar mapa del país/región
  world_map <- map_data("world", region = country)

  # Escala de colores personalizada (RdYlBu invertida)
  col_scale <- c(colorRampPalette(rev(brewer.pal(11, "RdYlBu"))[1:5])(5), 
                 colorRampPalette(rev(brewer.pal(11, "RdYlBu"))[6:11])(15), 
                 rep(rev(brewer.pal(11, "RdYlBu"))[11], 5))

  # Generar gráfico de paneles mensuales
  g <- ggplot() + 
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
                 fill = "lightgrey", color = "black", size = 1) + 
    geom_tile(data = indexP_typical_year_mean_rasters, 
              aes(x = x, y = y, fill = indexP)) + 
    scale_fill_gradientn(colours = rep(col_scale, each=3), 
                         na.value = "white", 
                         guide = guide_colorbar(frame.colour = "black", ticks.colour = "black"), 
                         limits = c(0, 4), breaks = seq(0, 4, 1), name = "Index P") + 
    theme_minimal() + 
    theme(axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          strip.text = element_text(size = 9, family = "sans"), 
          legend.title = element_text(size = 9, family = "sans"), 
          legend.text = element_text(size = 9, family = "sans")) + 
    facet_wrap(~layer, nrow = 2)
  
  return(g)
}

# Ejemplo de ejecución para Argentina
# plot_typical_year_maps(country = "Argentina")


# ==============================================================================
# 4. FUNCIÓN: Series temporales mensuales (1981-2019)
# ==============================================================================

plot_pixel_time_series <- function(country, fechas) {
  monthly_time_series_ext <- "_indexP_monthly_mean_rasters.tif"
  file_name <- paste0(country, monthly_time_series_ext)
  file_path <- file.path(indexP_maps_path, country, file_name)
  monthly_mean_rasters <- raster::brick(file_path)
  
  mediacapa <- function(x) { mean(as.vector(x), na.rm = TRUE) }
  time_series_values <- apply(as.array(monthly_mean_rasters), 3, mediacapa)
  
  time_series <- data.frame(date = fechas, indexP = time_series_values)
  
  g <- ggplot(data = time_series) + 
    geom_hline(yintercept = 1.7, color = "black", linetype = "dashed") + 
    geom_line(mapping = aes(x = date, y = indexP)) + 
    scale_x_date(breaks = seq(as.Date("1980/1/1"), as.Date("2020/1/1"), "5 years"), 
                 date_labels = "'%y", limits = as.Date(c("1980-01-01", "2020-03-01")), 
                 expand = c(0.01, 0.07)) + 
    labs(y = "Mean index P") + 
    theme_minimal() + 
    theme(axis.title.x = element_blank(), 
          axis.line = element_line(),
          axis.text = element_text(size = 8, family = "sans", color = "black"),
          panel.border = element_blank(), 
          panel.grid = element_blank(),
          axis.title = element_text(size = 9, family = "sans", color = "black"), 
          axis.ticks = element_line(color = "black"), 
          panel.spacing.y = unit(-0.2, "cm"))
  
  return(g)
}
