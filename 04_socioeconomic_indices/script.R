# ==============================================================================
# Proyecto: Global Socioeconomic Indices & Trends
# Análisis: Procesamiento de indicadores socioeconómicos (Alfabetización, PBI, 
#           Urbanización, Gini e IDH) a escala global.
# Descripción: Carga de series temporales por país, cálculo de valores medios 
#              históricos, estimación de tendencias temporales mediante la 
#              pendiente de Theil-Sen, y consolidación en una tabla maestra.
# ==============================================================================

# 1. Carga de paquetes necesarios ----------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(trend)

# 2. Carga y preparación de bases de datos de índices --------------------------
# Nota: Ajusta las rutas de lectura según tu directorio de trabajo o proyecto

alfabet <- read.csv("C:/Users/meryb/Documents/doctorado/factor p/analisis/alfabetizacion.csv", sep = ";", dec = ",") 
colnames(alfabet) <- gsub("^X", "", colnames(alfabet)) 
alfabet[, 2:ncol(alfabet)] <- lapply(alfabet[, 2:ncol(alfabet)], as.numeric)

pbi <- read.csv("C:/Users/meryb/Documents/doctorado/factor p/analisis/pbi.csv", sep = ";", dec = ",") 
colnames(pbi) <- gsub("^X", "", colnames(pbi))
pbi[, 2:ncol(pbi)] <- lapply(pbi[, 2:ncol(pbi)], as.numeric)

urban <- read.csv("C:/Users/meryb/Documents/doctorado/factor p/analisis/urbanizacion.csv", sep = ";", dec = ",") 
colnames(urban) <- gsub("^X", "", colnames(urban))
urban[, 2:ncol(urban)] <- lapply(urban[, 2:ncol(urban)], as.numeric)

gini <- read.csv("C:/Users/meryb/Documents/doctorado/factor p/analisis/gini.csv", sep = ";", dec = ",") 
colnames(gini) <- gsub("^X", "", colnames(gini))
gini[, 2:ncol(gini)] <- lapply(gini[, 2:ncol(gini)], as.numeric)

idh <- read.csv("C:/Users/meryb/Documents/doctorado/factor p/analisis/idh.csv", sep = ";", dec = ",") 
colnames(idh) <- gsub("^X", "", colnames(idh))
idh[, 2:ncol(idh)] <- lapply(idh[, 2:ncol(idh)], as.numeric)


# ==============================================================================
# 3. CÁLCULO DE VALORES MEDIOS HISTÓRICOS E INTEGRACIÓN
# ==============================================================================

# Nota: Se asume que 'tabla_pendientes' ya existe en el entorno de trabajo previa a este bloque.

alfabet_m <- alfabet %>%
  rowwise() %>% 
  mutate(media_alfabet = mean(c_across(2:ncol(alfabet)), na.rm = TRUE)) %>% 
  select(pais = 1, media_alfabet) %>% 
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, alfabet_m, by = "pais")


pbi_m <- pbi %>%
  rowwise() %>% 
  mutate(media_pbi = mean(c_across(2:ncol(pbi)), na.rm = TRUE)) %>% 
  select(pais = 1, media_pbi) %>% 
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, pbi_m, by = "pais")


urban_m <- urban %>%
  rowwise() %>% 
  mutate(media_urban = mean(c_across(2:ncol(urban)), na.rm = TRUE)) %>% 
  select(pais = 1, media_urban) %>% 
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, urban_m, by = "pais")


gini_m <- gini %>%
  rowwise() %>% 
  mutate(media_gini = mean(c_across(2:ncol(gini)), na.rm = TRUE)) %>% 
  select(pais = 1, media_gini) %>% 
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, gini_m, by = "pais")


idh_m <- idh %>%
  rowwise() %>% 
  mutate(media_idh = mean(c_across(2:ncol(idh)), na.rm = TRUE)) %>% 
  select(pais = 1, media_idh) %>% 
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, idh_m, by = "pais")


# ==============================================================================
# 4. CÁLCULO DE TENDENCIAS TEMPORALES (PENDIENTE DE THEIL-SEN)
# ==============================================================================

# A. Alfabetización
datos_alfabet <- alfabet %>%
  pivot_longer(cols = -pais, names_to = "Año", values_to = "Alfabetizacion") %>%
  mutate(Año = as.numeric(Año))

pend_alfabet <- datos_alfabet %>%
  group_by(pais) %>%
  filter(!is.na(Alfabetizacion)) %>%
  filter(n() >= 2) %>% 
  summarise(pendiente_alfabet = sens.slope(as.numeric(Alfabetizacion), as.numeric(Año))$estimates) %>%
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, pend_alfabet, by = "pais")

p1 <- ggplot(pend_alfabet, aes(x = reorder(pais, pendiente_alfabet), y = pendiente_alfabet)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Pendiente Theil-Sen de Alfabetización por país", x = "País", y = "Pendiente alfabetización") +
  theme_minimal()
print(p1)


# B. PBI
datos_pbi <- pbi %>%
  pivot_longer(cols = -pais, names_to = "Año", values_to = "Pbi") %>%
  mutate(Año = as.numeric(Año))

pend_pbi <- datos_pbi %>%
  group_by(pais) %>%
  filter(!is.na(Pbi)) %>%
  filter(n() >= 2) %>%  
  summarise(pendiente_pbi = sens.slope(as.numeric(Pbi), as.numeric(Año))$estimates) %>%
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, pend_pbi, by = "pais")

p2 <- ggplot(pend_pbi, aes(x = reorder(pais, pendiente_pbi), y = pendiente_pbi)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Pendiente Theil-Sen de PBI por país", x = "País", y = "Pendiente PBI") +
  theme_minimal()
print(p2)


# C. Urbanización
datos_urban <- urban %>%
  pivot_longer(cols = -pais, names_to = "Año", values_to = "Urban") %>%
  mutate(Año = as.numeric(Año))

pend_urban <- datos_urban %>%
  group_by(pais) %>%
  filter(!is.na(Urban)) %>%
  filter(n() >= 2) %>%  
  summarise(pendiente_urban = sens.slope(as.numeric(Urban), as.numeric(Año))$estimates) %>%
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, pend_urban, by = "pais")

p3 <- ggplot(pend_urban, aes(x = reorder(pais, pendiente_urban), y = pendiente_urban)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Pendiente Theil-Sen de Urbanización por país", x = "País", y = "Pendiente Urbanización") +
  theme_minimal()
print(p3)


# D. Índice Gini
datos_gini <- gini %>%
  pivot_longer(cols = -pais, names_to = "Año", values_to = "Gini") %>%
  mutate(Año = as.numeric(Año))

pend_gini <- datos_gini %>%
  group_by(pais) %>%
  filter(!is.na(Gini)) %>%
  filter(n() >= 2) %>%  
  summarise(pendiente_gini = sens.slope(as.numeric(Gini), as.numeric(Año))$estimates) %>%
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, pend_gini, by = "pais")

p4 <- ggplot(pend_gini, aes(x = reorder(pais, pendiente_gini), y = pendiente_gini)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Pendiente Theil-Sen de Índice Gini por país", x = "País", y = "Pendiente Gini") +
  theme_minimal()
print(p4)


# E. Índice de Desarrollo Humano (IDH)
datos_idh <- idh %>%
  pivot_longer(cols = -pais, names_to = "Año", values_to = "Idh") %>%
  mutate(Año = as.numeric(Año))

pend_idh <- datos_idh %>%
  group_by(pais) %>%
  filter(!is.na(Idh)) %>%
  filter(n() >= 2) %>%  
  summarise(pendiente_idh = sens.slope(as.numeric(Idh), as.numeric(Año))$estimates) %>%
  ungroup()

tabla_pendientes <- merge(tabla_pendientes, pend_idh, by = "pais")

p5 <- ggplot(pend_idh, aes(x = reorder(pais, pendiente_idh), y = pendiente_idh)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Pendiente Theil-Sen de IDH por país", x = "País", y = "Pendiente IDH") +
  theme_minimal()
print(p5)


# ==============================================================================
# 5. LIMPIEZA FINAL Y EXPORTACIÓN DE LA TABLA MAESTRA
# ==============================================================================

# Limpieza de columnas duplicadas o redundantes
tabla_pendientes <- tabla_pendientes %>% 
  select(-any_of("media_idh.x"))

# Renombrar columnas de ser necesario
tabla_pendientes <- tabla_pendientes %>%
  rename(any_of(c(media_gini = "media_gini.x")))

# Guardar la tabla maestra consolidada
write.csv(tabla_pendientes, "tabla_paises.csv", row.names = FALSE)
