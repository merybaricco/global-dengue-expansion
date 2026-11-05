# ==============================================================================
# Proyecto: Global Dengue Dynamics & Suitability
# Análisis: Selección de Modelos (Stepwise AIC), Coeficientes y ANOVA Intercontinental
# Descripción: Modelado lineal múltiple de la incidencia de dengue (media y pendiente)
#              en función de variables climáticas y socioeconómicas escaladas. 
#              Selección stepwise (AIC), extracción de coeficientes y comparaciones 
#              múltiples intercontinentales (ANOVA + Tukey HSD con tablas gt).
# ==============================================================================

# 1. Carga de paquetes necesarios ----------------------------------------------
library(tidyverse)
library(gt)

# Nota: Se asume que 'tabla_pendientes' y 'tabla_filtrada' se encuentran 
# en el entorno de trabajo previa ejecución de este script.

# ==============================================================================
# 2. ESTANDARIZACIÓN / ESCALADO DE VARIABLES
# ==============================================================================

tabla_pendientes <- tabla_pendientes %>%
  mutate(
    casos_escalado   = scale(pend_casos_estand),
    alfabet_escalado = scale(media_alfabet),
    urban_escalado   = scale(media_urban),
    p_escalado       = scale(pendiente_p),
    gini_escalado    = scale(media_gini),
    pbi_escalado     = scale(media_pbi),
    idh_escalado     = scale(if ("media_idh.y" %in% names(.)) media_idh.y else media_idh)
  )

# ==============================================================================
# 3. SELECCIÓN MULTIVARIADA DE MODELOS (STEPWISE AIC)
# ==============================================================================

# A. Modelo completo inicial con pendientes e índices
lm3 <- lm(
  pendiente_incidencias ~ urban_escalado + p_escalado + gini_escalado +
    alfabet_escalado + pbi_escalado + idh_escalado +
    pendiente_alfabet + pendiente_gini + pendiente_urban +
    pendiente_pbi + pendiente_p + pendiente_idh,
  data = tabla_pendientes
)

# Selección stepwise basada en AIC
step_lm3 <- step(object = lm3, direction = "both", trace = 1)

# Modelo reducido seleccionado para Pendiente de Incidencia (Modelo 3)
modelo3 <- lm(
  pendiente_incidencias ~ p_escalado + gini_escalado + pbi_escalado + pendiente_urban,
  data = tabla_pendientes
)
summary(modelo3)

# B. Modelo multivariado para Incidencia Media (Modelo 4)
lm4 <- lm(
  media_incidencias ~ urban_escalado + p_escalado + gini_escalado +
    alfabet_escalado + pbi_escalado + idh_escalado +
    pendiente_alfabet + pendiente_gini + pendiente_urban +
    pendiente_pbi + pendiente_p + pendiente_idh,
  data = tabla_pendientes
)

step_lm4 <- step(object = lm4, direction = "both", trace = 1)

modelo4 <- lm(
  media_incidencias ~ gini_escalado + pendiente_urban + pendiente_pbi + p_escalado,
  data = tabla_pendientes
)
summary(modelo4)


# ==============================================================================
# 4. VISUALIZACIÓN DE COEFICIENTES DEL MODELO
# ==============================================================================

# Extraer coeficientes del modelo 3
coeficientes <- summary(modelo3)$coefficients

tabla_coeficientes_simple <- data.frame(
  Variable    = rownames(coeficientes),
  Coeficiente = coeficientes[, "Estimate"]
) %>%
  filter(Variable != "(Intercept)")

# Gráfico de barras horizontal de coeficientes
p_coef <- ggplot(tabla_coeficientes_simple, aes(x = reorder(Variable, Coeficiente), y = Coeficiente)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Model Coefficients (Dengue Incidence Trend)",
    x = "Variable",
    y = "Coefficient Estimate"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank())

print(p_coef)


# ==============================================================================
# 5. ANÁLISIS DE VARIANZA (ANOVA) Y COMPARACIONES POST-HOC POR CONTINENTE
# ==============================================================================

# A. ANOVA para Pendiente de Incidencia
modelo_pendiente <- aov(pendiente_incidencias ~ continente, data = tabla_filtrada)
summary(modelo_pendiente)
tukey_pend_raw <- TukeyHSD(modelo_pendiente)

ggplot(tabla_filtrada, aes(x = continente, y = pendiente_incidencias)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Pendiente de incidencia de dengue por continente", x = "Continente", y = "Pendiente")

# B. ANOVA para Media de Incidencia
modelo_media <- aov(media_incidencias ~ continente, data = tabla_filtrada)
summary(modelo_media)
tukey_media_raw <- TukeyHSD(modelo_media)

ggplot(tabla_filtrada, aes(x = continente, y = media_incidencias)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  theme_minimal() +
  labs(title = "Media de incidencia de dengue por continente", x = "Continente", y = "Media")


# ==============================================================================
# 6. TABLA FORMATO EJECUTIVO / CIENTÍFICO CON GT
# ==============================================================================

tukey_pend <- as.data.frame(tukey_pend_raw$continente)
tukey_pend$comparacion <- rownames(tukey_pend)
tukey_pend$variable <- "Pendiente incidencias"

tukey_media <- as.data.frame(tukey_media_raw$continente)
tukey_media$comparacion <- rownames(tukey_media)
tukey_media$variable <- "Media incidencias"

tabla_resumen <- bind_rows(tukey_pend, tukey_media) %>%
  rename(Diff = diff, Lwr = lwr, Upr = upr, `p adj` = `p adj`) %>%
  mutate(Signif = case_when(
    `p adj` < 0.01 ~ "**",
    `p adj` < 0.05 ~ "*",
    `p adj` < 0.1  ~ "(.)",
    TRUE           ~ "ns"
  )) %>%
  select(variable, comparacion, Diff, Lwr, Upr, `p adj`, Signif)

# Generar tabla con gt
tabla_gt <- tabla_resumen %>%
  gt(rowname_col = "comparacion", groupname_col = "variable") %>%
  tab_header(
    title = "Comparaciones múltiples (Tukey HSD)",
    subtitle = "Pendiente y media de incidencias según continente"
  ) %>%
  fmt_number(columns = c(Diff, Lwr, Upr, `p adj`), decimals = 6)

print(tabla_gt)
