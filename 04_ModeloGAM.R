# ============================================================================
# Modelos GAM
# ============================================================================

library(ggplot2)
library(dplyr)
library(mgcv)

# ============================================================================
# 1. Cargar y preparar los datos
# ============================================================================

datos_limpios <- readRDS("03_results/db_limpios/datos_limpios.rds") |>
  mutate(Bosque = recode(Bosque, "Plantacion" = "Plantación"))

datos_azt <- datos_limpios |> 
  filter(Zona == "Aztaparreta")
datos_liz <- datos_limpios |> 
  filter(Zona == "Lizardoia")
datos_tjn <- datos_limpios |> 
  filter(Zona == "Tejera Negra")

# ============================================================================
# 2. Paleta de colores y tema (igual que en el resto del TFM)
# ============================================================================

colores_bosques <- c(
  "Primario" = "#1A5320",
  "Secundario" = "#7CB342",
  "Plantación" = "#D87800"
)

tema_tfm_2 <- theme_minimal(base_size = 16) +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16))

# ============================================================================
# Ajustar los diferentes modelos (2 por Zona, NDVI y NDMI)
# ============================================================================

# NDVI - Aztaparreta
modelo_ndvi_azt <- gam(
  NDVI ~ s(Year, by = Bosque, k = 10) + Bosque,
  data = datos_azt
)

summary(modelo_ndvi_azt)

# NDMI - Aztaparreta
modelo_ndmi_azt <- gam(
  NDMI ~ s(Year, by = Bosque, k = 10) + Bosque,
  data = datos_azt
)

summary(modelo_ndmi_azt)

# NDVI - Lizardoia
modelo_ndvi_liz <- gam(
  NDVI ~ s(Year, by = Bosque, k = 12) + Bosque,
  data = datos_liz
)

summary(modelo_ndvi_liz)

# NDMI - Lizardoia
modelo_ndmi_liz <- gam(
  NDMI ~ s(Year, by = Bosque, k = 12) + Bosque,
  data = datos_liz
)

summary(modelo_ndmi_liz)

# NDVI - Tejera Negra
modelo_ndvi_tjn <- gam(
  NDVI ~ s(Year, by = Bosque, k = 10) + Bosque,
  data = datos_tjn
)

summary(modelo_ndvi_tjn)

# NDMI - Tejera Negra
modelo_ndmi_tjn <- gam(
  NDMI ~ s(Year, by = Bosque, k = 10) + Bosque,
  data = datos_tjn
)

summary(modelo_ndmi_tjn)

# Comprobar el modelo GAM
gam.check(modelo_ndvi_azt)
gam.check(modelo_ndmi_azt)
gam.check(modelo_ndvi_liz)
gam.check(modelo_ndmi_liz)
gam.check(modelo_ndvi_tjn)
gam.check(modelo_ndmi_tjn)

# ============================================================================
# Crear una tabla de años a predecir
# ============================================================================
nuevos_datos_azt <- expand.grid(
  Year = seq(2013, 2025, by = 0.1),
  Bosque = c("Primario", "Secundario", "Plantación")
)

nuevos_datos_liz <- expand.grid(
  Year = seq(2013, 2025, by = 0.1),
  Bosque = c("Primario", "Secundario", "Plantación")
)

nuevos_datos_tjn <- expand.grid(
  Year = seq(2013, 2025, by = 0.1),
  Bosque = c("Primario", "Secundario", "Plantación")
)

# ============================================================================
# Predecir con el modelo
# ============================================================================

# Aztaparreta
# ----------------------------------------------------------------------------
pred_ndvi_azt <- predict(modelo_ndvi_azt, newdata = nuevos_datos_azt, se.fit = TRUE)
nuevos_datos_azt$NDVI_pred <- pred_ndvi_azt$fit
nuevos_datos_azt$NDVI_se <- pred_ndvi_azt$se.fit

pred_ndmi_azt <- predict(modelo_ndmi_azt, newdata = nuevos_datos_azt, se.fit = TRUE)
nuevos_datos_azt$NDMI_pred <- pred_ndmi_azt$fit
nuevos_datos_azt$NDMI_se <- pred_ndmi_azt$se.fit

# Lizardoia
# ----------------------------------------------------------------------------
pred_ndvi_liz <- predict(modelo_ndvi_liz, newdata = nuevos_datos_liz, se.fit = TRUE)
nuevos_datos_liz$NDVI_pred <- pred_ndvi_liz$fit
nuevos_datos_liz$NDVI_se <- pred_ndvi_liz$se.fit

pred_ndmi_liz <- predict(modelo_ndmi_liz, newdata = nuevos_datos_liz, se.fit = TRUE)
nuevos_datos_liz$NDMI_pred <- pred_ndmi_liz$fit
nuevos_datos_liz$NDMI_se <- pred_ndmi_liz$se.fit

# Tejera Negra
# ----------------------------------------------------------------------------
pred_ndvi_tjn <- predict(modelo_ndvi_tjn, newdata = nuevos_datos_tjn, se.fit = TRUE)
nuevos_datos_tjn$NDVI_pred <- pred_ndvi_tjn$fit
nuevos_datos_tjn$NDVI_se <- pred_ndvi_tjn$se.fit

pred_ndmi_tjn <- predict(modelo_ndmi_tjn, newdata = nuevos_datos_tjn, se.fit = TRUE)
nuevos_datos_tjn$NDMI_pred <- pred_ndmi_tjn$fit
nuevos_datos_tjn$NDMI_se <- pred_ndmi_tjn$se.fit

# Añadir la columna ZONA y unir las tres tablas
# ----------------------------------------------------------------------------
nuevos_datos_azt$Zona <- "Aztaparreta"
nuevos_datos_liz$Zona <- "Lizardoia"
nuevos_datos_tjn$Zona <- "Tejera Negra"

predicciones_gam <- bind_rows(nuevos_datos_azt, nuevos_datos_liz, nuevos_datos_tjn)

# Comprobar nuevos datos
head(predicciones_gam)
nrow(predicciones_gam)

# Ordenar el factor Bosque
predicciones_gam <- predicciones_gam |>
  mutate(Bosque = factor(Bosque, levels = c("Primario", "Secundario", "Plantación")))

# ============================================================================
# Generar las gráficas: NDVI y NDMI
# ============================================================================

# NDVI
# ----------------------------------------------------------------------------
grafico_ndvi_gam <- ggplot(predicciones_gam, aes(x = Year, y = NDVI_pred, color = Bosque, group = Bosque)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_ribbon(
    aes(ymin = NDVI_pred - 1.96 * NDVI_se,
        ymax = NDVI_pred + 1.96 * NDVI_se,
        fill = Bosque),
    color = NA,
    alpha = 0.15
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Zona, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colores_bosques) +
  scale_fill_manual(values = colores_bosques) +
  guides(fill = "none") +
  scale_x_continuous(breaks = 2013:2025) +
  labs(x = "Año",
       y = "NDVI",
       color = "Tipo de bosque") +
  tema_tfm_2

print(grafico_ndvi_gam)

# NDMI
# ----------------------------------------------------------------------------
grafico_ndmi_gam <- ggplot(predicciones_gam, aes(x = Year, y = NDMI_pred, color = Bosque, group = Bosque)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_ribbon(
    aes(ymin = NDMI_pred - 1.96 * NDMI_se,
        ymax = NDMI_pred + 1.96 * NDMI_se,
        fill = Bosque),
    color = NA,
    alpha = 0.15
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Zona, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colores_bosques) +
  scale_fill_manual(values = colores_bosques) +
  guides(fill = "none") +
  scale_x_continuous(breaks = 2013:2025) +
  labs(x = "Año",
       y = "NDMI",
       color = "Tipo de bosque") +
  tema_tfm_2

print(grafico_ndmi_gam)

# ============================================================================
# Guardar con ggsave
# ============================================================================
ggsave(
  filename = "04_outputs/g_modelos_GAM/gam_ndvi.png",
  plot = grafico_ndvi_gam,
  width = 14,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_modelos_GAM/gam_ndmi.png",
  plot = grafico_ndmi_gam,
  width = 14,
  height = 9,
  dpi = 300
)