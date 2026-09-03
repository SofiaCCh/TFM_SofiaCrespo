# ============================================================================
# Gráficos de tendencias y correlación de los índices
# ============================================================================
# 1. Cargar las librerías necesarias
# ============================================================================

library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)
library(mgcv)
library(ggpubr)
library(GGally)

# Paleta de colores para cada tipo de bosque (igual para todo el TFM)
colores_bosques <- c(
  "Primario" = "#1A5320",
  "Secundario" = "#7CB342",
  "Plantación" = "#D87800"
)

# Ataparreta = azt
# Lizardoia = liz
# Tejera Negra = tjn

# Generar un tema para los gráficos
tema_tfm_2 <- theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16))

# ============================================================================
# 2. Leer archivo
# ============================================================================

datos_originales <- readRDS("03_results/db_landsat/db_landsat.rds")

variables <- c("TCB", "TCG", "TCW", "NBR", "NDVI", "EVI")

# Bandas de Landsat-7 ETM+
# B1 -> Blue
# B2 -> Green
# B3 -> Red
# B4 -> NIR
# B5 -> SWIR1
# B7 -> SWIR2

# ============================================================================
# 3. Análisis de los datos
# ============================================================================

# Número de datos (pixeles) por zona y tipo de bosque
count(datos_originales, Zona, Bosque)

# Dimensiones y NAs
cat("Filas:", nrow(datos_originales), "\n")
cat("Columnas:", ncol(datos_originales), "\n")

# Visualizar el número total de NAs (valores nulos)
colSums(is.na(datos_originales))

# Nombre de las columnas del dataframe 
print(names(datos_originales))

# Visualización general de los datos
glimpse(datos_originales)
summary(datos_originales)

# Visualizar el número total de NAs (valores nulos)
colSums(is.na(datos_originales))

# Mostrar min y max por zona y bosque
datos_originales |>
  group_by(Zona, Bosque) |>
  summarise(across(all_of(variables),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE))),
            .groups = "drop")

# ============================================================================
# 4. Calcular el índice de humedad (NDMI) y volver a analizar los datos
# ============================================================================

datos <- datos_originales |>
  mutate(
    across(SR_B1:EVI, ~ .x / 10000),          # Reescalar los valores originales
    NDMI = (SR_B4 - SR_B5) / (SR_B4 + SR_B5)  # Calcular NDMI
  )

variables <- c("TCB", "TCG", "TCW", "NBR", "NDVI", "EVI", "NDMI")

# Análisis general de los datos 
summary(datos)

# Contar NAs por variable
datos |>
  summarise(across(all_of(variables), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_NA")

# Estadísticos descriptivos (mín, máx, media, mediana, sd)
estadísticos <- datos |>
  summarise(across(all_of(variables),
                   list(min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE),
                        media = ~mean(.x, na.rm = TRUE),
                        mediana = ~median(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "valor")

# Se encuentran 5250 NAs en la columna del índice NDMI

# ============================================================================
# 4. Obtener tabla final
# ============================================================================

# Comprobar si el NA del NDMI viene de las propias bandas (NA de origen)
# o de una división 0/0 (denominador nulo)
datos |>
  filter(is.na(NDMI)) |>
  summarise(
    NA_en_B4 = sum(is.na(SR_B4)),
    NA_en_B5 = sum(is.na(SR_B5)),
    denominador_cero = sum(SR_B4 == 0 & SR_B5 == 0, na.rm = TRUE)
  )
# -> NA_en_B4 = 0, NA_en_B5 = 0, denominador_cero = 5250
#    Las bandas no traen NA de origen; el problema es que B4 y B5 valen
#    0 a la vez, lo que produce una división 0/0 (NaN) en el NDMI

# --------------------------------------------------------------------------
# Ver si los NA se concentran en alguna zona, tipo de bosque o año concreto
datos |>
  filter(is.na(NDMI)) |>
  count(Zona, Bosque, Year) |>
  arrange(desc(n))
# -> El 92% de los casos caen en 2014, repartidos entre varias zonas y
#    bosques -> apunta a huecos del compuesto de ese año (posible
#    cobertura de nubes), no a un error de procesamiento propio

# --------------------------------------------------------------------------
# Comprobar si el resto de bandas (no solo B4/B5) también están a 0
# en esas mismas filas -> confirmaría que es un píxel sin dato completo
datos |>
  filter(is.na(NDMI)) |>
  summarise(across(c(SR_B1, SR_B2, SR_B3, SR_B7),
                   ~ sum(.x == 0, na.rm = TRUE)))
# -> Las 4 bandas dan también 5250 ceros: son los mismos píxeles en
#    todos los casos, es decir, un píxel "vacío" completo, no un fallo
#    puntual de una sola banda

# --------------------------------------------------------------------------
# Confirmar que el NDVI tiene exactamente los mismos píxeles afectados
# que el NDMI (mismo origen del problema, no una coincidencia numérica)
identical(
  datos |> filter(is.na(NDMI)) |> pull(Pixel_ID, Year),
  datos |> filter(is.na(NDVI_2)) |> pull(Pixel_ID, Year)
)
# -> TRUE: mismo conjunto de píxeles en ambos índices

# Se determina que los NA resultantes en NDMI (y en el resto de índices
# basados en ratios: NDVI, NBR, EVI) proceden de píxeles sin dato real,
# codificados como 0 en todas las bandas de reflectancia en lugar de
# como NA, y no de un error en el cálculo de los índices ni en el
# procesamiento de las bandas. Se filtran de la tabla final.

datos_limpios <- datos |>
  filter(!(SR_B4 == 0 & SR_B5 == 0))

# Comprobar que se ha filtrado correctamente
nrow(datos) - nrow(datos_limpios)

# Guardar los datos limpios en archivo .rds
saveRDS(datos_limpios, "03_results/db_limpios/datos_limpios.rds")

# Leer los datos (para saltarse lo anterior)
datos_limpios <- readRDS("03_results/db_limpios/datos_limpios.rds") |>
  mutate(Bosque = recode(Bosque, "Plantacion" = "Plantación"))

# ============================================================================
# 5. Correlación entre índices
# ============================================================================

indices <- datos_limpios |>
  select(NDVI, EVI)

matriz_correlacion <- cor(indices, use = "complete.obs")

print("--- MATRIZ DE CORRELACIÓN ---")
print(round(matriz_correlacion, 2))

# El NDDI y el NDMI muestran lo mismo por lo que solo cojo 1 índice de humedad
# La correlación entre el NDVI y el EVI es confusa --> PREGUNTAR!!!

# ============================================================================
# 6. Medianas de verano
# ============================================================================

indices_anuales <- datos_limpios |>
  filter(Year >= 2017) |>
  group_by(Zona, Bosque, Year) |>
  summarise(
    # Conteo de píxeles
    n_pixeles = n(),
    
    # Media
    NDVI_media = mean(NDVI, na.rm = TRUE),
    EVI_media  = mean(EVI, na.rm = TRUE),
    NDMI_media = mean(NDMI, na.rm = TRUE),
    
    # Mediana
    NDVI_mediana = median(NDVI, na.rm = TRUE),
    EVI_mediana  = median(EVI, na.rm = TRUE),
    NDMI_mediana = median(NDMI, na.rm = TRUE),
    
    # Percentil 10 (límite inferior de variabilidad)
    NDVI_p10 = quantile(NDVI, 0.10, na.rm = TRUE),
    EVI_p10  = quantile(EVI, 0.10, na.rm = TRUE),
    NDMI_p10 = quantile(NDMI, 0.10, na.rm = TRUE),
    
    # Percentil 90 (límite superior de variabilidad)
    NDVI_p90 = quantile(NDVI, 0.90, na.rm = TRUE),
    EVI_p90  = quantile(EVI, 0.90, na.rm = TRUE),
    NDMI_p90 = quantile(NDMI, 0.90, na.rm = TRUE),
    
    # Desviación estándar SD (variabilidad real entre píxeles)
    NDVI_sd = sd(NDVI, na.rm = TRUE),
    EVI_sd  = sd(EVI, na.rm = TRUE),
    NDMI_sd = sd(NDMI, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  mutate(
    # Error estándar = sd / raíz(n)
    NDVI_se = NDVI_sd / sqrt(n_pixeles),
    EVI_se  = EVI_sd / sqrt(n_pixeles),
    NDMI_se = NDMI_sd / sqrt(n_pixeles),
    
    # CI 95% de la media = 1.96 * SE
    NDVI_ci = 1.96 * NDVI_se,
    EVI_ci  = 1.96 * EVI_se,
    NDMI_ci = 1.96 * NDMI_se,
    
    Bosque = factor(Bosque, levels = c("Primario", "Secundario", "Plantación"))
  )

head(indices_anuales)
cat("Total de filas anuales:", nrow(indices_anuales), "\n")

# ============================================================================
# 7. Visualizar las trayectorias de los índices
# ============================================================================

# NDVI
# ----------------------------------------------------------------------------
grafico_ndvi <- ggplot(indices_anuales, aes(x = Year, y = NDVI_media, color = Bosque, group = Bosque)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_ribbon(
    aes(ymin = NDVI_media - NDVI_ci,
        ymax = NDVI_media + NDVI_ci,
        fill = Bosque),
    color = NA,
    alpha = 0.15
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Zona, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colores_bosques) +
  scale_fill_manual(values = colores_bosques) +
  guides(fill = "none") +
  scale_x_continuous(breaks = 2017:2025) +
  scale_y_continuous(
    breaks = function(x) seq(min(x), max(x), length.out = 3),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(x = "Año",
       y = "NDVI",
       color = "Tipo de bosque"
       ) +
  tema_tfm_2

# NDMI
# ----------------------------------------------------------------------------
grafico_ndmi <- ggplot(indices_anuales, aes(x = Year, y = NDMI_media, color = Bosque, group = Bosque)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_ribbon(
    aes(ymin = NDMI_media - NDMI_ci,
        ymax = NDMI_media + NDMI_ci,
        fill = Bosque),
    color = NA,
    alpha = 0.15
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~ Zona, ncol = 1, scales = "free_y") +
  scale_color_manual(values = colores_bosques) +
  scale_fill_manual(values = colores_bosques) +
  guides(fill = "none") +
  scale_x_continuous(breaks = 2017:2025) +
  scale_y_continuous(
    breaks = function(x) seq(min(x), max(x), length.out = 3),
    labels = scales::label_number(accuracy = 0.01)
  ) +
  labs(x = "Año",
       y = "NDMI",
       color = "Tipo de bosque"
  ) +
  tema_tfm_2

# GRÁFICOS
# ----------------------------------------------------------------------------
print(grafico_ndvi)
print(grafico_ndmi)

# Guardar los gráficos
ggsave(
  filename = "04_outputs/g_trayectoria_NDVI_NDMI/grafico_ndvi.png", 
  plot = grafico_ndvi, 
  width = 14,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_trayectoria_NDVI_NDMI/grafico_ndmi.png", 
  plot = grafico_ndmi, 
  width = 14,
  height = 9,
  dpi = 300
)

# Gráfico que combina ambos índices
# ----------------------------------------------------------------------------
library(ggh4x)

datos_combo <- indices_anuales |>
  select(Zona, Bosque, Year,
         NDVI_media, NDVI_ci,
         NDMI_media, NDMI_ci) |>
  pivot_longer(
    cols = c(NDVI_media, NDVI_ci, NDMI_media, NDMI_ci),
    names_to = c("Indice", ".value"),
    names_pattern = "(NDVI|NDMI)_(media|ci)"
  ) |>
  mutate(Indice = factor(Indice, levels = c("NDVI", "NDMI")))

grafico_NDVI_NDMI <- ggplot(datos_combo, aes(x = Year, y = media, color = Bosque, group = Bosque)) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_ribbon(
    aes(ymin = media - ci,
        ymax = media + ci,
        fill = Bosque),
    color = NA,
    alpha = 0.15
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.3) +
  facet_grid2(
    Zona ~ Indice,
    scales = "free_y",
    independent = "y"
  ) +
  scale_color_manual(values = colores_bosques) +
  scale_fill_manual(values = colores_bosques) +
  guides(fill = "none") +
  scale_x_continuous(breaks = seq(2017, 2025, by = 2)) +
  labs(x = "Año",
       y = NULL,
       color = "Tipo de bosque") +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.01)
  ) +
  tema_tfm_2 +
  theme(
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey92"),
    plot.caption = element_text(size = 11, color = "grey40", hjust = 0)
  )

print(grafico_NDVI_NDMI)

ggsave(
  filename = "04_outputs/g_trayectoria_NDVI_NDMI/grafico_NDVI_NDMI.png",
  plot = grafico_NDVI_NDMI,
  width = 14,
  height = 9,
  dpi = 300
)

# CORRELACIÓN ENTRE NDVI Y NDMI (gráfico 3x3)
# ----------------------------------------------------------------------------
# Se usan todos los píxeles de datos_limpios
datos_correlacion <- datos_limpios

# Cálculo de la correlación de Pearson (r), su significancia (p-valor) y el
# coeficiente de determinación (R2) para cada combinación de Zona y Bosque

# Se usa Pearson porque el ajuste que se dibuja en el gráfico es una
# regresión lineal (geom_smooth(method = "lm")), y con Pearson R2 = r^2,
# es decir, ambos valores son coherentes entre sí (a diferencia de Spearman,
# que es una correlación de rangos y no corresponde a un ajuste lineal).

etiquetas_correlacion <- datos_correlacion |>
  group_by(Zona, Bosque) |>
  summarise(
    test = list(cor.test(NDMI, NDVI, method = "pearson")),
    .groups = "drop"
  ) |>
  mutate(
    r        = purrr::map_dbl(test, ~ unname(.x$estimate)),
    p_valor  = purrr::map_dbl(test, ~ .x$p.value),
    R2       = r^2,
    # Asteriscos de significancia: * p<0.05, ** p<0.01, *** p<0.001
    significancia = case_when(
      p_valor < 0.001 ~ "***",
      p_valor < 0.01  ~ "**",
      p_valor < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    etiqueta = sprintf("r = %.2f%s\nR\u00b2 = %.2f", r, significancia, R2)
  ) |>
  select(Zona, Bosque, r, p_valor, R2, significancia, etiqueta)

print(etiquetas_correlacion)

# Dibujamos el gráfico 3x3
grafico_correlacion_3x3 <- ggplot(datos_correlacion, aes(x = NDMI, y = NDVI, color = Bosque, fill = Bosque)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x,
    color = "red",
    fill = "darkred",
    alpha = 0.2, 
    linewidth = 1) +
  geom_text(
    data = etiquetas_correlacion,
    aes(x = -Inf, y = Inf, label = etiqueta),
    inherit.aes = FALSE,
    hjust = -0.05,
    vjust = 1.2,
    size = 4.2,
    color = "black",
    lineheight = 0.95
  ) +
  facet_grid(Bosque ~ Zona) +
  scale_color_manual(values = colores_bosques) +
  scale_fill_manual(values = colores_bosques) +
  guides(fill = "none") +
  labs(
    x = "NDMI",
    y = "NDVI",
    color = "Tipo de bosque"
  ) +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 16, face = "bold"),   # antes 11
    strip.background = element_rect(fill = "#f0f0f0"),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16))

# Mostramos el gráfico final
print(grafico_correlacion_3x3)

# Guardar el gráfico
ggsave(
  filename = "04_outputs/g_correlacion_3x3/correlacion_3x3_NDVI-NDMI_2.png", 
  plot = grafico_correlacion_3x3, 
  width = 12,
  height = 9,
  dpi = 300
)

# MATRIZ DE CORRELACIÓN ENTRE NDVI Y NDMI
# ----------------------------------------------------------------------------
model_data <- datos_correlacion |>
  select(NDVI, NDMI)

cor(model_data)
GGally::ggpairs(model_data)

# 1. Definir la función para el panel INFERIOR (puntos + línea de tendencia)
lower_fn <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(size = 2, alpha = 0.5, color = "#2c3e50") +     # Puntitos
    geom_smooth(method = "lm", se = TRUE, color = "#E74C3C",   # Línea roja
                fill = "#FADBD8", ...) +                       # Banda de confianza rosa
    theme_minimal()
}

# 2. Aplicarlo en ggpairs
cor_NDVI_NDMI <- ggpairs(
  model_data,
  lower = list(continuous = lower_fn), # Aquí se aplica la función
  diag = list(continuous = wrap("barDiag", bins = 30,
                                fill = "#2E86C1", color = "white")),
  upper = list(continuous = wrap("cor", size = 4, color = "black")))

ggsave(
  filename = "04_outputs/g_correlacion_3x3/cor_NDVU-NDMI.png",
  plot = cor_NDVI_NDMI,
  width = 10,
  height = 8,
  dpi = 300
)
