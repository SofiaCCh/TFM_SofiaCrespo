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
  theme(legend.position = "right",
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
  scale_x_continuous(breaks = 2013:2025) +
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
  scale_x_continuous(breaks = 2013:2025) +
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

# CORRELACIÓN ENTRE NDVI Y NDMI (gráfico 3x3)
# ----------------------------------------------------------------------------
set.seed(123)

# Muestra de 1000 píxeles
datos_correlacion <- datos_limpios |>
  group_by(Zona, Bosque) |>
  slice_sample(n = 1000) |>
  ungroup()

# Dibujamos el gráfico 3x3
grafico_correlacion_3x3 <- ggplot(datos_correlacion, aes(x = NDMI, y = NDVI, color = Bosque, fill = Bosque)) +
  geom_point(alpha = 0.25, size = 1.5) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x,
    color = "red",
    fill = "darkred",
    alpha = 0.2, 
    linewidth = 1) +
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
  filename = "04_outputs/g_correlacion_3x3/correlacion_3x3_NDVI-NDMI.png", 
  plot = grafico_correlacion_3x3, 
  width = 12,
  height = 9,
  dpi = 300
)