# ============================================================================
# Boxplot de resistencia, recuperación y resiliencia
# pre- y post-sequía (NDVI), a 1, 2 y 3 años
# ============================================================================

library(dplyr)
library(ggplot2)

# Datos
# ----------------------------------------------------------------------------

datos_limpios <- readRDS("03_results/db_limpios/datos_limpios.rds") |>
  mutate(Bosque = recode(Bosque, "Plantacion" = "Plantación"))

# Paleta de colores por tipo de bosque
colores_bosques <- c(
  "Primario" = "#1A5320",
  "Secundario" = "#7CB342",
  "Plantación" = "#D87800"
)

# Año de la sequía (fijo para las tres ventanas)
aseq <- 2022


# Función: calcula Resistencia, Recuperación y Resiliencia
# para una ventana pre/post concreta
# ----------------------------------------------------------------------------

calcular_resiliencia <- function(datos, apre, apost, aseq) {
  
  # Medianas de referencia por Zona-Bosque: año pre-sequía y año de sequía
  medianas_ref <- datos |>
    group_by(Zona, Bosque) |>
    summarise(
      ref_pre = median(NDVI[Year == apre], na.rm = TRUE),
      ref_seq = median(NDVI[Year == aseq], na.rm = TRUE),
      .groups = "drop"
    )
  
  # RESISTENCIA: año de sequía frente a la línea base pre-sequía
  df_rt <- datos |>
    filter(Year == aseq) |>
    left_join(medianas_ref, by = c("Zona", "Bosque")) |>
    mutate(Metrica = "Resistencia", Valor = NDVI / ref_pre) |>
    select(Zona, Bosque, Metrica, Valor)
  
  # Año post-sequía, base para Recuperación y Resiliencia
  df_post <- datos |>
    filter(Year == apost) |>
    left_join(medianas_ref, by = c("Zona", "Bosque"))
  
  df_rc <- df_post |>
    mutate(Metrica = "Recuperación", Valor = NDVI / ref_seq) |>
    select(Zona, Bosque, Metrica, Valor)
  
  df_rs <- df_post |>
    mutate(Metrica = "Resiliencia", Valor = NDVI / ref_pre) |>
    select(Zona, Bosque, Metrica, Valor)
  
  # Unir las tres métricas y ordenar los factores
  bind_rows(df_rc, df_rs, df_rt) |>
    mutate(
      Metrica = factor(Metrica, levels = c("Recuperación", "Resiliencia", "Resistencia")),
      Bosque = factor(Bosque, levels = names(colores_bosques))
    )
}


# Función: dibuja el boxplot a partir de una tabla ya calculada
# ----------------------------------------------------------------------------

graficar_boxplot_resiliencia <- function(df_boxplot) {
  ggplot(df_boxplot, aes(x = Bosque, y = Valor, fill = Bosque)) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.3, outlier.alpha = 0.3, width = 0.6) +
    facet_grid(Zona ~ Metrica) +
    scale_fill_manual(values = colores_bosques) +
    labs(x = NULL, fill = "Tipo de Bosque") +
    theme_bw(base_size = 16) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "#f0f0f0")
    )
}

# 1 año pre- y post-sequía (2021, 2022, 2023)
# ----------------------------------------------------------------------------

apre1 <- 2021
apost1 <- 2023

df_boxplot_1a <- calcular_resiliencia(datos_limpios, apre1, apost1, aseq)
boxplot_1a <- graficar_boxplot_resiliencia(df_boxplot_1a)
print(boxplot_1a)

# 2 años pre- y post-sequía (2020, 2022, 2024)
# ----------------------------------------------------------------------------

apre2 <- 2020
apost2 <- 2024

df_boxplot_2a <- calcular_resiliencia(datos_limpios, apre2, apost2, aseq)
boxplot_2a <- graficar_boxplot_resiliencia(df_boxplot_2a)
print(boxplot_2a)

# 3 años pre- y post-sequía (2019, 2022, 2025)
# ----------------------------------------------------------------------------

apre3 <- 2019
apost3 <- 2025

df_boxplot_3a <- calcular_resiliencia(datos_limpios, apre3, apost3, aseq)
boxplot_3a <- graficar_boxplot_resiliencia(df_boxplot_3a)
print(boxplot_3a)

# Guardar los 3 boxplot
# ----------------------------------------------------------------------------
ggsave(
  filename = "04_outputs/g_boxplots/boxplot_1a.png", 
  plot = boxplot_1a, 
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_boxplots/boxplot_2a.png", 
  plot = boxplot_2a, 
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_boxplots/boxplot_3a.png", 
  plot = boxplot_3a, 
  width = 12,
  height = 9,
  dpi = 300
)

# ============================================================================
# Test de Wilcoxon entre ventanas (1, 2 y 3 años) por Zona-Bosque-Métrica
# ============================================================================

library(rstatix)
library(multcompView)

colores_ventanas <- c(
  "1 año"  = "#D8BFD8",
  "2 años" = "#9B59B6",
  "3 años" = "#4A235A"
)

colores_ventanas <- c(
  "1 año"  = "#E8D5B7",
  "2 años" = "#B08D57",
  "3 años" = "#5C4326"
)

# Unir las tablas y marcar la ventana
# ----------------------------------------------------------------------------
df_resiliencia_todas <- bind_rows(
  df_boxplot_1a |> mutate(Ventana = "1 año"),
  df_boxplot_2a |> mutate(Ventana = "2 años"),
  df_boxplot_3a |> mutate(Ventana = "3 años")
  ) |>
  mutate(Ventana = factor(Ventana, levels = c("1 año", "2 años", "3 años")))

head(df_resiliencia_todas)

# Wilcoxon por pares, dentro de cada Zona-Bosque-Métrica
# ----------------------------------------------------------------------------
resultados_wilcoxon <- df_resiliencia_todas |>
  group_by(Zona, Bosque, Metrica) |>
  pairwise_wilcox_test(Valor ~ Ventana, p.adjust.method = "bonferroni")

head(resultados_wilcoxon, 10)

# Preparar los p-valores en el formato de la librería multcompLetters
# ----------------------------------------------------------------------------
resultados_wilcoxon <- resultados_wilcoxon |>
  mutate(comparacion = paste(group1, group2, sep = "-"))

letras_por_grupo <- resultados_wilcoxon |>
  group_by(Zona, Bosque, Metrica) |>
  group_modify(~ {
    p_vector <- setNames(.x$p.adj, .x$comparacion)
    letras <- multcompLetters(p_vector)$Letters
    tibble(Ventana = names(letras), Letra = letras)
  }) |>
  ungroup()

head(letras_por_grupo, 15)

# Arreglar la posición de las letras
# ----------------------------------------------------------------------------
posiciones_letras <- df_resiliencia_todas |>
  group_by(Zona, Bosque, Metrica, Ventana) |>
  summarise(y_letra = max(Valor, na.rm = TRUE) * 1.10, .groups = "drop")

letras_por_grupo <- letras_por_grupo |>
  select(-any_of("y_letra")) |>
  left_join(posiciones_letras, by = c("Zona", "Bosque", "Metrica", "Ventana"))

head(letras_por_grupo, 6)

# Preparar la función del gráfico por métrica
# ----------------------------------------------------------------------------
graficar_boxplot_ventanas <- function(metrica_elegida) {
  
  datos_grafico <- df_resiliencia_todas |> filter(Metrica == metrica_elegida)
  letras_grafico <- letras_por_grupo |> filter(Metrica == metrica_elegida)
  
  ggplot(datos_grafico, aes(x = Ventana, y = Valor, fill = Ventana)) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.3, outlier.alpha = 0.3, width = 0.6) +
    geom_text(
      data = letras_grafico,
      aes(x = Ventana, y = y_letra, label = Letra),
      inherit.aes = FALSE,
      size = 4
    ) +
    facet_grid(Zona ~ Bosque) +
    scale_fill_manual(values = colores_ventanas) +
    guides(fill = "none") +
    labs(x = "Ventana pre/post-sequía", y = metrica_elegida) +
    theme_bw(base_size = 16) +
    theme(
      strip.text = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "#f0f0f0")
    )
}

# Gráficos
# ----------------------------------------------------------------------------

# Resiliencia
grafico_resiliencia_ventanas <- graficar_boxplot_ventanas("Resiliencia")
print(grafico_resiliencia_ventanas)

# Resistencia
grafico_recuperacion_ventanas <- graficar_boxplot_ventanas("Recuperación")
print(grafico_recuperacion_ventanas)

# Recuperación
grafico_resistencia_ventanas <- graficar_boxplot_ventanas("Resistencia")
print(grafico_resistencia_ventanas)

# Guardar los gráficos del test de Wilcoxon
# ----------------------------------------------------------------------------
ggsave(
  filename = "04_outputs/g_boxplots/wilcoxon_resiliencia.png",
  plot = grafico_resiliencia_ventanas,
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_boxplots/wilcoxon_recuperacion.png",
  plot = grafico_recuperacion_ventanas,
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_boxplots/wilcoxon_resistencia.png",
  plot = grafico_resistencia_ventanas,
  width = 12,
  height = 9,
  dpi = 300
)
