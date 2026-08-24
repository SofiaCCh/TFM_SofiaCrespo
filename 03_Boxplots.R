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
# para una ventana pre/post concreta (Lloret et al. 2011:
# PreDr y PostDr son la MEDIA de los años previos/posteriores a la
# sequía, no un año aislado; ver también Moreno-Fernández et al. 2022,
# Ecosystems, que usa la media de los 4 años previos/posteriores)
# ----------------------------------------------------------------------------

calcular_resiliencia <- function(datos, ventana, aseq) {
  
  anios_pre  <- (aseq - ventana):(aseq - 1)
  anios_post <- (aseq + 1):(aseq + ventana)
  
  # Referencias por Zona-Bosque:
  #  - ref_pre (PreDr): media de los "ventana" años previos a la sequía
  #  - ref_seq (Dr):    año de la sequía (valor único, no se promedia)
  medianas_ref <- datos |>
    group_by(Zona, Bosque) |>
    summarise(
      ref_pre = mean(NDVI[Year %in% anios_pre], na.rm = TRUE),
      ref_seq = mean(NDVI[Year == aseq], na.rm = TRUE),
      .groups = "drop"
    )
  
  # RESISTENCIA: año de sequía (Dr, por píxel) frente a la línea base pre-sequía (PreDr)
  df_rt <- datos |>
    filter(Year == aseq) |>
    left_join(medianas_ref, by = c("Zona", "Bosque")) |>
    mutate(Metrica = "Resistencia", Valor = NDVI / ref_pre) |>
    select(Zona, Bosque, Pixel_ID, Metrica, Valor)
  
  # PostDr: media por píxel de los "ventana" años posteriores a la sequía
  df_post <- datos |>
    filter(Year %in% anios_post) |>
    group_by(Zona, Bosque, Pixel_ID) |>
    summarise(NDVI_post = mean(NDVI, na.rm = TRUE), .groups = "drop") |>
    left_join(medianas_ref, by = c("Zona", "Bosque"))
  
  df_rc <- df_post |>
    mutate(Metrica = "Recuperación", Valor = NDVI_post / ref_seq) |>
    select(Zona, Bosque, Pixel_ID, Metrica, Valor)
  
  df_rs <- df_post |>
    mutate(Metrica = "Resiliencia", Valor = NDVI_post / ref_pre) |>
    select(Zona, Bosque, Pixel_ID, Metrica, Valor)
  
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

# 1 año pre- y post-sequía (media de 2021 y media de 2023, frente a 2022)
# ----------------------------------------------------------------------------
df_boxplot_1a <- calcular_resiliencia(datos_limpios, ventana = 1, aseq = aseq)
boxplot_1a <- graficar_boxplot_resiliencia(df_boxplot_1a)
print(boxplot_1a)

# 2 años pre- y post-sequía (media de 2020-2021 y media de 2023-2024, frente a 2022)
# ----------------------------------------------------------------------------
df_boxplot_2a <- calcular_resiliencia(datos_limpios, ventana = 2, aseq = aseq)
boxplot_2a <- graficar_boxplot_resiliencia(df_boxplot_2a)
print(boxplot_2a)

# 3 años pre- y post-sequía (media de 2019-2021 y media de 2023-2025, frente a 2022)
# ----------------------------------------------------------------------------
df_boxplot_3a <- calcular_resiliencia(datos_limpios, ventana = 3, aseq = aseq)
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
# Test de Wilcoxon entre tipos de bosque
# ============================================================================

library(multcompView)
library(tidyr)

#colores_ventanas <- c(
#  "1 año"  = "#BFD8D2",
#  "2 años" = "#5F9EA0",
#  "3 años" = "#264653"
#)

# Unir las tablas y marcar la ventana
# ----------------------------------------------------------------------------
df_resiliencia_todas <- bind_rows(
  df_boxplot_1a |> mutate(Ventana = "1 año"),
  df_boxplot_2a |> mutate(Ventana = "2 años"),
  df_boxplot_3a |> mutate(Ventana = "3 años")
  ) |>
  mutate(Ventana = factor(Ventana, levels = c("1 año", "2 años", "3 años")))

head(df_resiliencia_todas)

# Kruskal-Wallis: test global dentro de cada Zona-Ventana-Métrica
# ----------------------------------------------------------------------------
resultados_kruskal <- df_resiliencia_todas |>
  group_by(Zona, Ventana, Metrica) |>
  group_modify(~ {
    test <- kruskal.test(Valor ~ Bosque, data = .x)
    tibble(
      chi_cuadrado = unname(test$statistic),
      df = unname(test$parameter),
      p = test$p.value
    )
  }) |>
  ungroup()

head(resultados_kruskal, 30)

# Wilcoxon por pares entre tipos de bosque, dentro de cada Zona-Ventana-Métrica
# ----------------------------------------------------------------------------
# Cada Pixel_ID pertenece a UN solo tipo de bosque (a diferencia de las
# ventanas, donde el mismo píxel se repetía en las 3). Son grupos
# independientes, así que el test va SIN aparear (paired = FALSE) y no
# hace falta pivotar los datos a formato ancho.

comparaciones_bosques <- combn(levels(df_resiliencia_todas$Bosque), 2, simplify = FALSE)

resultados_wilcoxon <- df_resiliencia_todas |>
  group_by(Zona, Ventana, Metrica) |>
  group_modify(~ {
    
    datos_grupo <- .x
    
    bind_rows(lapply(comparaciones_bosques, function(par) {
      x <- datos_grupo$Valor[datos_grupo$Bosque == par[1]]
      y <- datos_grupo$Valor[datos_grupo$Bosque == par[2]]
      test <- wilcox.test(x, y, paired = FALSE)
      tibble(
        group1 = par[1],
        group2 = par[2],
        n1 = length(x),
        n2 = length(y),
        statistic = unname(test$statistic),
        p = test$p.value
      )
    }))
  }) |>
  ungroup() |>
  group_by(Zona, Ventana, Metrica) |>
  mutate(p.adj = p.adjust(p, method = "bonferroni")) |>
  ungroup()

head(resultados_wilcoxon, 10)

# Preparar los p-valores en el formato de la librería multcompLetters
# ----------------------------------------------------------------------------
resultados_wilcoxon <- resultados_wilcoxon |>
  mutate(comparacion = paste(group1, group2, sep = "-"))

letras_por_grupo <- resultados_wilcoxon |>
  group_by(Zona, Ventana, Metrica) |>
  group_modify(~ {
    p_vector <- setNames(.x$p.adj, .x$comparacion)
    letras <- multcompLetters(p_vector)$Letters
    tibble(Bosque = names(letras), Letra = letras)
  }) |>
  ungroup()

head(letras_por_grupo, 15)

# Arreglar la posición de las letras
# ----------------------------------------------------------------------------
posiciones_letras <- df_resiliencia_todas |>
  group_by(Zona, Ventana, Metrica, Bosque) |>
  summarise(y_letra = max(Valor, na.rm = TRUE) * 1.10, .groups = "drop")

letras_por_grupo <- letras_por_grupo |>
  select(-any_of("y_letra")) |>
  left_join(posiciones_letras, by = c("Zona", "Ventana", "Metrica", "Bosque"))

head(letras_por_grupo, 6)

# Preparar la función del gráfico por métrica
# ----------------------------------------------------------------------------
graficar_boxplot_bosques_test <- function(ventana_elegida) {
  
  datos_grafico <- df_resiliencia_todas |> filter(Ventana == ventana_elegida)
  letras_grafico <- letras_por_grupo |> filter(Ventana == ventana_elegida)
  
  ggplot(datos_grafico, aes(x = Bosque, y = Valor, fill = Bosque)) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.3, outlier.alpha = 0.3, width = 0.6) +
    geom_text(
      data = letras_grafico,
      aes(x = Bosque, y = y_letra, label = Letra),
      inherit.aes = FALSE,
      size = 4
    ) +
    facet_grid(Zona ~ Metrica) +
    scale_fill_manual(values = colores_bosques) +
    labs(x = NULL, y = ventana_elegida, fill = "Tipo de Bosque") +
    theme_bw(base_size = 16) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.text = element_text(size = 16, face = "bold"),
      strip.background = element_rect(fill = "#f0f0f0")
    )
}

# Gráficos
# ----------------------------------------------------------------------------
# 1 año
boxplot_1a_test <- graficar_boxplot_bosques_test("1 año")
print(boxplot_1a_test)

# 2 años
boxplot_2a_test <- graficar_boxplot_bosques_test("2 años")
print(boxplot_2a_test)

# 3 años
boxplot_3a_test <- graficar_boxplot_bosques_test("3 años")
print(boxplot_3a_test)

# Guardar los gráficos del test de Wilcoxon
# ----------------------------------------------------------------------------
ggsave(
  filename = "04_outputs/g_boxplots/boxplot_1a_test.png",
  plot = boxplot_1a_test,
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_boxplots/boxplot_2a_test.png",
  plot = boxplot_2a_test,
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_boxplots/boxplot_3a_test.png",
  plot = boxplot_3a_test,
  width = 12,
  height = 9,
  dpi = 300
)
