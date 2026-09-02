# ============================================================================
# 1. Cargar las librerías necesarias
# ============================================================================

library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)

# Generar un tema para los gráficos
tema_tfm <- theme_minimal(base_size = 16) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 16))

# ============================================================================
# 2. Leer los tres archivos CSV descargados del CSIC
# ============================================================================

azt_spei <- read.csv("01_Data/SPEI/azt_SPEI_serie.csv")
liz_spei <- read.csv("01_Data/SPEI/liz_SPEI_serie.csv")
tjn_spei <- read.csv("01_Data/SPEI/tjn_SPEI_serie.csv")

# ============================================================================
# 3. Crear el df final, con los datos en formato tidy: 1 FILA 1 OBSERVACIÓN
# ============================================================================

# 3.1. Dataframe SPEI con toda la serie histórica y los datos de SPEI (3 y 6 meses)
df_spei <- bind_rows(
  azt_spei |> 
    select(Fecha = DATA, spei_3, spei_6, spei_9, spei_12) |>
    mutate(Bosque = "Aztaparreta"), # mutate crea una columna nueva
  liz_spei |>
    select(Fecha = DATA, spei_3, spei_6, spei_9, spei_12) |>
    mutate(Bosque = "Lizardoia"),
  tjn_spei |>
    select(Fecha = DATA, spei_3, spei_6, spei_9, spei_12) |>
    mutate(Bosque = "Tejera Negra")
) |>
  mutate(Fecha = as.Date(Fecha)) |> # convierte el texto de las fecha en formato calendario
  pivot_longer(cols = c("spei_3", "spei_6", "spei_9", "spei_12"),
               names_to = "Escala",
               values_to = "SPEI")

# 3.2. Dataframe SPEI-3 a partir de 2017
df_spei3_2017 <- df_spei |>
  filter(Escala == "spei_3", Fecha >= as.Date("2017-01-01"))

# 3.3. Dataframe SPEI-6 a partir de 2017
df_spei6_2017 <- df_spei |>
  filter(Escala == "spei_6", Fecha >= as.Date("2017-01-01"))

# 3.4. Dataframe SPEI-9 a partir de 2017
df_spei9_2017 <- df_spei |>
  filter(Escala == "spei_9", Fecha >= as.Date("2017-01-01"))

# 3.5. Dataframe SPEI-12 a partir de 2017
df_spei12_2017 <- df_spei |>
  filter(Escala == "spei_12", Fecha >= as.Date("2017-01-01"))

# ============================================================================
# 4. Gráficas
# ============================================================================

# SPEI 3
# ----------------------------------------------------------------------------
grafica_spei3_2017 <- ggplot(df_spei3_2017,      # usa los datos del SPEI 3
                             aes(x = Fecha,      # fecha en el eje X
                                 y = SPEI)       # SPEI en el eje Y
                             ) +
  # Linea horizontal para la normalidad
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  # Línea de los datos (grosor 0,4)
  geom_line(linewidth = 0.4, color = "#26787E") +
  # Linea horizontal para el umbral de la sequía en -1,5
  geom_hline(yintercept = -1.5, linetype = "dashed", color = "red", linewidth = 1) +
  # Resaltar el año 2022 dibujando un rectángulo 
  annotate("rect", 
           xmin = as.Date("2022-01-01"), 
           xmax = as.Date("2022-12-31"),
           ymin = -Inf, # desde abajo del todo
           ymax = Inf,  # hasta arriba del todo
           alpha = 0.2, # transparencia
           fill = "red") +
  # La gráfica la separa en función de la columna Bosque
  facet_wrap(~ Bosque, ncol = 1) +
  # Para dividir el eje X, por año
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # Etiquetas
  labs(x = "Año",
       y = "Índice SPEI-3") +
  tema_tfm

# SPEI 6
# ----------------------------------------------------------------------------
grafica_spei6_2017 <- ggplot(df_spei6_2017,
                             aes(x = Fecha,
                                 y = SPEI)
                             ) +
  geom_hline(yintercept = 0, color = "grey20") +
  geom_line(linewidth = 0.4, color = "#26787E") +
  geom_hline(yintercept = -1.5, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("rect", 
           xmin = as.Date("2022-01-01"), 
           xmax = as.Date("2022-12-31"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "red") +
  facet_wrap(~ Bosque, ncol = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Año",
       y = "Índice SPEI-6") +
  tema_tfm

# SPEI 9
# ----------------------------------------------------------------------------
grafica_spei9_2017 <- ggplot(df_spei9_2017,
                              aes(x = Fecha,
                                  y = SPEI)
) +
  geom_hline(yintercept = 0, color = "grey20") +
  geom_line(linewidth = 0.4, color = "#26787E") +
  geom_hline(yintercept = -1.5, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("rect", 
           xmin = as.Date("2022-01-01"), 
           xmax = as.Date("2022-12-31"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "red") +
  facet_wrap(~ Bosque, ncol = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Año",
       y = "Índice SPEI-9") +
  tema_tfm

# SPEI 12
# ----------------------------------------------------------------------------
grafica_spei12_2017 <- ggplot(df_spei12_2017,
                             aes(x = Fecha,
                                 y = SPEI)
                             ) +
  geom_hline(yintercept = 0, color = "grey20") +
  geom_line(linewidth = 0.4, color = "#26787E") +
  geom_hline(yintercept = -1.5, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("rect", 
           xmin = as.Date("2022-01-01"), 
           xmax = as.Date("2022-12-31"),
           ymin = -Inf,
           ymax = Inf,
           alpha = 0.2,
           fill = "red") +
  facet_wrap(~ Bosque, ncol = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "Año",
       y = "Índice SPEI-12") +
  tema_tfm

# DIBUJAR LAS GRÁFICAS (SPEI 3, 6 Y 12)
# ----------------------------------------------------------------------------
print(grafica_spei3_2017)
print(grafica_spei6_2017)
print(grafica_spei9_2017)
print(grafica_spei12_2017)

# ============================================================================
# 5. Guardar a PNG
# ============================================================================
ggsave(
  filename = "04_outputs/g_spei/grafica_spei3_zonas.png", 
  plot = grafica_spei3_2017, 
  width = 12,       # Ancho de la imagen en pulgadas
  height = 9,       # Alto de la imagen en pulgadas (le damos más altura por tener 3 filas)
  dpi = 300         # Resolución profesional de impresión (puntos por pulgada)
)

ggsave(
  filename = "04_outputs/g_spei/grafica_spei6_zonas.png", 
  plot = grafica_spei6_2017, 
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_spei/grafica_spei9_zonas.png", 
  plot = grafica_spei9_2017, 
  width = 12,
  height = 9,
  dpi = 300
)

ggsave(
  filename = "04_outputs/g_spei/grafica_spei12_zonas.png", 
  plot = grafica_spei12_2017, 
  width = 12,
  height = 9,
  dpi = 300
)

# ============================================================================
# 6. Correlaciones índices SPEI
# ============================================================================

# Formato ancho de tabla, una columna por escala SPEI
# ----------------------------------------------------------------------------
df_spei_wide <- df_spei |>
  pivot_wider(names_from = Escala, values_from = SPEI) |>
  select(Fecha, Bosque, spei_3, spei_6, spei_9, spei_12)

# Correlación global
# ----------------------------------------------------------------------------
cor_global <- df_spei_wide |>
  select(spei_3, spei_6, spei_9, spei_12) |>
  cor(use = "pairwise.complete.obs")

round(cor_global, 2)
round(cor_global^2, 2) # Varianza explicada (R2 = r2)

# Correlación por sitio
# ----------------------------------------------------------------------------
cor_azt <- df_spei_wide |>
  filter(Bosque == "Aztaparreta") |>
  select(spei_3, spei_6, spei_9, spei_12) |>
  cor(use = "pairwise.complete.obs")

cor_liz <- df_spei_wide |>
  filter(Bosque == "Lizardoia") |>
  select(spei_3, spei_6, spei_9, spei_12) |>
  cor(use = "pairwise.complete.obs")

cor_tjn <- df_spei_wide |>
  filter(Bosque == "Tejera Negra") |>
  select(spei_3, spei_6, spei_9, spei_12) |>
  cor(use = "pairwise.complete.obs")

round(cor_azt, 2)
round(cor_liz, 2)
round(cor_tjn, 2)
