install.packages(c("flextable", "officer"))
library(dplyr)
library(purrr)
library(tibble)
library(flextable)
library(officer)

modelos <- list(
  list(zona = "Aztaparreta",  indice = "NDVI", modelo = modelo_ndvi_azt),
  list(zona = "Aztaparreta",  indice = "NDMI", modelo = modelo_ndmi_azt),
  list(zona = "Lizardoia",    indice = "NDVI", modelo = modelo_ndvi_liz),
  list(zona = "Lizardoia",    indice = "NDMI", modelo = modelo_ndmi_liz),
  list(zona = "Tejera Negra", indice = "NDVI", modelo = modelo_ndvi_tjn),
  list(zona = "Tejera Negra", indice = "NDMI", modelo = modelo_ndmi_tjn)
)

# Tabla 1: ajuste general de cada uno de los seis modelos
 tabla_ajuste <- map_dfr(modelos, function(x) {
  s <- summary(x$modelo)
  tibble(
    Zona = x$zona,
    Indice = x$indice,
    R2_ajustado = round(s$r.sq, 3),
    Devianza_explicada_pct = round(s$dev.expl * 100, 2),
    n = s$n
  )
})

# Tabla 2: significancia de la funcion suave del año, por tipo de bosque
tabla_suaves <- map_dfr(modelos, function(x) {
  s <- summary(x$modelo)
  as.data.frame(s$s.table) |>
    rownames_to_column("Termino") |>
    mutate(
      Zona = x$zona,
      Indice = x$indice,
      Termino = gsub("s\\(Year\\):Bosque", "", Termino)
    ) |>
    select(Zona, Indice, Tipo_de_bosque = Termino, edf, F = F, p_valor = `p-value`)
})

# Exportar ambas tablas a un documento Word
doc <- read_docx() |>
  body_add_par("Tabla 1. Ajuste de los modelos GAM por zona e índice", style = "heading 2") |>
  body_add_flextable(flextable(tabla_ajuste)) |>
  body_add_par("") |>
  body_add_par("Tabla 2. Significancia de la tendencia temporal (función suave) por tipo de bosque", style = "heading 2") |>
  body_add_flextable(flextable(tabla_suaves))

print(doc, target = "tablas_gam.docx")
