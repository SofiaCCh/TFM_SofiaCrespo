# ============================================================================
# Extract Landsat pixels by forest type and year
# ============================================================================

# ============================================================================
# 1. Cargar las librerías necesarias
# ============================================================================

library(terra)     # para manejar imágenes ráster
library(sf)        # para manejar datos vectoriales (shapefiles)
library(dplyr)     # librería de tidyverse para manipulación de datos
library(stringr)   # trabajar con textos (nombres de archivos)
library(purrr)     # para ejecución repetitiva de funciones

# ============================================================================
# 2. User input - Parámetros de entrada
# ============================================================================

ruta_raster <- "01_data/raster_landsat/"

# Listar los archivos ráster (e.g. Landsat_2014.tif)
archivos_raster <- list.files(
  path = ruta_raster,
  pattern = "\\.tif$",
  full.names = TRUE
)

# Listar los archivos vectoriales
archivos_shp <- tibble::tribble(
  ~archivo,                                            ~zona,          ~bosque,
  "01_data/shp_zonas/aztaparreta_primaryf.shp",       "Aztaparreta",  "Primario",
  "01_data/shp_zonas/aztaparreta_secondaryf.shp",     "Aztaparreta",  "Secundario",
  "01_data/shp_zonas/aztaparreta_plantation.shp",     "Aztaparreta",  "Plantacion",
  "01_data/shp_zonas/lizardoia_primaryf.shp",         "Lizardoia",    "Primario",
  "01_data/shp_zonas/lizardoia_secondaryf.shp",       "Lizardoia",    "Secundario",
  "01_data/shp_zonas/lizardoia_plantation.shp",       "Lizardoia",    "Plantacion",
  "01_data/shp_zonas/tejeranegra_primaryf.shp",       "Tejera Negra", "Primario",
  "01_data/shp_zonas/tejeranegra_secondaryf.shp",     "Tejera Negra", "Secundario",
  "01_data/shp_zonas/tejeranegra_plantation.shp",     "Tejera Negra", "Plantacion"
)

# ============================================================================
# 3. Leer los polígonos y combinar los 9 shp
# ============================================================================

bosque <- archivos_shp |>
  pmap(function(archivo, zona, bosque) {
    st_read(archivo, quiet = TRUE) |>
      mutate(
        Zona = zona, 
        Bosque = bosque
      )
  }) |>
  bind_rows() |>
  mutate(
    Zona = factor(Zona, levels = c("Aztaparreta", "Lizardoia", "Tejera Negra")),
    Bosque = factor(Bosque, levels = c("Primario", "Secundario", "Plantacion")),
    polygon_id = row_number()
  )

# ============================================================================
# 4. Comprobar la geometría del ráster por ZONA
# ============================================================================

# a. Crear una tabla (tibble) con los archivos ráster y su zona asignada
tabla_rasters <- tibble(archivo = archivos_raster) |>
  mutate(
    nombre_base = basename(archivos_raster),
    Zona = case_when(
      str_detect(nombre_base, "(?i)Aztaparreta") ~ "Aztaparreta",
      str_detect(nombre_base, "(?i)Lizardoia") ~ "Lizardoia",
      str_detect(nombre_base, "(?i)Tejera_?Negra") ~ "Tejera Negra",
      TRUE ~ NA_character_
    )
  )

# b. Función para comprobar los rásters de una zona concreta
comprobar_geometria_zona <- function(df_zona) {
  
  nombre_zona <- unique(df_zona$Zona)
  archivos_zona <- df_zona$archivo
  
  # Si solo hay 1 archivo o ninguno en esta zona, no hay nada que comparar
  if (length(archivos_zona) <= 1) {
    return(invisible(TRUE))
  }
  
  # Tomamos el primer ráster de la zona como referencia
  referencia <- rast(archivos_zona[1])
  
  # Comparamos el resto (del 2 en adelante) con la referencia
  geometrias_iguales <- map_lgl(archivos_zona[-1], function(x) {
    compareGeom(referencia, rast(x), stopOnError = FALSE)
  })
  
  if (!all(geometrias_iguales)) {
    stop(paste("ERROR: Los rásters de la zona", nombre_zona, "NO tienen geometría idéntica."))
  } else {
    cat("✓ Geometría validada para la zona:", nombre_zona, "\n")
  }
}

# c. Aplicar la comprobación dividiendo la tabla por zonas
# Usamos walk() de purrr porque solo queremos ejecutar la acción (el mensaje o error),
# sin que nos devuelva datos para guardar.
tabla_rasters |>
  group_split(Zona) |>
  walk(comprobar_geometria_zona)

# ============================================================================
# 5. Definir los nombres de las bandas
# ============================================================================

bandas <- c(
  
  "SR_B1",
  "SR_B2",
  "SR_B3",
  "SR_B4",
  "SR_B5",
  "SR_B7",
  
  "TCB",
  "TCG",
  "TCW",
  
  "NBR",
  "NDVI",
  "EVI"
  
)

# ============================================================================
# 6. Función de extracción
# ============================================================================

extraer_pixeles <- function(archivos_raster) {
  
  nombre_archivo <- basename(archivos_raster)
  cat("Procesando:", nombre_archivo, "\n")
  
  # Extraer el año del nombre (los 4 dígitos)
  anio <- as.integer(str_extract(nombre_archivo, "\\d{4}"))
  
  # Identificar la zona leyendo el nombre del archivo
  zona_actual <- case_when(
    str_detect(nombre_archivo, "(?i)Aztaparreta") ~ "Aztaparreta",
    str_detect(nombre_archivo, "(?i)Lizardoia") ~ "Lizardoia",
    str_detect(nombre_archivo, "(?i)Tejera_?Negra") ~ "Tejera Negra",
    TRUE ~ NA_character_
  )
  
  # Filtrar los polígonos usando sintaxis tidy sobre el objeto sf
  bosque_filtrado <- bosque |> 
    filter(Zona == zona_actual)
  
  # Cargar el ráster y asignar nombres de bandas
  raster_img <- rast(archivos_raster)
  names(raster_img) <- bandas
  
  # Como el ráster tiene los metadatos del CRS corruptos ("Unknown engineering datum"),
  # le forzamos a usar el CRS limpio y perfecto del shapefile
  terra::crs(raster_img) <- sf::st_crs(bosque_filtrado)$wkt
  
  # Extraer datos a nivel de píxel (conversión a SpatVector al vuelo)
  valores <- terra::extract(
    raster_img, 
    vect(bosque_filtrado), 
    cells = TRUE, 
    xy = TRUE
  )
  
  # Unir resultados con los metadatos de los polígonos y añadir el año
  resultado <- valores |> 
    left_join(
      bosque_filtrado |> 
        st_drop_geometry() |> 
        mutate(ID = row_number()), 
      by = "ID"
    ) |> 
    mutate(Year = anio)
  
  return(resultado)
}

# ============================================================================
# 7. Extraer cada ráster
# ============================================================================

# map_dfr de purrr aplica la función a cada uno de los archivos .tif
# y une todas las tablas resultantes en un solo dataframe gigante

db_pixel <- archivos_raster |>
  map_dfr(extraer_pixeles) |>
  mutate(
    # Creamos un identificador único y estable para cada píxel en el espacio
    Pixel_ID = paste(Zona, round(x, 3), round(y, 3), sep = "_")
  ) |>
  select(
    # Reordenamos las columnas para que la tabla sea intuitiva al leerla
    Pixel_ID, 
    Year, 
    Zona, 
    Bosque, 
    polygon_id,             # El ID de los 9 polígonos originales
    cell,                   # El número de celda del ráster
    x,                      # Coordenada longitud
    y,                      # Coordenada latitud
    all_of(bandas)
  )

# ============================================================================
# 8. Guardar el resultado en formato nativo de R (.rds)
# ============================================================================

saveRDS(db_pixel, "03_results/db_landsat/db_landsat.rds")

cat("¡Extracción completada con éxito! Datos guardados.\n")
