# ==============================================================================
# CÁLCULO DE POTENCIAL FOTOVOLTAICO EN ÁREA ESPECÍFICA
# Basado en edificaciones de OpenStreetMap (OSM)
# ==============================================================================

# 0. Instalación de librerías necesarias

#install.packages(c("sf", "osmdata", "tidyverse", "units", "leaflet", "ggspatial"), dependencies = TRUE)

# 1. Cargar librerías necesarias
library(sf)
library(osmdata)
library(tidyverse)
library(units)
library(leaflet)
library(ggspatial)
library(htmlwidgets)

# Limpiamos el entorno
rm(list  = ls())
gc()


# ==============================================================================
# 2. Crear polígono específico del área de estudio
# ==============================================================================

cat("Creando polígono específico del área de estudio...\n")


# Coordenadas obtenidas desde geojson.io
coordenadas <- matrix(
  c(
    -72.9172582, 11.5462595,
    -72.9158426, 11.5464025,
    -72.9152573, 11.5463929,
    -72.9153287, 11.5459479,
    -72.9172775, 11.5456871,
    -72.9172582, 11.5462595
  ),
  ncol = 2,
  byrow = TRUE
)


# Crear polígono espacial
riohacha <- st_sf(
  geometry = st_sfc(
    st_polygon(
      list(coordenadas)
    ),
    crs = 4326
  )
)


# Verificar geometría
riohacha <- riohacha %>%
  st_transform(4326) %>%
  st_make_valid()


# ==============================================================================
# 3. Obtener viviendas/edificaciones de OSM dentro del polígono
# ==============================================================================

cat("Descargando edificaciones del área de estudio...\n")

library(jsonlite)


# Caja del polígono
bbox <- st_bbox(riohacha)


# Consulta Overpass:
# Busca todos los edificios (viviendas, apartamentos y construcciones sin clasificar)

consulta <- paste0(
  '[out:json][timeout:300];',
  '(',
  'way["building"](',
  bbox["ymin"], ",",
  bbox["xmin"], ",",
  bbox["ymax"], ",",
  bbox["xmax"],
  ');',
  'relation["building"](',
  bbox["ymin"], ",",
  bbox["xmin"], ",",
  bbox["ymax"], ",",
  bbox["xmax"],
  ');',
  ');',
  'out geom;'
)


url <- "https://overpass-api.de/api/interpreter"


resultado <- fromJSON(
  paste0(
    url,
    "?data=",
    URLencode(consulta)
  ),
  flatten = TRUE
)



# Crear lista de geometrías

lista_poligonos <- list()



for(i in seq_along(resultado$elements$id)){
  
  
  if(!is.null(resultado$elements$geometry[[i]])){
    
    
    coords <- resultado$elements$geometry[[i]]
    
    
    poligono <- cbind(
      coords$lon,
      coords$lat
    )
    
    
    # cerrar polígono
    poligono <- rbind(
      poligono,
      poligono[1,]
    )
    
    
    # validar cantidad mínima de puntos
    if(nrow(poligono) >= 4){
      
      lista_poligonos[[length(lista_poligonos)+1]] <-
        st_polygon(list(poligono))
      
    }
    
  }
}



# Crear objeto espacial

if(length(lista_poligonos)==0){
  
  stop("No se encontraron edificaciones en el polígono.")
  
}



edificios <- st_sf(
  geometry = st_sfc(
    lista_poligonos,
    crs = 4326
  )
)



cat(
  "Edificaciones encontradas:",
  nrow(edificios),
  "\n"
)

# ==============================================================================
# 4. Procesamiento de datos y cálculo de áreas
# ==============================================================================

cat("Procesando geometrías y áreas...\n")


if (is.null(edificios) || nrow(edificios) == 0) {
  stop("No se encontraron edificios con el filtro actual. Intenta quitar el filtro de amenity.")
}


# Filtrar únicamente edificios dentro del polígono

edificios_riohacha <- edificios %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  st_intersection(st_union(riohacha))


# Proyección métrica

edificios_utm <- st_transform(
  edificios_riohacha,
  3116
)



# Calcular área y filtrar edificios grandes

edificios_areas <- edificios_utm %>%
  mutate(
    area_m2 = st_area(geometry),
    area_m2 = drop_units(area_m2)
  ) %>%
  filter(area_m2 > 10)



if (nrow(edificios_areas) == 0) {
  stop("No hay edificios con área mayor a 50 m2.")
}




# ==============================================================================
# 5. Cálculo del Potencial Fotovoltaico
# ==============================================================================


factor_uso_techo <- 0.6

potencia_panel_kw_m2 <- 0.18

irradiacion_media_kwh_m2_dia <- 5.8

PR <- 0.75



cat("Calculando potencial energético...\n")


edificios_pv <- edificios_areas %>%
  mutate(
    area_util_m2 = area_m2 * factor_uso_techo,
    
    potencia_pico_kw = area_util_m2 * potencia_panel_kw_m2,
    
    prod_kwh_dia = area_util_m2 *
      irradiacion_media_kwh_m2_dia *
      PR,
    
    prod_kwh_anio = prod_kwh_dia * 365
  )




# ==============================================================================
# 6. Resumen de resultados
# ==============================================================================


pot_total_mw <- sum(
  edificios_pv$potencia_pico_kw,
  na.rm = TRUE
) / 1000


prod_total_gwh <- sum(
  edificios_pv$prod_kwh_anio,
  na.rm = TRUE
) / 1000000



cat("\n--- RESUMEN ÁREA DE ESTUDIO ---\n")

cat(
  "Total edificios analizados:",
  nrow(edificios_pv),
  "\n"
)

cat(
  "Potencia Pico Total Instalable:",
  round(pot_total_mw,2),
  "MW\n"
)

cat(
  "Producción Anual Estimada:",
  round(prod_total_gwh,2),
  "GWh/año\n"
)




# ==============================================================================
# 7. Visualización con Leaflet
# ==============================================================================

centro <- st_centroid(
  st_union(riohacha)
) %>% 
  st_coordinates()


mapa <- leaflet() %>%
  
  addProviderTiles(
    providers$Esri.WorldImagery,
    group = "Satélite"
  ) %>%
  
  addProviderTiles(
    providers$OpenStreetMap,
    group = "Mapa Callejero"
  ) %>%
  
  addPolygons(
    data = riohacha,
    color = "white",
    weight = 2,
    fill = FALSE,
    group = "Área estudio"
  ) %>%
  
  addPolygons(
    data = st_transform(edificios_pv,4326),
    color = "yellow",
    weight = 1,
    fillOpacity = 0.6,
    popup = ~paste0(
      "Área: ",
      round(area_m2,1),
      " m2<br>",
      "Potencial: ",
      round(prod_kwh_anio,0),
      " kWh/año"
    ),
    group = "Edificios con Potencial"
  ) %>%
  
  addLayersControl(
    baseGroups = c(
      "Satélite",
      "Mapa Callejero"
    ),
    overlayGroups = c(
      "Área estudio",
      "Edificios con Potencial"
    ),
    options = layersControlOptions(
      collapsed = FALSE
    )
  ) %>%
  
  setView(
    lng = centro[1],
    lat = centro[2],
    zoom = 18
  )


# Visualizar en RStudio
mapa


# Crear archivo HTML interactivo
htmlwidgets::saveWidget(
  mapa,
  "mapa_potencial_fotovoltaico.html",
  selfcontained = TRUE
)


# Abrir en navegador
browseURL(
  "mapa_potencial_fotovoltaico.html"
)

# ==============================================================================
# 8. Guardar resultados
# ==============================================================================


st_write(
  edificios_pv,
  "edificios_potencial_pv_area_estudio.gpkg",
  delete_dsn = TRUE
)