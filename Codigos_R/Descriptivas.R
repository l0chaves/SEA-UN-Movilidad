# Archivos y Librerias ----
setwd("C:/Users/luluf/OneDrive - Universidad Nacional de Colombia/SEA-UN-Movilidad"); getwd()

library(readxl)
library(dplyr)
library(sf)
library(sf)
library(ggplot2)
library(leaflet)
library(colorRamps)

zat_shp <- st_read("Datos_Originales/Zonificacion_EODH/ZAT2023/ZAT2023.shp") %>%
  st_transform(crs = 4326)
df_viajes <- read_excel("Datos_Originales/Base_datos_procesada_EODH/XLSX/d. Modulo viajes.xlsx")
df_etapas <- read_excel("Datos_Originales/Base_datos_procesada_EODH/XLSX/e. Modulo etapas.xlsx")


## Mapas origen destino ----
flujos <- df_viajes %>%
  group_by(zat_ori, zat_des) %>%
  summarise(
    num_viajes = n(),
    duracion_min = mean(duracion_min, na.rm = TRUE),
    etapas = mean(etapas, na.rm = TRUE)
  )

# Centroides de cada ZAT
centroides <- zat_shp %>%
  st_centroid() %>%
  select(zat_codigo = ZAT)

# Unir origen y destino con coordenadas
flujos_geo <- flujos %>%
  left_join(centroides, by = c("zat_ori" = "zat_codigo")) %>%
  rename(geometry_ori = geometry) %>%
  left_join(centroides, by = c("zat_des" = "zat_codigo")) %>%
  rename(geometry_des = geometry)

# Crear líneas (LINESTRING) entre puntos
flujos_geo <- flujos_geo %>%
  rowwise() %>%
  mutate(geometry = st_sfc(st_linestring(
    matrix(c(st_coordinates(geometry_ori), st_coordinates(geometry_des)), ncol = 2, byrow = TRUE)
  ), crs = 4326)) %>%
  st_as_sf()


# Escala de colores según duración
pal <- colorNumeric("YlOrRd", domain = flujos_geo$duracion_min)

mapa <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolylines(data = flujos_geo,
               color = ~pal(duracion_min),
               weight = ~log1p(num_viajes)*2,
               opacity = 0.7,
               popup = ~paste0("<b>Origen:</b> ", zat_ori,
                               "<br><b>Destino:</b> ", zat_des,
                               "<br><b>Viajes:</b> ", num_viajes,
                               "<br><b>Duración promedio:</b> ", round(duracion_min,1), " min",
                               "<br><b>Etapas promedio:</b> ", round(etapas,1))
  ) %>%
  addLegend("bottomright", pal = pal, values = flujos_geo$duracion_min,
            title = "Duración promedio (min)",
            opacity = 1)

# Guardar el mapa como archivo HTML
saveWidget(mapa, "mapa_flujos_interactivo.html")