# Archivos y Librerias ----
setwd("C:/Users/luluf/OneDrive - Universidad Nacional de Colombia/SEA-UN-Movilidad"); getwd()

library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(leaflet)
library(colorRamps)
library(readxl)
library(patchwork)
library(magick)

loca_shp <- st_read("Datos_Originales/loca/Loca.shp") %>% st_transform(crs = 4326)
df_viajes <- read_excel("Datos_Originales/Base_datos_procesada_EODH/XLSX/d. Modulo viajes.xlsx")
df_etapas <- read_excel("Datos_Originales/Base_datos_procesada_EODH/XLSX/e. Modulo etapas.xlsx")

## Mapa de las localidades
ggplot(data = loca_shp) +
  geom_sf(fill = 'lightgray', color = "black", size = 0.2) +
  labs(title = "Mapa de Localidades",
       fill = "Localidad") +
  theme_minimal() +
  theme(legend.position = "none")

## Limpiar y Orginzar -----
quitar_tildes <- function(texto) {
  iconv(texto, from = "UTF-8", to = "ASCII//TRANSLIT")}

### sf's de origen y destino ----
# Centroides de cada ZAT
loca_cen <- loca_shp %>%
  st_centroid()

loca_ori <- loca_cen %>%
  select(LocNombre, geometry) %>%
  rename(localidad_ori = LocNombre) %>%
  rename(geometry_ori = geometry)  %>%
  mutate(localidad_ori = quitar_tildes(localidad_ori) %>%
           str_trim() %>%
           str_to_upper())

loca_des <- loca_cen %>%
  select(LocNombre, geometry) %>%
  rename(localidad_des = LocNombre) %>%
  rename(geometry_des = geometry)  %>%
  mutate(localidad_des = quitar_tildes(localidad_des) %>%
           str_trim() %>%
           str_to_upper())

### Organizar variables ----
df_desc <- df_viajes %>% 
  select(localidad_ori, localidad_des, duracion_min, etapas) %>%
  mutate(localidad_ori = quitar_tildes(localidad_ori) %>%
           str_trim() %>%
           str_to_upper()) %>%
  mutate(localidad_des = quitar_tildes(localidad_des) %>%
           str_trim() %>%
           str_to_upper()) %>%
  filter(!if_any(everything(), ~ .x == "NO APLICA"))

flujos <- df_desc %>%
  group_by(localidad_ori, localidad_des) %>%
  summarise(
    num_viajes = n(),
    duracion_min = mean(duracion_min, na.rm = TRUE),
    etapas = mean(etapas, na.rm = TRUE)) %>%
  left_join(loca_ori, by = c("localidad_ori" = "localidad_ori")) %>%
  left_join(loca_des, by = c("localidad_des" = "localidad_des")) 

# Crear líneas (LINESTRING) entre puntos
flujos_sf <- flujos %>%
  rowwise() %>%
  mutate(geometry = st_sfc(st_linestring(
    matrix(c(st_coordinates(geometry_ori), st_coordinates(geometry_des)), ncol = 2, byrow = TRUE)
  ), crs = 4326)) %>%
  st_as_sf()  %>% 
  st_cast("LINESTRING") 


# Mapas ----
## Numero de viajes ----
# Crear una lista de líneas entre las coordenadas de origen y destino
viajes_lines <- st_sf(
  origen = flujos$localidad_ori,
  destino = flujos$localidad_des,
  num_viajes = flujos$num_viajes,
  geometry = mapply(function(x, y) st_sfc(st_linestring(matrix(c(x, y), ncol = 2))), 
                    flujos$geometry_ori, flujos$geometry_des)
)


viajes_dist <- viajes_lines %>% filter(origen != destino)
st_crs(viajes_dist) <- 4326

ggplot() +
  geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
  geom_sf(data = viajes_dist, aes(color = num_viajes)) +
  scale_color_gradientn(
    colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
    limits = c(min(flujos$num_viajes), max(flujos$num_viajes))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
  ) +
  scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(
    title = "Mapa de Flujos de Viajes",
    color = "Número de Viajes") +
  theme(legend.position = "bottom")

### Por cada localidad ----
loca_list <- unique(viajes_lines$origen)
loca_graph <- vector("list", length = 20)

for (i in 1:length(loca_list)) {
  l  <- loca_list[i]
  viajes_loc <- viajes_dist %>% filter(origen == l)
  st_crs(viajes_loc) <- 4326
  
  p_l <- ggplot() +
    geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
    geom_sf(data = viajes_loc, aes(color = num_viajes)) +
    scale_color_gradientn(
      colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
      limits = c(min(viajes_loc$num_viajes), max(viajes_loc$num_viajes))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
    ) +
    scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
    labs(
      title = paste("Mapa de Flujos de Viajes en", l, sep = "\n"),
      color = "Número de Viajes") +
    theme(legend.position = "right")
  
  loca_graph[[i]] <- p_l
}

#Todos los graficos en un facet
wrap_plots(loca_graph, nrow = 2, ncol = 10)

#Todos los graficos en un gif
imagenes <- list()

for (i in seq_along(loca_graph)) {
  filename <- paste0("frame_", sprintf("%02d", i), ".png")
  ggsave(filename, plot = loca_graph[[i]], width = 6, height = 4, dpi = 150)
  
  img <- image_read(filename)
  img <- image_background(img, "white", flatten = TRUE)  # 👈 fondo blanco aquí
  
  imagenes[[i]] <- img
}

gif <- image_animate(image_join(imagenes), delay = 300)  # puedes ajustar los fps
image_write(gif, "mapas_animados.gif")

print(gif)

