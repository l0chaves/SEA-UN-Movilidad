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
# Numero de viajes ----
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
    limits = c(min(viajes_dist$num_viajes), max(viajes_dist$num_viajes))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
  ) +
  scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(
    title = "Mapa de Flujos de Viajes",
    color = "Número de Viajes") +
  theme(legend.position = "right")

### Por cada localidad ----
loca_list <- unique(viajes_lines$origen)
loca_graph <- vector("list", length = 20)

#### Desde ----
for (i in 1:length(loca_list)) {
  l  <- loca_list[i]
  viajes_loc <- viajes_dist %>% filter(origen == l)
  st_crs(viajes_loc) <- 4326
  
  p_l <- ggplot() +
    geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
    geom_sf(data = viajes_loc, aes(color = num_viajes)) +
    scale_color_gradientn(
      colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
      limits = c(min(viajes_dist$num_viajes), max(viajes_dist$num_viajes))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
    ) +
    scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
    labs(
      title = paste("Mapa de flujos de viajes desde", l, sep = "\n"),
      color = "Número de Viajes") +
    theme(legend.position = "right")
  
  loca_graph[[i]] <- p_l
}

#Todos los graficos en un gif
imagenes <- list()

for (i in seq_along(loca_graph)) {
  filename <- paste0("frame_", sprintf("%02d", i), ".png")
  ggsave(filename, plot = loca_graph[[i]], width = 6, height = 4, dpi = 150)
  
  img <- image_read(filename)
  img <- image_background(img, "white", flatten = TRUE)  # 👈 fondo blanco aquí
  
  imagenes[[i]] <- img
}

gif <- image_animate(image_join(imagenes), delay = 150)  # puedes ajustar los fps
image_write(gif, "nv_desde.gif")

print(gif)

#### Hacia ----
for (i in 1:length(loca_list)) {
  l  <- loca_list[i]
  viajes_loc <- viajes_dist %>% filter(destino == l)
  st_crs(viajes_loc) <- 4326
  
  p_l <- ggplot() +
    geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
    geom_sf(data = viajes_loc, aes(color = num_viajes)) +
    scale_color_gradientn(
      colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
      limits = c(min(viajes_dist$num_viajes), max(viajes_dist$num_viajes))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
    ) +
    scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
    labs(
      title = paste("Mapa de flujos de viajes hacia", l, sep = "\n"),
      color = "Número de Viajes") +
    theme(legend.position = "right")
  
  loca_graph[[i]] <- p_l
}

#Todos los graficos en un gif
imagenes <- list()

for (i in seq_along(loca_graph)) {
  filename <- paste0("frame_", sprintf("%02d", i), ".png")
  ggsave(filename, plot = loca_graph[[i]], width = 6, height = 4, dpi = 150)
  
  img <- image_read(filename)
  img <- image_background(img, "white", flatten = TRUE)  # 👈 fondo blanco aquí
  
  imagenes[[i]] <- img
}

gif <- image_animate(image_join(imagenes), delay = 150)  # puedes ajustar los fps
image_write(gif, "nv_hacia.gif")

print(gif)

#### Dentro ----
viajes_igO <- flujos %>% filter(localidad_ori == localidad_des) %>% ungroup() %>%
  select(localidad_ori, num_viajes, geometry_ori)
viajes_igO <- st_as_sf(viajes_igO, sf_column_name = "geometry_ori", crs = 4326)

ggplot() +
  geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
  geom_sf(data = viajes_igO, aes(color = num_viajes, size = num_viajes)) +
  scale_color_gradientn(
    colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
    limits = c(min(flujos$num_viajes), max(flujos$num_viajes))) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(
    title = paste("Mapa de flujos de viajes \n dentro de las localidades"),
    color = "Número de Viajes") +
  theme(legend.position = "right") + guides(size = "none")


# Duracion del viaje en promedio ----
# Crear una lista de líneas entre las coordenadas de origen y destino
duracion_lines <- st_sf(
  origen = flujos$localidad_ori,
  destino = flujos$localidad_des,
  duracion = flujos$duracion_min,
  geometry = mapply(function(x, y) st_sfc(st_linestring(matrix(c(x, y), ncol = 2))), 
                    flujos$geometry_ori, flujos$geometry_des)
)
head(duracion_lines, n=3)

duracion_dist <- duracion_lines %>% filter(origen != destino)
st_crs(duracion_dist) <- 4326

ggplot() +
  geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
  geom_sf(data = duracion_dist, aes(color = duracion)) +
  scale_color_gradientn(
    colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
    limits = c(min(duracion_dist$duracion), max(duracion_dist$duracion))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
  ) +
  scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(
    title = "Mapa de Flujos de Viajes",
    color = "Duración promedio del viaje") +
  theme(legend.position = "right")

### Por cada localidad ----
duracion_graph <- vector("list", length = 20)

#### Desde ----
for (i in 1:length(loca_list)) {
  l  <- loca_list[i]
  duracion_loc <- duracion_dist %>% filter(origen == l)
  st_crs(viajes_loc) <- 4326
  
  p_l <- ggplot() +
    geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
    geom_sf(data = duracion_loc, aes(color = duracion)) +
    scale_color_gradientn(
      colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
      limits = c(min(duracion_dist$duracion), max(duracion_dist$duracion))
    ) +
    scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
    labs(
      title = paste("Mapa de flujos de viajes desde", l, sep = "\n"),
      color = "Duración promedio del viaje") +
    theme(legend.position = "right")
  
  duracion_graph[[i]] <- p_l
}

#Todos los graficos en un gif
imagenes <- list()

for (i in seq_along(duracion_graph)) {
  filename <- paste0("frame_", sprintf("%02d", i), ".png")
  ggsave(filename, plot = duracion_graph[[i]], width = 6, height = 4, dpi = 150)
  
  img <- image_read(filename)
  img <- image_background(img, "white", flatten = TRUE)  # 👈 fondo blanco aquí
  
  imagenes[[i]] <- img
}

gif <- image_animate(image_join(imagenes), delay = 150)  # puedes ajustar los fps
image_write(gif, "dm_desde.gif")

print(gif)

#### Hacia ----
for (i in 1:length(loca_list)) {
  l  <- loca_list[i]
  duracion_loc <- duracion_dist %>% filter(destino == l)
  st_crs(duracion_loc) <- 4326
  
  p_l <- ggplot() +
    geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
    geom_sf(data = duracion_loc, aes(color = duracion)) +
    scale_color_gradientn(
      colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
      limits = c(min(duracion_dist$duracion), max(duracion_dist$duracion))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
    ) +
    scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
    labs(
      title = paste("Mapa de flujos de viajes hacia", l, sep = "\n"),
      color = "Duración promedio del viaje") +
    theme(legend.position = "right")
  
  loca_graph[[i]] <- p_l
}

#Todos los graficos en un gif
imagenes <- list()

for (i in seq_along(loca_graph)) {
  filename <- paste0("frame_", sprintf("%02d", i), ".png")
  ggsave(filename, plot = loca_graph[[i]], width = 6, height = 4, dpi = 150)
  
  img <- image_read(filename)
  img <- image_background(img, "white", flatten = TRUE)  # 👈 fondo blanco aquí
  
  imagenes[[i]] <- img
}

gif <- image_animate(image_join(imagenes), delay = 150)  # puedes ajustar los fps
image_write(gif, "dm_hacia.gif")

print(gif)

#### Dentro ----
duracion_igO <- flujos %>% filter(localidad_ori == localidad_des) %>% ungroup() %>%
  select(localidad_ori, duracion_min, geometry_ori)
duracion_igO <- st_as_sf(duracion_igO, sf_column_name = "geometry_ori", crs = 4326)

ggplot() +
  geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
  geom_sf(data = duracion_igO, aes(color = duracion_min, size = duracion_min)) +
  scale_color_gradientn(
    colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
    limits = c(min(duracion_igO$duracion_min), max(duracion_igO$duracion_min))) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(
    title = paste("Mapa de flujos de viajes \n dentro de las localidades"),
    color = "Duración promedio del viaje") +
  theme(legend.position = "right") + guides(size = "none")

# Numero promedio de etapas ----
# Crear una lista de líneas entre las coordenadas de origen y destino

etapas_lines <- st_sf(
  origen = flujos$localidad_ori,
  destino = flujos$localidad_des,
  etapas = flujos$etapas,
  geometry = mapply(function(x, y) st_sfc(st_linestring(matrix(c(x, y), ncol = 2))), 
                    flujos$geometry_ori, flujos$geometry_des)
)
head(etapas_lines, n=3)

etapas_dist <- etapas_lines %>% filter(origen != destino)
st_crs(etapas_dist) <- 4326

ggplot() +
  geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
  geom_sf(data = etapas_dist, aes(color = etapas)) +
  scale_color_gradientn(
    colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
    limits = c(min(etapas_dist$etapas), max(etapas_dist$etapas))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
  ) +
  scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(
    title = "Mapa de Flujos de Viajes",
    color = "Promedio de etapas \npor viaje") +
  theme(legend.position = "right")

### Por cada localidad ----
etapas_graph <- vector("list", length = 20)

#### Desde ----
for (i in 1:length(loca_list)) {
  l  <- loca_list[i]
  etapas_loc <- etapas_dist %>% filter(origen == l)
  st_crs(etapas_loc) <- 4326
  
  p_l <- ggplot() +
    geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
    geom_sf(data = etapas_loc, aes(color = etapas)) +
    scale_color_gradientn(
      colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
      limits = c(min(etapas_dist$etapas), max(etapas_dist$etapas))
    ) +
    scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
    labs(
      title = paste("Mapa de flujos de viajes desde", l, sep = "\n"),
      color = "Promedio de etapas \npor viaje") +
    theme(legend.position = "right")
  
  etapas_graph[[i]] <- p_l
}

#Todos los graficos en un gif
imagenes <- list()

for (i in seq_along(etapas_graph)) {
  filename <- paste0("frame_", sprintf("%02d", i), ".png")
  ggsave(filename, plot = etapas_graph[[i]], width = 6, height = 4, dpi = 150)
  
  img <- image_read(filename)
  img <- image_background(img, "white", flatten = TRUE)  # 👈 fondo blanco aquí
  
  imagenes[[i]] <- img
}

gif <- image_animate(image_join(imagenes), delay = 150)  # puedes ajustar los fps
image_write(gif, "et_desde.gif")

print(gif)

#### Hacia ----
for (i in 1:length(loca_list)) {
  l  <- loca_list[i]
  etapas_loc <- etapas_dist %>% filter(destino == l)
  st_crs(etapas_loc) <- 4326
  
  p_l <- ggplot() +
    geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
    geom_sf(data = etapas_loc, aes(color = etapas)) +
    scale_color_gradientn(
      colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
      limits = c(min(etapas_dist$etapas), max(etapas_dist$etapas))  # Aseguramos que el gradiente cubra todo el rango de 'num_viajes'
    ) +
    scale_size_continuous(range = c(0.1, 3)) +  # Ajustar el grosor de las líneas
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank()) +
    labs(
      title = paste("Mapa de flujos de viajes hacia", l, sep = "\n"),
      color = "Promedio de etapas \npor viaje") +
    theme(legend.position = "right")
  
  etapas_graph[[i]] <- p_l
}

#Todos los graficos en un gif
imagenes <- list()

for (i in seq_along(etapas_graph)) {
  filename <- paste0("frame_", sprintf("%02d", i), ".png")
  ggsave(filename, plot = etapas_graph[[i]], width = 6, height = 4, dpi = 150)
  
  img <- image_read(filename)
  img <- image_background(img, "white", flatten = TRUE)  # 👈 fondo blanco aquí
  
  imagenes[[i]] <- img
}

gif <- image_animate(image_join(imagenes), delay = 150)  # puedes ajustar los fps
image_write(gif, "et_hacia.gif")

print(gif)

#### Dentro ----
etapas_igO <- flujos %>% filter(localidad_ori == localidad_des) %>% ungroup() %>%
  select(localidad_ori, etapas, geometry_ori)
etapas_igO <- st_as_sf(etapas_igO, sf_column_name = "geometry_ori", crs = 4326)

ggplot() +
  geom_sf(data = loca_shp, fill = 'lightgray', color = "black", size = 0.2) +
  geom_sf(data = etapas_igO, aes(color = etapas, size = etapas)) +
  scale_color_gradientn(
    colors = c("red", "green", "blue"),  # Los colores que deseas en el gradiente
    limits = c(min(etapas_igO$etapas), max(etapas_igO$etapas))) +
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  labs(
    title = paste("Mapa de flujos de viajes \n dentro de las localidades"),
    color = "Promedio de etapas \npor viaje") +
  theme(legend.position = "right") + guides(size = "none")

# Otros ----
df_estrato <- df_viajes %>% filter(nom_mun_hg == "Bogotá") %>% select(cod_hg, estrato = estra_hg, zat = zat_hg)
df_estrato <- df_estrato[!duplicated(df_estrato), ]
zat_shp <- st_read("Datos_Originales/Zonificacion_EODH/ZAT2023/ZAT2023.shp") %>% 
  st_transform(crs = 4326) %>%
  select(zat = ZAT, geometry)

# Total de hogares por ZAT
total_por_zat <- df_estrato %>%
  group_by(zat) %>%
  summarise(total_hogares = n())

# Total por estrato en cada ZAT
porcentaje_estrato <- df_estrato %>%
  group_by(zat, estrato) %>%
  summarise(hogares_estrato = n()) %>%
  left_join(total_por_zat, by = "zat") %>%
  mutate(porcentaje = hogares_estrato / total_hogares * 100)

mapa_datos <- left_join(zat_shp, porcentaje_estrato, by = "zat")

# Lista de estratos únicos
estratos <- unique(porcentaje_estrato$estrato)

# Crear mapas por estrato
mapas_por_estrato <- lapply(estratos, function(e) {
  ggplot(data = mapa_datos %>% filter(estrato == e)) +
    geom_sf(aes(fill = porcentaje), color = NA) +
    scale_fill_viridis_c(option = "C", name = "% Hogares") +
    labs(title = paste("Estrato", e)) +
    theme_minimal()
})

# Usamos patchwork para combinar
mapa_final <- wrap_plots(mapas_por_estrato, ncol = 2) +
  plot_annotation(title = "Distribución porcentual de hogares por estrato y ZAT")
print(mapa_final)
