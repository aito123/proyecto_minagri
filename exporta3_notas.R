pp <- raster::rasterToPolygons(myraster)
outline <- sf::st_as_sf(myraster) #no funciona, extraer solo el borde.

e <- as.vector(extent(myraster)) 
  
ee <- bbox(myraster)

e1 = extent(myraster)

outline <- sf::st_as_sf(myraster) %>% st_cast("LINESTRING")

myraster %>% 
  as.data.frame() %>% 
  ggplot() + 
  geom_raster() +
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  theme_void()


ee1<-ee %>% reshape2::melt()

ggplot(ee1) + 
  geom_rect()

#--

st_bbox(myraster) %>% 
  st_as_sfc() %>% 
  ggplot() +
  geom_sf()

# se logró

pacman::p_load(raster, stars, fasterize, knitr, haven, tidyverse, janitor, glue, sf, here, gt, kableExtra, fs, glue)

data_j2<-st_read(
  here("data/data_j2/data_j2.shp"), 
  quiet = TRUE) %>% 
  as.data.frame()

provincias<-sf::st_read(
  here::here("data/QLAB_MIDAGRI_1.gdb"), 
  layer = "Provincias", 
  quiet=TRUE)

# myraster<-stars::read_stars("la_libertad/imagenes/IR/PLANET_IR572-983.tif")
myraster<-raster("la_libertad/imagenes/IR/PLANET_IR572-983.tif")

myraster3<-list(
  fs::dir_ls("la_libertad/imagenes/IR/") %>% 
    walk(
      st_as_sf(st_as_sfc(st_bbox(raster(.x)))) %>% 
      mutate(name="570") %>% 
      rename(geometry=x)  
    )
  
)

#
raster_import<- function(file){
    st_as_sf(st_as_sfc(st_bbox(raster(file)))) %>% 
      mutate(name=sub(".*PLANET_IR *(.*?) *.tif*", "\\1", file)) %>% 
      rename(geometry=x)
}

myraster4<-
  fs::dir_ls("la_libertad/imagenes/IR/") %>% 
  map(raster_import)

p <- ggplot() + geom_sf(data = provincias, size = 0.05, fill = "transparent")
for (i in 1:108) p <- p + 
  geom_sf(data=myraster4[[i]], alpha = 0.5) + 
  geom_sf_text(data = myraster4[[i]], aes(label = name), size = 2) + 
  theme_void()
p

#
myraster2<-list(
  st_as_sf(st_as_sfc(st_bbox(raster("la_libertad/imagenes/IR/PLANET_IR572-983.tif")))) %>% 
    mutate(name="570") %>% 
    rename(geometry=x),
  
  st_as_sf(st_as_sfc(st_bbox(raster("la_libertad/imagenes/IR/PLANET_IR570-982.tif")))) %>% 
    mutate(name="570") %>% 
    rename(geometry=x)
)
  
bound<- 
  st_as_sf(st_as_sfc(st_bbox(myraster))) %>% 
  mutate(name="570") %>% 
  rename(geometry=x)

ggplot() + 
  geom_sf(data = bound) +
  geom_sf_text(data = bound, aes(label = name), size = 2) +
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  theme_void()


p <- ggplot()
for (i in 1:2) p <- p + geom_sf(data=myraster2[[i]])
p + geom_sf(data = provincias, size = 0.05, fill = "transparent") 

# fitdata<-
#   fs::dir_ls("data/Fitabase Data 4.12.16-5.12.16/") %>%
#   map(~ .x %>% import(.)) %>%
#   Map(cbind, ., data_id=basename(names(.))) %>% #sirvió para la data origen
#   map(~.x %>% relocate(data_id, .before = everything()))

# fitdata_all<-
#   fitdata %>%
#   map(~.x %>% clean_names()) %>%
#   reduce(full_join, by=c("id")) # no corre porque la data es muy grande.
#   

for (i in 1:108) p <- p + 
  geom_sf(data=myraster4[[i]], alpha = 0.5) + 
  geom_sf_text(data = myraster4[[i]], aes(label = name), size = 2) + 
  theme_void()

p + geom_sf(
  data=data_j2_loop[[1]], 
  aes(geometry=geometry), 
  fill = "magenta", color = "magenta"
)

for (i in 1:3) p <- p + 
  geom_sf(data=data_j2_loop[[i]], 
          aes(geometry=geometry), 
          fill = "magenta", color = "magenta")

#-

myraster4 %>% 
  mutate(
    intersection=st_intersects(
      geometry,
      data_j2_loop[[3]] %>% st_as_sf(sf_column_name = "geometry")))

unlist(st_intersects(
  myraster4,
  data_j2_loop[[3]] %>% st_as_sf(sf_column_name = "geometry")
)) %>% View()

data.frame(myraster4, inter=lengths(st_intersects(myraster4, data_j2_loop[[3]] %>% st_as_sf(sf_column_name = "geometry"))) > 0)

ggplot() + 
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  
  geom_sf(data=data.frame(myraster4, inter=lengths(st_intersects(myraster4 %>% st_as_sf(sf_column_name = "geometry"), data_j2_loop[[1]] %>% st_as_sf(sf_column_name = "geometry"))) > 0) %>% st_as_sf(sf_column_name = "geometry"), 
          aes(fill=inter), # ese intersect no me parece tan confiable. No se ven los puntos.
          alpha = 0.5) + 
  
  geom_sf_text(data = myraster4, aes(label = name), size = 2) +
  geom_sf(data=data_j2_loop[[1]], 
          aes(geometry=geometry), 
          fill = "magenta", color = "magenta", size = 3) +
  theme_void()


st_sfc(data_j2_loop[[1]] %>% st_as_sf(sf_column_name = "geometry"),
       "+proj=longlat +datum=WGS84 +no_defs")


data_j2_prueba<-data_j2_loop[[3]] %>% st_as_sf(sf_column_name = "geometry")

myraster4_1<- myraster4 %>% st_as_sf(sf_column_name = "geometry")


#

# sf::st_crs(data_j2_prueba)<- sf::st_crs(myraster4_1)

sf::st_crs(myraster4_1)<- sf::st_crs(data_j2_prueba)

ggplot() + 
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  
  geom_sf(data=myraster4_1, 
          aes(fill=inter), # ese intersect no me parece tan confiable. No se ven los puntos.
          alpha = 0.5) + 
  
  geom_sf_text(data = myraster4_1, aes(label = name), size = 2) +
  geom_sf(data=data_j2_prueba, 
          aes(geometry=geometry), 
          fill = "magenta", color = "magenta", size = 3) +
  theme_void()

myraster4_1$inter<-lengths(st_intersects(myraster4_1, data_j2_prueba)) > 0

ggplot(myraster4_1) +
  stat_sf_coordinates()

ggplot(sf::st_zm(myraster4_1)) +
  stat_sf_coordinates()


#

PointIN<- st_as_sf(data.frame(lon=72.930,lat=19.112), coords=c("lon","lat"),crs=4326) 

Pointout<- st_as_sf(data.frame(lon=72.8702,lat=19.112), coords=c("lon","lat"),crs=4326) 

a<- st_as_sf(data.frame(lon=72.930,lat=19.112), coords=c("lon","lat"), crs=4326) 

buf<- st_buffer(a,0.05)   

st_intersects(PointIN, buf) %>% length > 1 

st_intersects(Pointout, buf) %>% length > 1

ggplot() +
  # geom_sf(data=PointIN) +
  # geom_sf(data=Pointout) +
  geom_sf(data=buf, color = "red", size =100) +
  coord_sf(default_crs = sf::st_crs(4326), xlim=c(72.9299996,72.9300004), ylim=c(19.110,19.113))
  
#

st_intersects(myraster4_1, data_j2_prueba) %>% length > 1

myraster4_1$inter<-lengths(st_intersects(myraster4_1, data_j2_prueba)) > 0

ggplot() + 
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  
  geom_sf(data=myraster4_1,
          aes(fill=inter),
          alpha = 0.5) + 
  
  geom_sf_text(data = myraster4_1, aes(label = name), size = 2) +
  




  geom_sf(data=data_j2_prueba, 
          aes(geometry=geometry), 
          fill = "magenta", color = "magenta", size = 3) +
  theme_void() +
  coord_sf(default_crs = sf::st_crs(4326))

ggplot() + 
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  
  geom_sf(data=data_j2_prueba, 
          aes(geometry=geometry), 
          fill = "magenta", color = "magenta", size = 3) +
  theme_void() +
  coord_sf(default_crs = sf::st_crs(4326))


data_j2<-st_read(
  here("data/data_j2/data_j2.shp"), 
  quiet = TRUE) %>% 
  as.data.frame()

provincias<-sf::st_read(
  here::here("data/QLAB_MIDAGRI_1.gdb"), 
  layer = "Provincias", 
  quiet=TRUE)

raster_import<- function(file){
  stars::read_stars(file)
}

myraster4<-
  fs::dir_ls("la_libertad/imagenes/IR/") %>% 
  map(raster_import)

raster_prueba_stars<-
  fs::dir_ls("la_libertad/imagenes/IR/")[17] %>%
  stars::read_stars()

raster_prueba_raster<-
  fs::dir_ls("la_libertad/imagenes/IR/")[17] %>%
  raster()

raster_prueba_terra<-
  fs::dir_ls("la_libertad/imagenes/IR/")[17] %>%
  terra::rast()
#

slot(raster_prueba_raster, "crs")<-CRS(SRS_string = "EPSG:4326") #not working
crs(raster_prueba_raster) <- CRS('+init=EPSG:4326')
crs(raster_prueba_raster) <- CRS('EPSG:4326')

#

st_as_sf(st_as_sfc(st_bbox(raster_prueba), crs=4326)) %>% ggplot() + geom_sf()

st_as_sf(st_as_sfc(st_bbox(raster_prueba))) %>% ggplot() + geom_sf()

# elementos de un problema: provincias shp, raster cuadrante, crops shp

st_crs(provincias) # EPSG 4326

st_crs(raster_prueba_raster) # EPSG 9122 with raster package

crs(r) <- CRS('+init=EPSG:4326')

raster::crs(raster_prueba_raster) <- "EPSG:4326" # sirvió

st_crs(4326)

crs(raster_prueba_raster)<-st_crs(4326)


st_crs(raster_prueba_stars) # EPSG 4326 with stars package
# raster_prueba2<-st_set_crs(raster_prueba, "EPSG:4326")

st_crs(raster_prueba_terra) # EPSG 4326

st_crs(data_j2_prueba) # EPSG 4326

ggplot() +
  geom_sf(data = provincias, fill = "transparent") + # la vaina es solida ps
  geom_stars(data = raster_prueba_stars) + #demora la cuestion
  geom_sf(data = data_j2_prueba, color = "magenta", size = 2) +
  coord_sf(default_crs = sf::st_crs(4326), crs = sf::st_crs(4326)) # parece no tener ningun efecto.
  

# todos en el mismo EPSG, no hay warnings.

ggplot() +
  geom_raster(data = raster::as.data.frame(raster_prueba_raster, xy=TRUE), aes(x = x, y = y, fill = layer)) + #funciona imprime RasterLayer, tal vez crear este objeto
  geom_sf(data = provincias, fill = "transparent") +
  geom_sf(data = data_j2_prueba, color = "magenta", size = 2)

# con terra
ggplot() +
  tidyterra::geom_spatraster(data = raster_prueba_terra) +
  geom_sf(data = provincias, fill = "transparent") +
  geom_sf(data = data_j2_prueba, color = "magenta", size = 2)



# con stars loop, muy grande la cuestion

for (i in 1:108) p <- p + 
  geom_stars(data=myraster4[[i]], alpha = 0.5) + 
  # geom_sf_text(data = myraster4[[i]], aes(label = name), size = 2) + 
  theme_void()

p + geom_sf(
  data=data_j2_loop[[1]], 
  aes(geometry=geometry), 
  fill = "magenta", color = "magenta"
)

for (i in 1:3) p <- p + 
  geom_sf(data=data_j2_loop[[i]], 
          aes(geometry=geometry), 
          fill = "magenta", color = "magenta")


#

#antiguo
raster_import<- function(file){
  st_as_sf(st_as_sfc(st_bbox(raster(file)))) %>% 
    mutate(name=sub(".*PLANET_IR *(.*?) *.tif*", "\\1", file)) %>% 
    rename(geometry=x)
}

#nuevo
raster_import<- function(file){
  st_as_sf(st_as_sfc(st_bbox(read_stars(file, crs = 4326)))) %>% 
    mutate(name=sub(".*PLANET_IR *(.*?) *.tif*", "\\1", file)) %>% 
    rename(geometry=x)
}

myraster4<-
  fs::dir_ls("la_libertad/imagenes/IR/") %>% 
  map(raster_import) %>% 
  reduce(bind_rows)

myraster4$within <- st_within(myraster4, data_j2 %>% st_as_sf(sf_column_name = "geometry")) %>% lengths > 0 #no funcionó

myraster4_1 <- 
  myraster4 %>% 
  mutate(
    within=st_contains(geometry, data_j2 %>% filter(P403_MES %in% "01")) %>% lengths > 0
  )

myraster4_1 <- 
  myraster4 %>% 
  mutate(
    within=st_overlaps(geometry, st_union(data_j2 %>% filter(P403_MES %in% "01"))) %>% lengths > 0
  )

table(myraster4_1$within)

myraster4_1 <- 
  myraster4 %>% 
  mutate(
    within=st_covers(geometry, data_j2 %>% filter(P403_MES %in% "01")) %>% lengths > 0
  )
  
intersection<-st_intersection(data_j2 %>% filter(P403_MES %in% "01"), myraster4)

ggplot() + 
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  geom_sf(data=myraster4_1, alpha = 0.5, aes(fill=within)) + 
  geom_sf(data=data_j2 %>% filter(P403_MES %in% "01"), 
          aes(geometry=geometry),
          fill = "magenta", color = "magenta", size = 10) +
  # geom_sf_text(data = myraster4, aes(label = name), size = 2) + #el error en etiquetas
  theme_void() + 
  coord_sf(default_crs = sf::st_crs(4326), crs = sf::st_crs(4326))

(
ggplot() + 
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  geom_sf(data=myraster4 %>% filter(name %in% intersection$name), alpha = 0.5) + 
  geom_sf(data=data_j2 %>% filter(P403_MES %in% "01"), 
          aes(geometry=geometry),
          fill = "magenta", color = "magenta", size = 2) +
  # geom_sf_text(data = myraster4, aes(label = name), size = 2) + #el error en etiquetas
  theme_void() + 
  coord_sf(default_crs = sf::st_crs(4326), crs = sf::st_crs(4326))
) %>% 
  ggsave("data/plots/plot3.png", ., device = "png")

multiplots<-function(num){
  
  myraster5 <- 
    myraster4 %>% 
    mutate(
      within=st_contains(
        geometry, 
        data_j2_loop[[num]]
      ) %>% 
        lengths > 0
    )
  
  ggplot() + 
    geom_sf(data = provincias, size = 0.05, fill = "transparent") +
    geom_sf(data=myraster5, aes(fill=within), alpha = 0.5) + 
    geom_sf_text(data = myraster5, aes(label = name), size = 2) +
    geom_sf(data=data_j2_loop[[num]], 
            aes(geometry=geometry), 
            fill = "magenta", color = "magenta", size = 2) +
    theme_void() + 
    coord_sf(default_crs = sf::st_crs(4326), crs = sf::st_crs(4326))
}

c(1:3) %>% 
  map(multiplots)

# si estaba bien lo que estaba haciendo era un problema del dpi la resolución que no permitia visualizar ptrme ver ese tema de scale en un ggplot y 
# agrandar etiquetas mandar reunion con pedro, practicantes y cerrar mañana.

###

#plots con raster borders
p <- 
  ggplot() + 
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  geom_sf(data=myraster4, alpha = 0.5) + 
  geom_sf_text(data = myraster4, aes(label = name), size = 2) + 
  theme_void()

```{r}

(
  ggplot() + 
    geom_sf(data = provincias, size = 0.05, fill = "transparent") +
    geom_sf(data=myraster4, alpha = 0.5) + 
    geom_sf(data=data_j2 %>% filter(P403_MES %in% "01"), 
            aes(geometry=geometry),
            fill = "magenta", color = "magenta", size = 2) +
    # geom_sf_text(data = myraster4, aes(label = name), size = 2) + #el error en etiquetas
    theme_void() + 
    coord_sf(default_crs = sf::st_crs(4326), crs = sf::st_crs(4326))
) %>% 
  ggsave("data/plots/plot.png", . , width = 9180, height = 7580, units = "px")

knitr::include_graphics("data/plots/plot.png")
```

```{r plot-white, fig.width = 25, fig.height = 23.63}

knitr::opts_chunk$set(fig.retina = 1)
library(ragg)
library(pdftools)

intersection<-st_intersection(data_j2 %>% filter(P403_MES %in% "01"), myraster4)

p<-
  ggplot() + 
  geom_sf(data = provincias, size = 0.05, fill = "transparent") +
  geom_sf(data=myraster4 %>% filter(name %in% intersection$name), alpha = 0.5) + 
  geom_sf(data=data_j2 %>% filter(P403_MES %in% "01"), 
          aes(geometry=geometry),
          fill = "magenta", color = "magenta", size = 2) +
  # geom_sf_text(data = myraster4, aes(label = name), size = 2) + #el error en etiquetas
  theme_void() + 
  coord_sf(default_crs = sf::st_crs(4326), crs = sf::st_crs(4326))

p

# ggsave(here::here("data", "plots", "plot2.pdf"), 
#       width = 25, height = 23.63, device = cairo_pdf)
# 
# pdf_convert(pdf = here::here("data", "plots", "plot1.pdf"),
#             filenames = here::here("data", "plots", "plot4.png"),
#             pages = 1,
#             format = "png", dpi = 120)

knitr::include_graphics("data/plots/plot4.png")

```

####

cities <- 
  st_read(system.file("vectors/cities.shp", package = "rgdal"))

cities


#----
vector_filepath = system.file("shapes/world.gpkg", package = "spData")
new_vector = read_sf(vector_filepath)
st_crs(new_vector)
new_vector = st_set_crs(new_vector, "EPSG:4326") # set CRS

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = terra::rast(raster_filepath)
cat(crs(my_rast))

crs(my_rast) = "EPSG:26912" # set CRS, si lo cambia.












