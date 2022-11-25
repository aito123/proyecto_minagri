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