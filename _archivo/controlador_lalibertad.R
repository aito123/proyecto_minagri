render_fun <- function(raster_list){
  quarto::quarto_render(
    input = "la_libertad.qmd",
    execute_params = list(raster = raster_list)#,
   # output_file = NULL
  )
}

fs::dir_ls("la_libertad/imagenes/", regexp = "IR") %>%
# "la_libertad/imagenes/PLANET_IR572-983.tif" %>% 
  purrr::walk(render_fun)
