#Cargar recursos



# Ruta salida -------------------------------------------------------------

set_outdir <- function(outdir = paste0(ruta_salida, "R",cod_reg, "/cartas")){
  ruta_salida_pdf <<- paste0(outdir,"/pdf")
  ruta_salida_png <<- paste0(outdir, "/png")
  make_dir(path = ruta_salida_pdf)
  make_dir(path = ruta_salida_png)
}


# Carga de Insumos --------------------------------------------------------

#dir_insumos <- paste0("data/ind_calculados/R",cod_reg, "_INDICADORES_MZ.shp")


set_directorios_insumos <- function(
  mz_indicadores = paste0("data/", "R", cod_reg, "/ind_calculados/R",cod_reg, "_INDICADORES.shp"),
  region = "data/insumos_generales/region/Regiones_Chile.shp",
  comunas = "data/insumos_generales/comunas/Comunas_Chile.shp",
  cuerpo_agua = "data/insumos_generales/cuerpo_agua/cuerpo_agua.shp",
  calles = "data/insumos_generales/calles/RED_VIAL_CHILE.shp",
  entidades = paste0("data/insumos_generales/manzanas_entidades/MZ_REGION_", n_reg, ".shp"),
  logos = 'data/insumos_generales/logo/logo.png'
){
  directorios <<- list(mz_indicadores = mz_indicadores,region = region,comunas = comunas,cuerpo_agua = cuerpo_agua,calles = calles,entidades = entidades,logos = logos)
}


load_dict <- function(path = "data/indice/indice_accesibilidad.csv"){
  Diccionario_indicadores <<- read_delim(path, delim = ";") %>%
    filter(itera_grafico == 1) %>%
    mutate(abrev_ind = stringr::str_to_upper(abrev_ind))
  
  set_vars()
}

load_insumos <- function(){
  region <<- read_sf(directorios[["region"]]) %>%
    st_transform(crs_utm) %>%
    filter(REGION == cod_reg)
  
  comunas <<- read_sf(directorios[["comunas"]]) %>%
    st_transform(crs_utm) %>%
    filter(REGION == cod_reg) %>%
    mutate(
      CENTROID = map(geometry, st_centroid),
      COORDS = map(CENTROID, st_coordinates),
      COORDS_X = map_dbl(COORDS, 1),
      COORDS_Y = map_dbl(COORDS, 2)
    )
  
  entidades <<- read_sf(directorios[["entidades"]]) %>%
    st_transform(crs_utm)
  
  cuerpo_agua <<- read_sf(directorios[["cuerpo_agua"]]) %>%
    st_transform(crs_utm) %>% 
    filter(REGION == cod_reg) %>% 
    filter(AREA >= 30000)
  
  calles <<- read_sf(directorios[["calles"]]) %>% 
    st_transform(crs_utm) %>% 
    filter(REGION == cod_reg) %>% 
    filter(CARPETA == "Pavimento"|CARPETA=="Pavimento Básico"|CARPETA=="Pavimento Doble Calzada")
  
  mz_indicadores <<- st_read (directorios[["mz_indicadores"]])%>% st_transform(crs_utm) %>% sf_toupper() %>% fix_insumos()
  
  logos <<- readPNG(directorios[['logos']])
  
  load_dict()
}
