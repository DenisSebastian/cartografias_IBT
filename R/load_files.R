#Cargar recursos

# Ruta salida -------------------------------------------------------------

ruta_salida <- "data/"
ruta_salida_pdf <- paste0(ruta_salida, "R",cod_reg, "/cartas", "/pdf")
ruta_salida_png <- paste0(ruta_salida, "R",cod_reg, "/cartas", "/png")
make_dir(path = ruta_salida_pdf)
make_dir(path = ruta_salida_png)


# Carga de Insumos --------------------------------------------------------


#dir_insumos <- paste0("data/ind_calculados/R",cod_reg, "_INDICADORES_MZ.shp")
dir_insumos <- paste0("data/", "R", cod_reg, "/ind_calculados/R",cod_reg, "_INDICADORES.shp")
dir_region <- "data/insumos_generales/region/Regiones_Chile.shp"
dir_comunas <- "data/insumos_generales/comunas/Comunas_Chile.shp"
dir_cuerpo_agua <- "data/insumos_generales/cuerpo_agua/cuerpo_agua.shp"
dir_calles <- "data/insumos_generales/calles/RED_VIAL_CHILE.shp"
dir_entidades <- paste0("data/insumos_generales/manzanas_entidades/MZ_REGION_", n_reg, ".shp")


Diccionario_indicadores <- read_delim("data/indice/indice_accesibilidad.csv", delim = ";") %>%
  filter(itera_grafico == 1) %>%
  mutate(abrev_ind = stringr::str_to_upper(abrev_ind))


region <- read_sf(dir_region) %>%
  st_transform(crs_utm) %>%
  filter(REGION == cod_reg)

comunas <- read_sf(dir_comunas) %>%
  st_transform(crs_utm) %>%
  filter(REGION == cod_reg)

entidades <- read_sf(dir_entidades) %>%
  st_transform(crs_utm)

cuerpo_agua <- read_sf(dir_cuerpo_agua) %>%
  st_transform(crs_utm) %>% 
  filter(REGION == cod_reg) %>% 
  filter(AREA >= 30000)

calles <- read_sf(dir_calles) %>% 
  st_transform(crs_utm) %>% 
  filter(REGION == cod_reg) %>% 
  filter(CARPETA == "Pavimento"|CARPETA=="Pavimento Básico"|CARPETA=="Pavimento Doble Calzada")

insumos_acc <-   st_read (dir_insumos)%>%
  st_transform(crs_utm)
# Insumo a mayuscula
insumos_acc <- sf_toupper(insumos_acc)
# Logos

logos <- readPNG('data/insumos_generales/logo/logo.png')


