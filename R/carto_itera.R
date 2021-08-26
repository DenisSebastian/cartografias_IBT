# Opciones Generales ------------------------------------------------------

options(scipen = 999, stringsAsFactors=F, encoding = "UTF-8", warn=-1)
options("rgdal_show_exportToProj4_warnings"="none")
options(OutDec= ",")

# Definción de Región de Estudio ------------------------------------------


n_reg <- 11
cod_reg <- sprintf("%02d", n_reg)
d_reg <- sprintf("%02d", n_reg)


# Cargar Recursos ---------------------------------------------------------

source("R/librerias.R")
source("R/funciones.R")
source("R/temas_cartografias.R")
source("R/load_files.R")
source("R/parametros.R")
source("R/mapa_base.R")
source("R/composicion.R")
source("R/bbox.R")

# COnfig ------------------------------------------------------------------

set_params()
set_outdir()
set_path_insumos()
load_insumos()
load_dict()
set_vars()

indicadores <- fix_insumos(insumos_acc)

lista <- lista_indicadores()

# Ciclo -------------------------------------------------------------------

lista <- c('IAV')
#indicadores_fuente <- c('IEM')

for(indicador in lista ){
  
  #indicador <-  indicadores_fuente[1]

set.seed(1) #Incorporacion de semilla


print(paste0("Procesando: ", indicador))


# filtro de indicador -----------------------------------------------------

insumo <- indicadores %>%
  dplyr::select(all_of(indicador))

# definicón de nombres ----------------------------------------------------

var_ind <- Diccionario_indicadores %>%
  filter(abrev_ind==indicador)

titulo <- make_title(element_1 = var_ind$nombre_grafico, element_2 =  region$NOM_REGION, nchar_max = 50)
subtitulo <- var_ind$unidad
archivo <- var_ind$archivo
colorStr <- as.character(Diccionario_indicadores[Diccionario_indicadores$abrev_ind == indicador,
                                                 colnames(Diccionario_indicadores) == "colores"])
ind_pal <- colors_str_list(colorStr) %>% unlist()

# crear variables con los quiebres ---------------------------------------

vector_indicador <- insumo %>%
  st_drop_geometry() %>%
  pull()

if(indicador!="CLUSTERM"){
  vector_indicador <- round(vector_indicador,2)
} 

set_breaks()

# Repel y bbox ------------------------------------------------------------

# bbox

set_bbox_region(region)


# Mapa --------------------------------------------------------------------

if(var_ind$grupo==1){
  mapa_base <- mapa_base_g1()
} else if (var_ind$grupo==2)  {
  mapa_base <- mapa_base_g2()
} else { 
  mapa_base <- mapa_base_otros()
}

# Composicion -------------------------------------------------------------

composicion(mapa_base)
}
