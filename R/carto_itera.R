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
source("R/quiebres.R")
source("R/elementos_texto.R")
source("R/funcion_global.R")

# COnfig ------------------------------------------------------------------

set_color_variables()
set_outdir('data/res')
set_directorios_insumos()
load_dict()
load_insumos()

lista <- lista_indicadores()

# Ciclo -------------------------------------------------------------------

lista <- c('IAV')
#indicadores_fuente <- c('IEM')

for(indicador in lista ){
  
  cartasPlot()
  #indicador <-  indicadores_fuente[1]

}