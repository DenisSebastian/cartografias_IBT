# Opciones Generales ------------------------------------------------------

options(scipen = 999, stringsAsFactors=F, encoding = "UTF-8", warn=-1)
options("rgdal_show_exportToProj4_warnings"="none")
options(OutDec= ",")

# Definción de Región de Estudio ------------------------------------------


n_reg <- 11
cod_reg <- sprintf("%02d", n_reg)
d_reg <- sprintf("%02d", n_reg)


# Cargar Recursos ---------------------------------------------------------

source("R/cartasplot.R")

# COnfig ------------------------------------------------------------------

set_outdir('data/res')
set_directorios_insumos()
load_dict()
load_insumos()
set_color_variables()


cartasPlot('IAV')


# Ciclo -------------------------------------------------------------------

for(indicador in lista_indicadores() ){
  cartasPlot(indicador)
}