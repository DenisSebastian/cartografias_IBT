#librerias
library(rosm)
library(ggplot2)
library(sf)
library(dplyr)
library(grid)
library(png)
library(gridExtra)
library(ggspatial)
library(ggpattern)
library(classInt)
library(readxl)
library(tictoc)
library(scales)
library(readr)
library(stringr)
library(tidyverse)
library(cowplot)
library(ggrepel)
#remotes::install_github("coolbutuseless/ggpattern", force = TRUE)

register_tile_source(light ="http://a.basemaps.cartocdn.com/light_nolabels/${z}/${x}/${y}.png")

crs_utm <- 32719
crs_latlon <- 4326

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

cartasPlot <- function(indicador) {
  
  set.seed(1) #Incorporacion de semilla
  
  print(paste0("Procesando: ", indicador))
  
  # filtro de indicador -----------------------------------------------------
  
  indicador_seleccionado <<- mz_indicadores %>%
    dplyr::select(all_of(indicador))
  
  # definicón de nombres ----------------------------------------------------
  
  set_text_elements(indicador)
  
  # crear variables con los quiebres ---------------------------------------
  
  set_breaks(indicador)
  
  # Repel y bbox ------------------------------------------------------------
  
  # bbox
  
  set_bbox_region(region)
  
  # Mapa --------------------------------------------------------------------
  
  if(var_ind$grupo==1){
    mapa_base <- mapa_base_g1(indicador)
  } else if (var_ind$grupo==2)  {
    mapa_base <- mapa_base_g2(indicador)
  } else { 
    mapa_base <- mapa_base_otros(indicador)
  }
  
  # Composicion -------------------------------------------------------------
  
  composicion(mapa_base)
  
}
