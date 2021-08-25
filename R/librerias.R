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
