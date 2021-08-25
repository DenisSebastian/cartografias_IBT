
# Opciones Generales ------------------------------------------------------

options(scipen = 999, stringsAsFactors=F, encoding = "UTF-8", warn=-1)
options("rgdal_show_exportToProj4_warnings"="none")
options(OutDec= ",")

# Definción de Región de Estudio ------------------------------------------

n_reg <- 6
cod_reg <- sprintf("%02d", n_reg)
d_reg <- sprintf("%02d", n_reg)


# Cargar Recursos ---------------------------------------------------------

source("R/librerias.R")
source("R/funciones.R")
source("R/temas_cartografias.R")
source("R/load_files.R")
source("R/parametros.R")


# Ciclo -------------------------------------------------------------------

 indicadores_fuente <- c('IEM')

for(indicador in indicadores_fuente){
  
  #indicador <-  indicadores_fuente[1]

set.seed(1) #Incorporacion de semilla

tic()

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

if(indicador=="IEJ"){
  insumo <-
    insumo %>%
    mutate(class = cut(insumo$IEJ,
                       breaks=c(0, 1, 4, 8, 12 ,17, 20),
                       labels=c("0", "1 - 4", "5 - 8", "9 - 12", "13 - 17", ">17"
                       ), include.lowest = TRUE))
  
  breaks=c(0, 1, 4, 8, 12 ,17, 20)
  
  
} else if (indicador=="CLUSTERM"){
  # Zonas IBT ---------------------------------------------------------------
  insumo$CLUSTERM[insumo$CLUSTERM=="HH"] <-"Zona IBT Alto"
  insumo$CLUSTERM[insumo$CLUSTERM=="NS"] <-"Zona IBT Medio"
  insumo$CLUSTERM[insumo$CLUSTERM=="LL"] <-"Zona IBT Bajo"
  colnames(insumo)[colnames(insumo)== "CLUSTERM"] <- "ZONAS_IBT"
  insumo <-
    insumo %>%
    mutate(class = insumo$ZONAS_IBT)
  insumo$class <- factor(insumo$class, levels = c("Zona IBT Bajo", "Zona IBT Medio", "Zona IBT Alto"))
  indicador <- "ZONAS_IBT"
  subtitulo <-  "Zonas IBT"
  
 } else  {
   
   breaksJenks <- classIntervals(na.omit(vector_indicador),
                                 n = length (ind_pal),
                                 style = "fisher")$brks
  
   breaksJenks <- round(breaksJenks,2)
   

  breaks <- breaksJenks
  labs <- gen_labs(insumo$class, breaks = breaksJenks, dig_dif = 0.01) %>% gsub("\\.",",",.)
  
  insumo <-
     insumo %>%
     mutate(class = cut((vector_indicador),
                      breaks = breaks,
                      labels = labs, include.lowest=TRUE))

}


# Repel y bbox ------------------------------------------------------------

comunas <- comunas %>%  
  mutate(
    CENTROID = map(geometry, st_centroid),
    COORDS = map(CENTROID, st_coordinates),
    COORDS_X = map_dbl(COORDS, 1),
    COORDS_Y = map_dbl(COORDS, 2)
  )

# bbox

bbox_new <- st_bbox(region)# current bounding box

xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values

bbox_new[1] <- bbox_new[1] - (0.2 * xrange) # xmin - left, hacia la izquierda
# bbox_new[3] <- bbox_new[3] + (0.5 * xrange) # xmax - right, hacia la derecha
# bbox_new[2] <- bbox_new[2] - (0.5 * yrange) # ymin - bottom, hacia abajo
# bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top, hacia arriba

bbox_new <- bbox_new %>%  # take the bounding box and make it a sf polygon
  st_as_sfc() %>% 
  st_transform(crs_utm)


# Mapa --------------------------------------------------------------------

if(var_ind$grupo==1){
# Manzanas con achurado ---------------------------------------------------

  insumo <-
    insumo %>%
    mutate(class_a = cut((vector_indicador),
                         breaks = breaks,
                         labels=c("Muy Bajo", "Bajo", "Medio Bajo","Medio Alto", "Alto", "Muy Alto"),
                         include.lowest = TRUE ))
  
  insumo <-
    insumo %>%
    mutate(class_b = str_c(class," (", class_a, ")"))
  
  insumo$class_b <- as.factor(insumo$class_b)
  
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[6]," (", levels(insumo$class_a)[6], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[5]," (", levels(insumo$class_a)[5], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[4]," (", levels(insumo$class_a)[4], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[3]," (", levels(insumo$class_a)[3], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[2]," (", levels(insumo$class_a)[2], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[1]," (", levels(insumo$class_a)[1], ")"))
  
  
pobRelevante <- pobRelevante_f(indicador)
sin_pob <- indicadores %>%
  as.data.frame() %>%
  dplyr::filter(!!sym(pobRelevante) == 0) %>%
  st_as_sf() %>%
  st_transform(crs_utm)


mapa_base <-
  ggplot(bbox_new)+
  #Mapa Base
  annotation_map_tile(type = tipo_map, zoomin = 1)+
  #Capas
  geom_sf(data = region,  aes(colour = col_reg), fill = "white", size = 0.8, show.legend = FALSE)+
  geom_sf(data = sin_pob, aes(colour="A"), fill = col_pob, size = 0.3)+
  geom_sf(data = insumo, aes(fill = class_b), alpha = 0.6,colour=NA) +
  geom_sf(data = calles, aes(), color= col_calles, alpha= 0.6, size =0.6,show.legend = FALSE) +
  geom_sf(data = cuerpo_agua, aes(), fill=col_agua, color = NA, show.legend = FALSE)+
  geom_sf(data = comunas, aes(colour=col_com), fill = NA, size = 0.3,show.legend = FALSE) +
  #Limites bbox
  coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
           ylim = st_coordinates(bbox_new)[c(2,3),2]) +
  #Paleta colores indicador
  scale_fill_manual(
    name =  ifelse(indicador == "ZONAS_IBT", "ZONAS IBT", paste0(indicador, "\n(", subtitulo,")")),
    values = ind_pal, na.translate=FALSE)+
  #Paleta color sin poblacion
  scale_color_manual(
    name = c(""),
    values = c("A" = col_pob),
    labels = c(name_sin(indicador)))+
  #Guias Leyenda
  guides(size=guide_legend(direction = "vertical"),
         fill=guide_legend(reverse = TRUE, order = 1),
         colour=guide_legend(order = 2),
         pattern=guide_legend(order=3))+
  #Nombre comunas
  geom_text_repel(data = comunas, aes(x = COORDS_X, y = COORDS_Y, label = NOM_COMUNA, family = my_font)
                  , size = 3, max.overlaps = Inf)+
  #Elementos carta
  annotation_north_arrow(location="tr", rotation = NULL, height = unit(1,'cm'),
                         style = north_arrow_fancy_orienteering())+
  annotation_scale(plot_unit = "m", location = 'bl', height = unit(0.3, "cm"), style ='ticks') +
  labs(title = titulo, subtitle = subtitulo) +
  tema_insumos
#mapa_base

} else if (var_ind$grupo==2)  {

    
  insumo <-
    insumo %>%
    mutate(class_a = cut((vector_indicador),
                         breaks = breaks,
                         labels=c("Muy Bajo", "Bajo", "Medio Bajo","Medio Alto", "Alto", "Muy Alto"),
                         include.lowest = TRUE ))
  
  insumo <-
    insumo %>%
    mutate(class_b = str_c(class," (", class_a, ")"))
  
  insumo$class_b <- as.factor(insumo$class_b)
  
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[6]," (", levels(insumo$class_a)[6], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[5]," (", levels(insumo$class_a)[5], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[4]," (", levels(insumo$class_a)[4], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[3]," (", levels(insumo$class_a)[3], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[2]," (", levels(insumo$class_a)[2], ")"))
  insumo$class_b <- relevel(insumo$class_b, paste0(levels(insumo$class)[1]," (", levels(insumo$class_a)[1], ")"))
  IEJ
  mapa_base <-
    ggplot(bbox_new)+
    #Mapa Base
    annotation_map_tile(type = tipo_map, zoomin = 1)+
    #Capas
    geom_sf(data = region,  aes(colour = col_reg), fill = "white", size = 0.8, show.legend = FALSE)+
    #geom_sf(data = sin_pob, aes(colour="A"), fill = col_pob, size = 0.3)+
    geom_sf(data = insumo, aes(fill = class_b), alpha = .6,colour=NA) +
    geom_sf(data=calles, aes(), color= col_calles, alpha= 0.6, size =0.6,show.legend = FALSE) +
    geom_sf(data = cuerpo_agua, aes(), fill=col_agua, color = NA, show.legend = FALSE)+
    geom_sf(data = comunas, aes(colour=col_com), fill = NA, size = 0.3,show.legend = FALSE) +
    #Limites bbox
    coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
             ylim = st_coordinates(bbox_new)[c(2,3),2]) +
    #Paleta colores indicador
    scale_fill_manual(
      name =  ifelse(indicador == "ZONAS_IBT", "ZONAS IBT", paste0(indicador, "\n(", subtitulo,")")),
      values = ind_pal, na.translate=FALSE)+
    #Paleta color sin poblacion
    scale_color_manual(
      name = c(""),
      values = c("A" = col_pob),
      labels = c(name_sin(indicador)))+
    #Guias Leyenda
    guides(size=guide_legend(direction = "vertical"),
           fill=guide_legend(reverse = TRUE, order = 1),
           colour=guide_legend(order = 2),
           pattern=guide_legend(order=3))+
    #Nombre comunas
    geom_text_repel(data = comunas, aes(x = COORDS_X, y = COORDS_Y, label = NOM_COMUNA, family = my_font)
                    , size = 3, max.overlaps = Inf)+
    #Elementos carta
    annotation_north_arrow(location="tr", rotation = NULL, height = unit(1,'cm'),
                           style = north_arrow_fancy_orienteering())+
    annotation_scale(plot_unit = "m", location = 'bl', height = unit(0.3, "cm"), style ='ticks') +
    labs(title = titulo, subtitle = subtitulo) +
    tema_insumos
  #mapa_base  
  
  
} else { 
  
  
  mapa_base <-
    ggplot(bbox_new)+
    annotation_map_tile(type = tipo_map, zoomin = 1)+
    #Capas
    geom_sf(data = region,  aes(colour = col_reg), fill = "white", size = 0.8, show.legend = FALSE)+
    geom_sf(data = insumo, aes(fill = class), alpha = .6,colour=NA) +
    geom_sf(data=calles, aes(), color= col_calles, alpha= 0.6, size =0.6,show.legend = FALSE) +
    geom_sf(data = cuerpo_agua, aes(), fill=col_agua, color = NA, show.legend = FALSE)+
    geom_sf(data = comunas, aes(colour=col_com), fill = NA, size = 0.3,show.legend = FALSE) +
     #Limites bbox
    coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
             ylim = st_coordinates(bbox_new)[c(2,3),2]) +
    #Paleta color sin poblacion
    scale_color_manual(
      name = c(""),
      values = c("A" = col_pob),
      labels = c(name_sin(indicador)))+
    #Paleta colores indicador
    scale_fill_manual(
      name =  ifelse(indicador == "ZONAS_IBT", "ZONAS IBT", paste0(indicador, "\n(", subtitulo,")")),
      values = ind_pal, na.translate=FALSE,
      guide=guide_legend(reverse=TRUE))+
    #Nombre comunas
    geom_text_repel(data = comunas, aes(x = COORDS_X, y = COORDS_Y, label = NOM_COMUNA, family = my_font)
                    , size = 3, max.overlaps = Inf)+
    #Elementos carta
    annotation_north_arrow(location="tr", rotation = NULL, height = unit(1,'cm'),
                           style = north_arrow_fancy_orienteering())+
    annotation_scale(plot_unit = "m", location = 'bl', height = unit(0.3, "cm"), style ='ticks') +
    labs(title = titulo, subtitle = subtitulo) +
    tema_insumos
  #mapa_base  
 
}

# Composicion -------------------------------------------------------------

logos_grob <- grobTree(rasterGrob(logos, x=0.9, hjust=1))
mapa_insumo <- grid.arrange(grob(), # margen
                            logos_grob,# logos
                            mapa_base, #plot
                            heights = c(unit(1,'cm'), # altura margen
                                        unit(0.7,'cm'), # tamano logos
                                        unit(25,'cm'))) # altura plot

print(paste0("Guardando Mapa de Indicador: ", indicador))
   
reg_alargada <- c(11, 12, 2)
width_page <-  ifelse(n_reg %in% reg_alargada , 29.7, 42)
height_page <-  ifelse(n_reg %in% reg_alargada, 42, 29.7)



# ggsave(paste0(ruta_salida_png,"/",  archivo,"_",stringr::str_to_lower(indicador),"_",cod_reg, '.png'),
       # mapa_insumo, width = width_page, height = height_page, units = 'cm', dpi = 300,limitsize = F)

ggsave(paste0("data/res/",  archivo,"_",stringr::str_to_lower(indicador),"_",cod_reg, '.png'),
       mapa_insumo, width = width_page, height = height_page, units = 'cm', dpi = 300,limitsize = F)
# ggsave(paste0(ruta_salida_pdf, "/", archivo,"_", stringr::str_to_lower(indicador),"_",cod_reg, '.pdf'),
#        mapa_insumo, width = width_page, height = height_page, units = 'cm', dpi = 300,limitsize = F)

toc()

}



