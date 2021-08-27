mapa_base_g1 <- function(indicador){
  # Manzanas con achurado ---------------------------------------------------
  
  insumo <-
    indicador_seleccionado %>%
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
  sin_pob <- mz_indicadores %>%
    as.data.frame() %>%
    dplyr::filter(!!sym(pobRelevante) == 0) %>%
    st_as_sf() %>%
    st_transform(crs_utm)
  
  
  mapa_base <-
    ggplot(bbox_new)+
    #Mapa Base
    annotation_map_tile(type = params[['tipo_map']], zoomin = 1)+
    #Capas
    geom_sf(data = region,  aes(colour = params[['col_reg']]), fill = "white", size = 0.8, show.legend = FALSE)+
    geom_sf(data = sin_pob, aes(colour="A"), fill = params[['col_pob']], size = 0.3)+
    geom_sf(data = insumo, aes(fill = class_b), alpha = 0.6,colour=NA) +
    geom_sf(data = calles, aes(), color= params[['col_calles']], alpha= 0.6, size =0.6,show.legend = FALSE) +
    geom_sf(data = cuerpo_agua, aes(), fill=params[['col_agua']], color = NA, show.legend = FALSE)+
    geom_sf(data = comunas, aes(colour=params[['col_com']]), fill = NA, size = 0.3,show.legend = FALSE) +
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
      values = c("A" = params[['col_pob']]),
      labels = c(name_sin(indicador)))+
    #Guias Leyenda
    guides(size=guide_legend(direction = "vertical"),
           fill=guide_legend(reverse = TRUE, order = 1),
           colour=guide_legend(order = 2),
           pattern=guide_legend(order=3))+
    #Nombre comunas
    geom_text_repel(data = comunas, aes(x = COORDS_X, y = COORDS_Y, label = NOM_COMUNA, family = params[['my_font']])
                    , size = 3, max.overlaps = Inf)+
    #Elementos carta
    annotation_north_arrow(location="tr", rotation = NULL, height = unit(1,'cm'),
                           style = north_arrow_fancy_orienteering())+
    annotation_scale(plot_unit = "m", location = 'bl', height = unit(0.3, "cm"), style ='ticks') +
    labs(title = titulo, subtitle = subtitulo) +
    tema_insumos
  
  return(mapa_base)
}

mapa_base_g2 <- function(indicador)
{
  insumo <-
    indicador_seleccionado %>%
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
  
  mapa_base <-
    ggplot(bbox_new)+
    #Mapa Base
    annotation_map_tile(type = params[['tipo_map']], zoomin = 1)+
    #Capas
    geom_sf(data = region,  aes(colour = params[['col_reg']]), fill = "white", size = 0.8, show.legend = FALSE)+
    #geom_sf(data = sin_pob, aes(colour="A"), fill = params[['col_pob']], size = 0.3)+
    geom_sf(data = insumo, aes(fill = class_b), alpha = .6,colour=NA) +
    geom_sf(data=calles, aes(), color= params[['col_calles']], alpha= 0.6, size =0.6,show.legend = FALSE) +
    geom_sf(data = cuerpo_agua, aes(), fill=params[['col_agua']], color = NA, show.legend = FALSE)+
    geom_sf(data = comunas, aes(colour=params[['col_com']]), fill = NA, size = 0.3,show.legend = FALSE) +
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
      values = c("A" = params[['col_pob']]),
      labels = c(name_sin(indicador)))+
    #Guias Leyenda
    guides(size=guide_legend(direction = "vertical"),
           fill=guide_legend(reverse = TRUE, order = 1),
           colour=guide_legend(order = 2),
           pattern=guide_legend(order=3))+
    #Nombre comunas
    geom_text_repel(data = comunas, aes(x = COORDS_X, y = COORDS_Y, label = NOM_COMUNA, family = params[['my_font']])
                    , size = 3, max.overlaps = Inf)+
    #Elementos carta
    annotation_north_arrow(location="tr", rotation = NULL, height = unit(1,'cm'),
                           style = north_arrow_fancy_orienteering())+
    annotation_scale(plot_unit = "m", location = 'bl', height = unit(0.3, "cm"), style ='ticks') +
    labs(title = titulo, subtitle = subtitulo) +
    tema_insumos
  
 return(mapa_base) 
  
}

mapa_base_otros <- function(indicador) { 
  mapa_base <-
    ggplot(bbox_new)+
    annotation_map_tile(type = params[['tipo_map']], zoomin = 1)+
    #Capas
    geom_sf(data = region,  aes(colour = params[['col_reg']]), fill = "white", size = 0.8, show.legend = FALSE)+
    geom_sf(data = indicador_seleccionado, aes(fill = class), alpha = .6,colour=NA) +
    geom_sf(data=calles, aes(), color= params[['col_calles']], alpha= 0.6, size =0.6,show.legend = FALSE) +
    geom_sf(data = cuerpo_agua, aes(), fill=params[['col_agua']], color = NA, show.legend = FALSE)+
    geom_sf(data = comunas, aes(colour=params[['col_com']]), fill = NA, size = 0.3,show.legend = FALSE) +
    #Limites bbox
    coord_sf(xlim = st_coordinates(bbox_new)[c(1,2),1], # min & max of x values
             ylim = st_coordinates(bbox_new)[c(2,3),2]) +
    #Paleta color sin poblacion
    scale_color_manual(
      name = c(""),
      values = c("A" = params[['col_pob']]),
      labels = c(name_sin(indicador)))+
    #Paleta colores indicador
    scale_fill_manual(
      name =  ifelse(indicador == "ZONAS_IBT", "ZONAS IBT", paste0(indicador, "\n(", subtitulo,")")),
      values = ind_pal, na.translate=FALSE,
      guide=guide_legend(reverse=TRUE))+
    #Nombre comunas
    geom_text_repel(data = comunas, aes(x = COORDS_X, y = COORDS_Y, label = NOM_COMUNA, family = params[['my_font']])
                    , size = 3, max.overlaps = Inf)+
    #Elementos carta
    annotation_north_arrow(location="tr", rotation = NULL, height = unit(1,'cm'),
                           style = north_arrow_fancy_orienteering())+
    annotation_scale(plot_unit = "m", location = 'bl', height = unit(0.3, "cm"), style ='ticks') +
    labs(title = titulo, subtitle = subtitulo) +
    tema_insumos
    
  return(mapa_base)
}
