
composicion <- function(mapa_base,salida = "data/res/"){ 
  
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
  
  ggsave(paste0(salida , archivo,"_",stringr::str_to_lower(indicador),"_",cod_reg, '.png'),
         mapa_insumo, width = width_page, height = height_page, units = 'cm', dpi = 300,limitsize = F)
  
  # ggsave(paste0(ruta_salida_pdf, "/", archivo,"_", stringr::str_to_lower(indicador),"_",cod_reg, '.pdf'),
  #        mapa_insumo, width = width_page, height = height_page, units = 'cm', dpi = 300,limitsize = F)
  
}
