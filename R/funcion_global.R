cartasPlot <- function(insumo) {
  
  set.seed(1) #Incorporacion de semilla
  
  
  print(paste0("Procesando: ", indicador))
  
  
  # filtro de indicador -----------------------------------------------------
  
  insumo <<- indicadores %>%
    dplyr::select(all_of(indicador))
  
  # definicón de nombres ----------------------------------------------------
  
  set_text_elements()
  
  # crear variables con los quiebres ---------------------------------------
  
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
