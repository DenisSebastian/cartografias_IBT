
set_breaks <- function(){
  vector_indicador <<- indicador_seleccionado %>%
    st_drop_geometry() %>%
    pull()
  
  if(indicador!="CLUSTERM"){
    vector_indicador <<- round(vector_indicador,2)
  } 

  
  
  if(indicador=="IEJ"){
    indicador_seleccionado <<-
      indicador_seleccionado %>%
      mutate(class = cut(indicador_seleccionado$IEJ,
                         breaks=c(0, 1, 4, 8, 12 ,17, 20),
                         labels=c("0", "1 - 4", "5 - 8", "9 - 12", "13 - 17", ">17"
                         ), include.lowest = TRUE))
    
    breaks <<- c(0, 1, 4, 8, 12 ,17, 20)
    
    
  } else if (indicador=="CLUSTERM"){
    # Zonas IBT ---------------------------------------------------------------
    indicador_seleccionado$CLUSTERM[indicador_seleccionado$CLUSTERM=="HH"] <-"Zona IBT Alto"
    indicador_seleccionado$CLUSTERM[indicador_seleccionado$CLUSTERM=="NS"] <-"Zona IBT Medio"
    indicador_seleccionado$CLUSTERM[indicador_seleccionado$CLUSTERM=="LL"] <-"Zona IBT Bajo"
    colnames(indicador_seleccionado)[colnames(indicador_seleccionado)== "CLUSTERM"] <- "ZONAS_IBT"
    indicador_seleccionado <-
      indicador_seleccionado %>%
      mutate(class = indicador_seleccionado$ZONAS_IBT)
    indicador_seleccionado$class <- factor(indicador_seleccionado$class, levels = c("Zona IBT Bajo", "Zona IBT Medio", "Zona IBT Alto"))
    indicador_seleccionado <<- indicador_seleccionado
    indicador <<- "ZONAS_IBT"
    subtitulo <<-  "Zonas IBT"
    
   } else  {
      breaksJenks <- classIntervals(na.omit(vector_indicador),
                                   n = length (ind_pal),
                                   style = "fisher")$brks %>%
                                   round(2)
     
  
      breaks <<- breaksJenks
      labels <- gen_labs(indicador_seleccionado$class, breaks = breaksJenks, dig_dif = 0.01) %>% gsub("\\.",",",.)
      
      indicador_seleccionado <<-
         indicador_seleccionado %>%
         mutate(class = cut((vector_indicador),
                          breaks = breaks,
                          labels = labels, include.lowest=TRUE))
  }
}