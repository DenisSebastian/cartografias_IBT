
set_breaks <- function(){
  vector_indicador <<- insumo %>%
    st_drop_geometry() %>%
    pull()
  
  if(indicador!="CLUSTERM"){
    vector_indicador <<- round(vector_indicador,2)
  } 

  
  
  if(indicador=="IEJ"){
    insumo <<-
      insumo %>%
      mutate(class = cut(insumo$IEJ,
                         breaks=c(0, 1, 4, 8, 12 ,17, 20),
                         labels=c("0", "1 - 4", "5 - 8", "9 - 12", "13 - 17", ">17"
                         ), include.lowest = TRUE))
    
    breaks <<- c(0, 1, 4, 8, 12 ,17, 20)
    
    
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
    insumo <<- insumo
    indicador <<- "ZONAS_IBT"
    subtitulo <<-  "Zonas IBT"
    
   } else  {
      breaksJenks <- classIntervals(na.omit(vector_indicador),
                                   n = length (ind_pal),
                                   style = "fisher")$brks %>%
                                   round(2)
     
  
      breaks <<- breaksJenks
      labels <- gen_labs(insumo$class, breaks = breaksJenks, dig_dif = 0.01) %>% gsub("\\.",",",.)
      
      insumo <<-
         insumo %>%
         mutate(class = cut((vector_indicador),
                          breaks = breaks,
                          labels = labels, include.lowest=TRUE))
  }
}