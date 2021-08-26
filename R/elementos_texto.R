set_text_elements <- function(){
  var_ind <<- Diccionario_indicadores %>%
    filter(abrev_ind==indicador)
  
  titulo <<- make_title(element_1 = var_ind$nombre_grafico, element_2 =  region$NOM_REGION, nchar_max = 50)
  subtitulo <<- var_ind$unidad
  archivo <<- var_ind$archivo
  colorStr <<- as.character(Diccionario_indicadores[Diccionario_indicadores$abrev_ind == indicador,
                                                   colnames(Diccionario_indicadores) == "colores"])
  ind_pal <<- colors_str_list(colorStr) %>% unlist()
}