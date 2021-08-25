# Funciones para cartografìas

# Si no existe directorio lo crea

make_dir <- function(path){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# transformar -999 a NA
fix_missing <- function(x) {
  if(is.numeric(x)){
    x[x == -999] <- NA

  }
  return(x)
}

sel_indicadores <- function(tipo) {
  indicadores <- Diccionario_indicadores %>%
    filter(pob_rev == tipo) %>%
    select(abrev_ind) %>% pull()
  if(tipo == "AREA"){
    indicadores <- indicadores[indicadores != "ICV"]
  }
  return(indicadores)
}

pto_por_coma <- function(x){
  format(x, big.mark = ".", decimal.mark = "," ,
         scientific = FALSE)

}

breaks_to_labels <- function(breaksList, roundDigits){
  breaksRound <- round(breaksList, roundDigits)
  breaksRound <- lapply(breaksRound, pto_por_coma)
  breaksStr <- lapply(breaksRound, as.character) %>% unlist()
  lengthBreaks <- length(breaksStr)
  labelList <- list()

  for (b in 1:(lengthBreaks-1)){
    labelList[b] <- paste0(breaksStr[b], " - ",
                           breaksStr[b + 1])
  }
  return(labelList %>% unlist())
}


gen_labs <-  function(X, breaks, dig_dif = dig_dif, sep = " - "){
  labA <- c(breaks[1], breaks[2:(length(breaks)-1)]+dig_dif)
  labB <- sprintf('%.2f', breaks[2:(length(breaks))])
  labs <- c(paste(labA, labB, sep = sep))
  return(labs)
}

#labs <- gen_labs(Z, breaks = quiebres,  dig_dif = 0.01)

#cut(Z, breaks = quiebres, labels = labs)


# Define lista de colores -------------------------------------------------

colors_str_list <- function(colorsStr){
  return(as.list(strsplit(colorsStr, ",")))
}



# Definir columna de población relevante ----------------------------------


pobRelevante_f <-  function(ind){
  pr <- Diccionario_indicadores %>%
    dplyr::filter(abrev_ind == ind) %>%
    dplyr::select(pob_rev) %>% pull() %>% toupper()
  if(pr == "HOGARES"){
    pr <- "HOG_N"
  }else if(pr == "VIVIENDAS"){
    pr <-  "TOTAL_V"
  }
  return(pr)
}


# Definir nombre en Legenda sin Registros --------------------------------

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


name_sin <-  function(ind){
  if(ind == "ISE"){
    pr <- "Sin Población de 4 a 18 años"
  }else if (ind == "IPJ"){
      pr <- "Sin Población de 15 a 24 años"
  }else{
    pr <- Diccionario_indicadores %>%
      dplyr::filter(abrev_ind == ind) %>%
      dplyr::select(leyenda) %>% pull()
    pr <-  paste0("Sin ", firstup (pr))
  }
  return(pr)
}

# Cuenta duplicados por columna ------------------------------------------

duplicados <- function(file,columna){
  if(columna == "geometry"){
    col <- file %>% select(all_of(columna))
  }else{
    col <- file %>% st_drop_geometry() %>% select(all_of(columna))
  }

  d <- col %>% duplicated() %>% sum()
  return(d)
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

cor_especial <-  function(x){
  x <-  gsub(" Del ", " del ", x)
  x <-  gsub(" A ", " a ", x)
  x <-  gsub(" De ", " de ", x)
  x <-  gsub("O'higgins", "O'Higgins", x)
  x <-  gsub("Ibt", "IBT", x)
  return(x)
}

clean_title <-  function(x){
  y <- tolower(x)
  y <- simpleCap(y)
  y <- cor_especial(y)
  y <- trimws(y)
  return(y)
}

sep_by_length <- function(element_1, element_2, nchar_max = 50){
  if(nchar(element_2) > nchar_max){
    title <-  paste0(clean_title(element_1),"\n",
                     clean_title(element_2))
  }else{
    title <-  paste0(clean_title(element_1),", ",
                     clean_title(element_2))
  }
  return(title)
}


make_title <- function(element_1, element_2, nchar_max = 50){
  if(nchar(element_2) > nchar_max){
    title <-  paste0(clean_title(element_1),"\n",
                     clean_title(element_2))
  }else{
    title <-  paste0(clean_title(element_1),", ",
                     clean_title(element_2))
  }
  return(title)
}

sf_toupper <-  function(sf_object){
  names(sf_object)[1:(ncol(sf_object)-1)] <- toupper(names(sf_object)[1:(ncol(sf_object)-1)])
  return(sf_object)
}