# Parámetros --------------------------------------------------------------
set_params <- function(col_com =  "black",col_reg = "gray30",
                       col_pob = "#CFCAB6",col_ent = "gray50",
                       tipo_map =  "light",col_agua = "#D1EBF7",
                       col_calles = "#FDFEFE",my_font = "sans")
{
params <<- c(col_com = col_com,col_reg = col_reg,col_pob = col_pob,col_ent = col_ent,tipo_map = tipo_map,col_agua = col_agua,col_calles = col_calles,my_font = my_font) 
}

# Definciónde Parámetros --------------------------------------------------

set_vars <- function(personas = sel_indicadores(tipo = "personas"),
                     rural = sel_indicadores(tipo = "AREA")[!rural %in% "IATA"],
                     viviendas = sel_indicadores(tipo = "viviendas"),
                     hogares = sel_indicadores(tipo = "hogares"),
                     ninos = sel_indicadores(tipo = "e4a18"))
{
  vars <<- c(personas = personas, rural = rural, viviendas = viviendas,hogares = hogares, ninos = ninos)
}


# Actualización de datos por condiciones ----------------------------------

# transforma -999 a NA ----------------------------------------------------
fix_insumos <- function(){
  indicadores <- insumos_acc %>%
    as.data.frame() %>%
    mutate(across(all_of(vars[['rural']]), ~ifelse(MANZ_EN == "RURAL", NA, .x))) %>%
    mutate(across(all_of(vars[['personas']]), ~ifelse(PERSONAS <= 0, NA, .x))) %>%
    mutate(across(all_of(vars[['viviendas']]), ~ifelse(TOTAL_V <= 0, NA, .x))) %>%
    mutate(across(all_of(vars[['hogares']]), ~ifelse(HOG_N <= 0, NA, .x))) %>%
    mutate(across(all_of(vars[['ninos']]), ~ifelse(E4A18 <= 0, NA, .x))) %>%
    st_as_sf()
  
  indicadores[] <- lapply(indicadores[], FUN = fix_missing)
  indicadores
}

# lista de indicadores

lista_indicadores <- function(){
  lista <- names(indicadores)[names(indicadores) %in% Diccionario_indicadores$abrev_ind]
  lista
}
