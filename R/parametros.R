# Parámetros --------------------------------------------------------------
set_color_variables <- function(col_com =  "black",col_reg = "gray30",
                       col_pob = "#CFCAB6",col_ent = "gray50",
                       tipo_map =  "light",col_agua = "#D1EBF7",
                       col_calles = "#FDFEFE",my_font = "sans")
{
params <<- list(col_com = col_com,col_reg = col_reg,col_pob = col_pob,col_ent = col_ent,tipo_map = tipo_map,col_agua = col_agua,col_calles = col_calles,my_font = my_font) 
}

# Definciónde Parámetros --------------------------------------------------

set_vars <- function(personas = sel_indicadores(tipo = "personas"),
                     rural = sel_indicadores(tipo = "AREA") %>% setdiff(c('IATA')),
                     viviendas = sel_indicadores(tipo = "viviendas"),
                     hogares = sel_indicadores(tipo = "hogares"),
                     ninos = sel_indicadores(tipo = "e4a18"))
{
  vars_list <<- list(personas = personas, rural = rural, viviendas = viviendas,hogares = hogares, ninos = ninos)
}


# Actualización de datos por condiciones ----------------------------------


# lista de indicadores

lista_indicadores <- function(){
  lista <- names(mz_indicadores)[names(mz_indicadores) %in% Diccionario_indicadores$abrev_ind]
  lista
}
