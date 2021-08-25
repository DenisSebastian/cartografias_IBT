# Definciónde Parámetros --------------------------------------------------

vars_personas <- sel_indicadores(tipo = "personas")
vars_rural <- sel_indicadores(tipo = "AREA")
vars_rural <-  vars_rural[!vars_rural %in% "IATA"]
vars_viviendas <- sel_indicadores(tipo = "viviendas")
vars_hogares <- sel_indicadores(tipo = "hogares")
vars_ninos <- sel_indicadores(tipo = "e4a18")


# Actualización de datos por condiciones ----------------------------------

indicadores <- insumos_acc %>%
  as.data.frame() %>%
  mutate(across(all_of(vars_rural), ~ifelse(MANZ_EN == "RURAL", NA, .x))) %>%
  mutate(across(all_of(vars_personas), ~ifelse(PERSONAS <= 0, NA, .x))) %>%
  mutate(across(all_of(vars_viviendas), ~ifelse(TOTAL_V <= 0, NA, .x))) %>%
  mutate(across(all_of(vars_hogares), ~ifelse(HOG_N <= 0, NA, .x))) %>%
  mutate(across(all_of(vars_ninos), ~ifelse(E4A18 <= 0, NA, .x))) %>%
  st_as_sf()

#summary(indicadores)

# transforma -999 a NA ----------------------------------------------------

indicadores[] <- lapply(indicadores[], FUN = fix_missing)
#summary(indicadores)

# lista de indicadores

indicadores_fuente <- names(indicadores)[names(indicadores) %in% Diccionario_indicadores$abrev_ind]


