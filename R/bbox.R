bbox_funcion <- function(region) {
  
  bbox_new <- st_bbox(region)# current bounding box
  
  xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
  yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
  
  bbox_new[1] <- bbox_new[1] - (0.2 * xrange) # xmin - left, hacia la izquierda
  # bbox_new[3] <- bbox_new[3] + (0.5 * xrange) # xmax - right, hacia la derecha
  # bbox_new[2] <- bbox_new[2] - (0.5 * yrange) # ymin - bottom, hacia abajo
  # bbox_new[4] <- bbox_new[4] + (0.5 * yrange) # ymax - top, hacia arriba
  
  bbox_new <<- bbox_new %>%  # take the bounding box and make it a sf polygon
    st_as_sfc() %>% 
    st_transform(crs_utm)
}