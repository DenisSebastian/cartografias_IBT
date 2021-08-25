library(ggplot2)
# Parámetros --------------------------------------------------------------
col_com <-  "black"
col_reg <- "gray30"
col_pob <- "#CFCAB6"
col_ent <- "gray50"
tipo_map <-  "light"
col_agua <- "#D1EBF7"
col_calles <- "#FDFEFE"
my_font <- "sans"
# Temas para cartografias -------------------------------------------------
# Tema para visualización de insumos
tema_insumos <- theme(
  
  
                      panel.background = element_rect(fill = NA,
                                                      colour = 'grey50',
                                                      size = 0.5,
                                                      linetype = "solid"),
                      panel.ontop = T,
                      panel.grid = element_blank(),
                      axis.title=element_blank(),
                      # axis.text.y = element_text(angle = 90, hjust=0.5),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      plot.title = element_text(family='sans',
                                                size = 16,
                                                face="bold",
                                                color = "gray10"),
                      plot.subtitle=element_text(size=14,
                                                 face = "bold",
                                                 colour = 'gray50'),
                      legend.title=element_text(family='sans', size=12, face='bold'),
                      legend.text = element_text(family='sans', size=10),
                      legend.key = element_rect(colour = NA, fill = "transparent",
                                                size = 1),
                      legend.key.size = unit(0.5,'line'),
                      legend.key.height = unit(1,'cm'),
                      legend.key.width = unit(1,'cm'),
                      legend.margin = margin(0,0,0,0),
                      legend.box = 'vertical',
                      #legend.position = c(0.05, 0.05),
                      #legend.position = c(0.01, 0.8), # posicion de la leyenda (x,y)
                      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
                      legend.justification = c(0, 1), #justificacion
                      legend.position = c(0.03, .95), #posicion de la leyenda (x,y)
                      #legend.box.background = element_rect(fill = "transparent"), # get rid of legend panel bg
                      #panel.background = element_rect(fill = NA), # bg of the panel
                      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                      panel.grid.major = element_blank(), # get rid of major grid
                      panel.grid.minor = element_blank(), # get rid of minor grid
                      #legend.box.margin = margin(3,3,3,3),
                      #legend.box.background = element_rect(color="transparent", size=0.5), #box de la leyenda
                      #legend.justification = c("left", "bottom"),
                      legend.direction = 'vertical',
                      #t,r,b,l
                      plot.margin = unit(c(0.4,0.4,0.4,0.4), 'in'))
