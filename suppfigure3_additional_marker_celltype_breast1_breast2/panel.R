


plot_scatter <- function(data, gene ){ 
  
  y_var <- paste0(gene, "_gt")
  
  p1 <- ggplot( data , aes(x = x_cord, y = y_cord, 
                        colour =  .data[[y_var]] )) + 
    geom_scattermore(pointsize = 1.3) + theme_minimal() + 
    scale_colour_viridis_c( limits = c(-2, 4)) + 
    ggtitle(   y_var )
  
  y_var <- paste0(gene, "_pred")
  
  p2  <- ggplot( data , aes(x = x_cord, y = y_cord, 
                        colour =  .data[[y_var]] )) + 
    geom_scattermore(pointsize = 1.3) + theme_minimal() + 
    scale_colour_viridis_c( limits = c(-2, 4)) + 
    ggtitle(   y_var )
  
  return ( list(p1 = p1, p2 = p2  )) 
  
}

sample1_data <- read_csv("sample1_data.csv")

plot_scatter( data =  sample1_data,   "stromal_POSTN" ) 

plot_scatter( data =  sample1_data,   "macrophage_CD68" ) 

plot_scatter( data =  sample1_data,   "epithelial_MYLK" ) 

plot_scatter( data =  sample1_data,   "T_CXCR4" ) 



sample2_data <- read_csv("sample2_data.csv")

plot_scatter( data =  sample2_data,   "stromal_POSTN" ) 

plot_scatter( data =  sample2_data,   "macrophage_CD68" ) 

plot_scatter( data =  sample2_data,   "epithelial_MYLK" ) 

plot_scatter( data =  sample2_data,   "T_CXCR4" ) 





 