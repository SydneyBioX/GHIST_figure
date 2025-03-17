
library(ggplot2)
library(viridis)
library(ggpubr)

gt <- read_csv( "panel_e_left_data.csv" )
pred <- read_csv( "panel_e_right_data.csv" )


# panel e 
ggplot( gt , aes(x = x_cord, y = y_cord, colour = gene)) + 
  geom_scattermore(pointsize = 1.3) + theme_minimal() + 
  scale_colour_viridis_c() + ggtitle("ground truth") 

ggplot( pred  , aes(x = x_cord, y = y_cord, colour = gene)) + 
  geom_scattermore(pointsize = 1.3) + theme_minimal() + 
  scale_colour_viridis_c() + ggtitle("predicted")
 


# panel f 

data <- read_csv("panel_f_data.csv")

data$n <- factor(data$n, levels = c( "top_20", "top_50" , "bottom_20" ,"bottom_50" ) ) 
ggplot(data, aes(x = n, y = corr, fill = n , 
                                  colour = n )) +
  geom_boxplot( alpha = 0.7) + 
  theme_minimal() +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(~sample   , scales= "free", ncol = 4)  




# panel g 

scd <- read_csv("panel_g_SCD_data.csv")
  
ggplot( scd, aes(x = gt, y = pred, colour = pred)) + geom_scattermore() +  
  stat_cor( aes(label = ..r.label..) , method = "pearson" ) + theme_minimal() + 
  scale_colour_viridis_c() + ggtitle("SCD")



fasn <- read_csv("panel_g_FASN_data.csv")

ggplot( fasn, aes(x = gt, y = pred, colour = pred)) + geom_scattermore() +  
  stat_cor( aes(label = ..r.label..) , method = "pearson" ) + theme_minimal() + 
  scale_colour_viridis_c() + ggtitle("FASN")


foxa1 <- read_csv("panel_g_FOXA1_data.csv")

ggplot( foxa1, aes(x = gt, y = pred, colour = pred)) + geom_scattermore() +  
  stat_cor( aes(label = ..r.label..) , method = "pearson" ) + theme_minimal() + 
  scale_colour_viridis_c() + ggtitle("FOXA1")




epcam <- read_csv("panel_g_EPCAM_data.csv")

ggplot( epcam , aes(x = gt, y = pred, colour = pred)) + geom_scattermore() +  
  stat_cor( aes(label = ..r.label..) , method = "pearson" ) + theme_minimal() + 
  scale_colour_viridis_c() + ggtitle("EPCAM")





