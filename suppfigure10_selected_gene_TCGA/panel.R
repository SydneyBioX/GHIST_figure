

to_plot <- read_csv("left_data.csv")

 
to_plot$variable <- factor(to_plot$variable, 
       levels = c( "Malignant--EPCAM" , "T--EPCAM",  "Macrophage--EPCAM", "Myoepi--EPCAM", 
                   "Stromal--EPCAM", "Myeloid--EPCAM",  "B--EPCAM",  "Plasma--EPCAM" ))

a <- ggplot(to_plot, aes( y = value, x = variable,  colour = variable, fill = variable ) ) + 
  geom_boxplot( alpha = 0.7) +  theme_minimal() + mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

# left panel 
a

to_plot <- read_csv("right_data.csv")

to_plot$variable <- factor(to_plot$variable , 
                           levels = c( "Malignant--SFRP4" , "T--SFRP4",  "Macrophage--SFRP4", "Myoepi--SFRP4", 
                                       "Stromal--SFRP4", "Myeloid--SFRP4",  "B--SFRP4",  "Plasma--SFRP4" ))
b <- ggplot(to_plot, aes( y = value, x = variable,  colour = variable, fill = variable ) ) + 
  geom_boxplot( alpha = 0.7)  + mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

# right panel 
b
