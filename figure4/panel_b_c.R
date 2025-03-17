
df <- read_csv("panel_c_data.csv")

colour_choice <-c("#4E79A7" , "#F28E2B" , "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",       "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,   "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,        "T", "B", 
                            "unassigned" )


ggplot( df , aes(x = sample, y = Freq, fill = Var1)) + geom_col(position = "fill") + 
  theme_minimal() + 
  scale_fill_manual(values = colour_choice ) + mytheme +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))    

colour_choice <-c("#4E79A7" , "#F28E2B",   "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",       "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,   "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,        "T", "B", 
                            "unassigned" )




plot_patient <- function( patient, patient_name ){
     
    p1 <- ggplot(to_plot, aes(x = x_cord, y = y_cord, colour =  group_broad_type)) + 
      geom_scattermore(pointsize = 1.2) + mytheme + 
      scale_colour_manual(values = colour_choice ) + ggtitle(patient_name)
    
     
    p2 <- ggplot(to_plot, aes(x = x_cord, y = y_cord, colour =   DSP )) + 
      geom_scattermore(pointsize = 1.2) + mytheme + scale_colour_viridis_b() + ggtitle(patient_name)
    
     
    p3 <- ggplot(to_plot, aes(x = x_cord, y = y_cord, colour =   SFRP4 )) + 
      geom_scattermore(pointsize = 1.2) + mytheme + scale_colour_viridis_b() + ggtitle(patient_name)
     
    p4 <- ggplot(to_plot, aes(x =  umap1, y = umap2, colour = group_broad_type )) + 
      geom_scattermore(pointsize = 1.2) + mytheme + 
      scale_colour_manual(values = colour_choice ) +  ggtitle(patient_name)
    
    
    return( list(p1 , p2 , p3, p4))
    
}




TCGA_AN_A0XP <- read_csv("TCGA-AN-A0XP.csv")

plot_patient( TCGA_AN_A0XP , "TCGA_AN_A0XP")



TCGA_C8_A1HF <- read_csv("TCGA-C8-A1HF.csv")

plot_patient( TCGA_C8_A1HF  , "TCGA_C8_A1HF")






