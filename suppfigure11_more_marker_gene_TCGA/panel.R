


mytheme <- theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

 

df_all <- read_csv("data.csv")



plot_fibro <- ggplot( df_all, aes(x  = celltype, y = Stromal_PDGFRB, fill = "1" , colour = "1" ))+ 
  geom_boxplot( alpha= 0.3) + mytheme  +
  scale_fill_manual(values = "steelblue") +
  scale_colour_manual( values = "steelblue") 


plot_macro <- ggplot( df_all, aes(x  = celltype, y = Macrophag_CD163, fill = "1" , colour = "1" ))+ 
  geom_boxplot( alpha= 0.3) + mytheme  +
  scale_fill_manual(values = "steelblue") +
  scale_colour_manual( values = "steelblue") 



plot_malig <- ggplot( df_all, aes(x  = celltype, y = Malignant_EPCAM, fill = "1" , colour = "1" ))+ 
  geom_boxplot( alpha= 0.3) + mytheme  +
  scale_fill_manual(values = "steelblue") +
  scale_colour_manual( values = "steelblue") 



plot_B <- ggplot( df_all, aes(x  = celltype, y = B_MS4A1, fill = "1" , colour = "1" ))+ 
  geom_boxplot( alpha= 0.3) + mytheme  +
  scale_fill_manual(values = "steelblue") +
  scale_colour_manual( values = "steelblue") 



plot_epi<- ggplot( df_all, aes(x  = celltype, y = Epithelial_KRT5, fill = "1" , colour = "1" ))+ 
  geom_boxplot( alpha= 0.3) + mytheme  +
  scale_fill_manual(values = "steelblue") +
  scale_colour_manual( values = "steelblue") 




plot_T <- ggplot( df_all , aes(x  = celltype, y = T_CD3E , fill = "1" , colour = "1" ))+ 
  geom_boxplot( alpha= 0.3) + mytheme  +
  scale_fill_manual(values = "steelblue") +
  scale_colour_manual( values = "steelblue") 



plot_myeloid <- ggplot( df_all , aes(x  = celltype, y = Myeloid_LILRA4 , fill = "1" , colour = "1" ))+ 
  geom_boxplot( alpha= 0.3) + mytheme  +
  scale_fill_manual(values = "steelblue") +
  scale_colour_manual( values = "steelblue") 



plot_plasma <- ggplot( df_all, aes(x  = celltype, y = Plasma_MZB1, fill = "1" , colour = "1" ))+ 
  geom_boxplot( alpha= 0.3) + mytheme  +
  scale_fill_manual(values = "steelblue") +
  scale_colour_manual( values = "steelblue") 





ggarrange( plotlist = list(plot_B,    plot_T ,
                           plot_plasma,      plot_myeloid,
                           plot_malig ,    plot_epi, 
                           plot_fibro, plot_macro ),
           
           ncol = 2, nrow = 5 , common.legend = T)













