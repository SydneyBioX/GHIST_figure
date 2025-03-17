

library(ggplot2)
library(SingleCellExperiment)
library(scattermore)
library(ggpubr)


mytheme <- theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))



gt <- read_csv("gt.csv")
otsu <- read_csv("otsu.csv")
stardist <- read_csv("stardist.csv")


# accuracy
num_cells <- rbind(
  
  data.frame( model = "GHIST (Hover-Net)", 
              cells = nrow(gt ) ) ,  
  
  data.frame( model = "Otsu", 
              cells = nrow( otsu ) )  , 
  
  
  data.frame( model = "StarDist" , 
              cells = nrow( stardist  ) )   
  
  
)


num_cells$model <- factor(num_cells $model, 
                          levels = c( "GHIST (Hover-Net)", 
                                      "Otsu", 
                                      "StarDist") )

num_cells_comparison <- ggplot(num_cells , aes(x = model, y = cells, fill = model)) + geom_col( alpha = 0.8) + mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",
                                 "#8172B2", "coral"))

# panel a 
num_cells_comparison




# accuracy
accuracy_df <- rbind(
  
  data.frame( model = "GHIST (Hover-Net)" , 
              accu = mean( gt$celltype  == gt$celltype_gt )) ,  
  
  data.frame( model = "Otsu" , 
              accu = mean( otsu$celltype   ==  otsu$celltype_gt )) , 
  
  
  data.frame( model = "StarDist" , 
              accu = mean( stardist$celltype ==  stardist$celltype_gt ))  
  
  
)


accuracy_df$model <- factor(accuracy_df$model, 
                            levels = c(   "GHIST (Hover-Net)" , 
                                          "Otsu", 
                                          "StarDist" ) )

accuracy_comparison <- ggplot(accuracy_df, aes(x = model, y = accu, fill = model)) + geom_col( alpha = 0.8) + mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",
                                 "#8172B2", "coral"))


# panel b 
accuracy_comparison

# number of cells segmented 




colour_choice <-c("#4E79A7" , "#F28E2B",   "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",       "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,   "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,        "T", "B", 
                            "unassigned" )

comp_df <- NULL

to_plot <- gt
temp <- data.frame( table( to_plot$celltype ) )
temp$model <-"GHIST (Hover-Net)"  
comp_df  <- rbind(comp_df ,  temp )





to_plot <-  gt 
temp <- data.frame( table( to_plot$celltype_gt ) )
temp$model <-  "ground truth"
comp_df  <- rbind(comp_df ,  temp )



to_plot <-  otsu
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "Otsu"
comp_df  <- rbind(comp_df ,  temp )




to_plot <-  stardist
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "StarDist"
comp_df  <- rbind(comp_df ,  temp )


comp_df$model <- factor( comp_df$model , levels = c( "ground truth" , 
                                                     "GHIST (Hover-Net)", 
                                                     "Otsu", 
                                                     "StarDist") )
composition_comparison <- ggplot(comp_df, aes(x = model, y = Freq, fill = Var1)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = colour_choice)  + ggtitle("Cell type composition") + mytheme +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 


# panel c 
composition_comparison 





svg <- read_csv("svg.csv")

svg$model <- factor(svg$model ,   levels = c(  "GHIST", 
                                                    "OTSU" , 
                                                    "stardist"))

# panel d 
ggplot( svg, aes(x = model, y = corr, fill = model, colour = model)) + 
  geom_boxplot( alpha = 0.7) + 
  theme_minimal() +  facet_wrap(~sample + type + n , scales= "free", ncol = 4) + ylim(0, 1) + 
  mytheme + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",  "#8172B2", "coral")) +
  scale_colour_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6", "#8172B2", "coral"))



