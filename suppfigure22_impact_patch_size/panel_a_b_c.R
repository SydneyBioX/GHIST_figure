

library(ggplot2)
library(SingleCellExperiment)
library(scattermore)
library(ggpubr)


mytheme <- theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))



gt <- read_csv("gt.csv")
final_model <- read_csv("final_model.csv")
patch_192 <- read_csv("patch_192.csv")
patch_384 <- read_csv("patch_384.csv")


# accuracy
accuracy_df <- rbind(
  
  data.frame( model = "patch 256 (final model)", 
              accu = mean( final_model$celltype == gt$celltype )) , 
  
  data.frame( model = "patch 192", 
              accu = mean( patch_192$celltype ==  gt$celltype ))  ,
  
  data.frame( model = "patch 384", 
              accu = mean( patch_384$celltype == gt$celltype))  
  
)


accuracy_df$model <- factor(accuracy_df$model, 
                            levels = c( 
                              "patch 256 (final model)" , 
                              "patch 192", 
                              "patch 384") )

accuracy_comparison <- ggplot(accuracy_df, aes(x = model, y = accu, fill = model)) + geom_col( alpha = 0.8) + mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",
                                 "#8172B2", "coral"))


# panel a 
accuracy_comparison


# scatterplot 

colour_choice <-c("#4E79A7" , "#F28E2B",   "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",       "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,   "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,        "T", "B", 
                            "unassigned" )

comp_df <- NULL

to_plot <-   gt 
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "ground truth"
comp_df  <- rbind(comp_df ,  temp )



to_plot <-  final_model 
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "patch 256 (final model)"
comp_df  <- rbind(comp_df ,  temp )




to_plot <-   patch_192
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "patch 192" 
comp_df  <- rbind(comp_df ,  temp )



to_plot <-  patch_384 
temp <- data.frame( table(to_plot$celltype) )
temp$model <-  "patch 384"
comp_df  <- rbind(comp_df ,  temp )


comp_df$model <- factor( comp_df$model , levels = c("ground truth", 
                                                    "patch 256 (final model)" , 
                                                    "patch 192", 
                                                    "patch 384"  ) )

composition_comparison <- ggplot(comp_df, aes(x = model, y = Freq, fill = Var1)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = colour_choice)  + ggtitle("Cell type composition") + mytheme +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 


# panel b
composition_comparison





svg <- read_csv("svg.csv")


svg$model <- factor(svg$model ,  levels = c( "patch 256 (final model)", 
                                                   "patch 192" , "patch 384"))
 
 
ggplot( svg, aes(x = model, y = corr, fill = model, colour = model)) + 
  geom_boxplot( alpha = 0.7) + 
  theme_minimal() +  facet_wrap(~sample + type + n , scales= "free", ncol = 4) + ylim(0, 1) + 
  mytheme + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",  "#8172B2", "coral")) +
  scale_colour_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6", "#8172B2", "coral"))



