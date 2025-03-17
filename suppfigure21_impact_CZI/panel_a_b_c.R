 
library(ggplot2)
library(ggpubr)
library(readr)

mytheme <- theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))




gt <- read_csv("gt.csv")
final_model <- read_csv("final_model.csv")
czi_minor <- read_csv("czi_minor.csv")
czi_major <- read_csv("czi_major.csv")

# accuracy
accuracy_df <- rbind(
  
  data.frame( model = "model 80 (final model)", 
              accu = mean( final_model$celltype ==  gt$celltype )) , 
  
  
  data.frame( model = "CZI minor", 
              accu = mean( czi_minor$celltype ==  gt$celltype ))  ,
  
  data.frame( model = "CZI major", 
              accu = mean( czi_major$celltype ==  gt$celltype))  
  
  
  
)

# model      accu
#1 model 80 (final model) 0.6631207
#2              CZI minor 0.6627930
# 3              CZI major 0.6448828


accuracy_df$model <- factor(accuracy_df$model, 
                            levels = c( 
                              "model 80 (final model)" , 
                              "CZI minor" , 
                              "CZI major" ) )

accuracy_comparison <- ggplot(accuracy_df, aes(x = model, y = accu, fill = model)) + geom_col( alpha = 0.8) + mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",
                                 "#8172B2", "coral"))

# panel a
accuracy_comparison 
 
 




colour_choice <-c("#4E79A7" , "#F28E2B",   "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",       "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,   "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,        "T", "B", 
                            "unassigned" )

comp_df <- NULL

to_plot <-  gt
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "ground truth"
comp_df  <- rbind(comp_df ,  temp )

 
to_plot <- final_model
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "model 80 (final model)"
comp_df  <- rbind(comp_df ,  temp )


 
to_plot <- czi_minor
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "CZI minor"
comp_df  <- rbind(comp_df ,  temp )



to_plot <- czi_major
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "CZI major"
comp_df  <- rbind(comp_df ,  temp )
 


comp_df$model <- factor( comp_df$model , levels = c("ground truth", 
                                                    "model 80 (final model)" , 
                                                    "CZI minor"  ,   
                                                    "CZI major" ) )

composition_comparison <- ggplot(comp_df, aes(x = model, y = Freq, fill = Var1)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = colour_choice)  + ggtitle("Cell type composition") + mytheme +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 


# panel b 
composition_comparison 
 




svg <- read_csv("svg.csv")

svg$model <- factor(  svg$model ,   levels = c( 
                                         "model 80 (final model)" , 
                                         "czi major" , "czi minor"))


# panel c 
ggplot(svg, aes(x = model, y = corr, fill = model, colour = model)) + 
  geom_boxplot( alpha = 0.7) + 
  theme_minimal() +  facet_wrap(~sample + type + n , scales= "free", ncol = 4) + ylim(0, 1) + 
  mytheme + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",  "#8172B2", "coral")) +
  scale_colour_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6", "#8172B2", "coral"))



