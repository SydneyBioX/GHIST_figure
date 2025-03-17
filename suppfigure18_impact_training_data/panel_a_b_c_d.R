

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
training_50 <- read_csv("training_50.csv")
training_25 <- read_csv("training_25.csv")


# accuracy
accuracy_df <- rbind(
  
  data.frame(model = "final model" ,  
             accu = mean( final_model$celltype == gt$celltype )) , 
  
  data.frame( model = "50% training data", 
              accu = mean( training_50$celltype == gt$celltype )) , 
  
  data.frame( model = "25% training data", 
              accu = mean( training_25$celltype == gt$celltype))  

)




 

celltype_prop <- NULL 

celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  = gt$celltype , model = "ground truth" )  ) 


celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  = final_model$celltype , model = "final model")  ) 

celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  = training_50$celltype , model = "50% training data")  ) 

celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  =  training_25$celltype , model = "25% training data")  ) 


celltype_counts <- celltype_prop %>%
  dplyr::count(model, celltype) %>%
  complete(model, celltype, fill = list(n = 0)) %>%
  dplyr::arrange(model, celltype)




# Step 1: Calculate the proportions
celltype_counts <- celltype_counts %>%
  dplyr::group_by(model) %>%
  dplyr::mutate(proportion = n / sum(n)) %>%
  ungroup()

# Step 2: Separate ground truth data for comparison
ground_truth <- celltype_counts %>% 
  dplyr::filter(model == "ground truth") %>%
  dplyr::select(celltype, ground_truth_proportion = proportion)

# Step 3: Join with the ground truth proportions
data_with_ground_truth <- celltype_counts %>%
  dplyr::left_join(ground_truth, by = "celltype") %>%
  filter(model != "ground truth")

# Step 4: Calculate RMSE for each model
rmse_results <- data_with_ground_truth %>%
  dplyr::group_by(model) %>%
  dplyr:: summarize(
    RMSE = sqrt(mean((proportion - ground_truth_proportion)^2, na.rm = TRUE))
  )



 


rmse_results$model <- factor( rmse_results$model, 
                                             levels = c("final model", 
                                                        "50% training data" ,
                                                        "25% training data") )


rmse_comparison <- ggplot(rmse_results  ,   aes(x = model, y = RMSE, fill = model)) + 
  geom_col( alpha = 0.8) + 
  mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087"  ))

# panel b 
rmse_comparison

 
 

colour_choice <-c("#4E79A7" , "#F28E2B",   "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",       "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,   "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,        "T", "B", 
                            "unassigned" )

comp_df <- NULL

to_plot <-   gt
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "ground truth"
comp_df  <- rbind(comp_df ,  temp )


 

to_plot <- final_model
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "final model" 
comp_df  <- rbind(comp_df ,  temp )



to_plot <- training_50
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "50% training data" 
comp_df  <- rbind(comp_df ,  temp )




to_plot <- training_25
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "25% training data" 
comp_df  <- rbind(comp_df ,  temp )

 



comp_df$model <- factor(comp_df$model ,   levels = c( "ground truth" , 
                                                        "final model" , 
                                                        "50% training data", 
                                                        "25% training data" 
                                            ) )

composition_comparison <- ggplot(comp_df, aes(x = model, y = Freq, fill = Var1)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = colour_choice)  + ggtitle("Cell type composition") + mytheme +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 

# panel a 
composition_comparison


# panel d
ggplot(comp_df, aes(x = model, y = Freq, fill = Var1)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values = colour_choice)  + ggtitle("ground truth") + mytheme +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(~Var1, scales="free_y", ncol = 9) 

 



svg <- read_csv("svg.csv")


svg$model <- factor(   svg$model , 
  levels = c(
    "final model" ,
    "50% training data", "25% training data"))



ggplot( svg, aes(x = model, y = corr, fill = model, colour = model)) + 
  geom_boxplot( alpha = 0.7) + 
  theme_minimal() +  facet_wrap(~sample + type + n , scales= "free", ncol = 4) + ylim(0, 1) + 
  mytheme + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087"  )) +
  scale_colour_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087"  ))
 