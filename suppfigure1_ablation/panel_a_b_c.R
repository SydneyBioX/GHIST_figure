
gt <- read_csv("gt.csv")

final_model <- read_csv("final_model.csv")

no_neighb <- read_csv("no_neighb.csv")

no_celltype_no_neighb <- read_csv("no_celltype_no_neighb.csv")

no_ref_no_celltype_no_neighb <- read_csv("no_ref_no_celltype_no_neighb.csv")

no_ref <- read_csv("no_ref.csv")

no_ref_no_neighb <- read_csv("no_ref_no_neighb.csv")





celltype_prop <- NULL 

celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  =  gt$celltype , model = "ground truth" )  ) 


celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  =  final_model$celltype , model = "+ref, +cell type, +neighbourhood (final model)")  ) 

celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  = no_neighb$celltype , model = "+ref, +cell type (w/o neighbourhood)")  ) 

celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  = no_celltype_no_neighb$celltype , model = "+ref (w/o cell type, w/o neighbourhood)")  ) 

celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  = no_ref_no_celltype_no_neighb$celltype , model = "base model (w/o ref, w/o cell type, w/o neighbourhood)" )  ) 
 
celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  = no_ref$celltype  , model = "w/o ref + w cell type + w neighborhood" )  ) 

celltype_prop <- rbind(celltype_prop ,
                       data.frame ( celltype  = no_ref_no_neighb$celltype , model = "w/o ref + w cell type + w/o neighborhood" )  ) 



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



 

rmse_results$model <- factor(rmse_results$model, 
                             levels =  c(  
                               
                               "+ref, +cell type, +neighbourhood (final model)" , 
                               "+ref, +cell type (w/o neighbourhood)",
                               "w/o ref + w cell type + w neighborhood", 
                               
                               "+ref (w/o cell type, w/o neighbourhood)", 
                               "w/o ref + w cell type + w/o neighborhood", 
                               
                               "base model (w/o ref, w/o cell type, w/o neighbourhood)"
                               
                             ) )

rmse_proportion_comparison <- ggplot(rmse_results , 
                                     aes(x = model, y = RMSE, fill = model)) + 
  geom_col( alpha = 0.8) + 
  mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",
                                 "#8172B2", "coral" ,"#7FB800" , "#FF9999" ))


# panel a 
rmse_proportion_comparison 








colour_choice <-c("#4E79A7" , "#F28E2B",   "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",       "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,   "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,        "T", "B", 
                            "unassigned" )

comp_df <- NULL

to_plot <- gt
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "ground truth"
comp_df  <- rbind(comp_df ,  temp )



 
to_plot <-  no_neighb 
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "+ref, +cell type (w/o neighbourhood)"
comp_df  <- rbind(comp_df ,  temp )

 
to_plot <- no_celltype_no_neighb 
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "+ref (w/o cell type, w/o neighbourhood)"
comp_df  <- rbind(comp_df ,  temp )

 

to_plot <- no_ref_no_celltype_no_neighb 
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "base model (w/o ref, w/o cell type, w/o neighbourhood)"
comp_df  <- rbind(comp_df ,  temp )

 

to_plot <-  final_model
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "+ref, +cell type, +neighbourhood (final model)" 
comp_df  <- rbind(comp_df ,  temp )


 
to_plot <-  no_ref
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "w/o ref + w cell type + w neighborhood"
comp_df  <- rbind(comp_df ,  temp )

 

to_plot <- no_ref_no_neighb
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "w/o ref + w cell type + w/o neighborhood" 
comp_df  <- rbind(comp_df ,  temp )
 
 

comp_df$model <- factor( comp_df$model , levels = c( "ground truth" , 
                                                     "+ref, +cell type, +neighbourhood (final model)" , 
                                                     "+ref, +cell type (w/o neighbourhood)",
                                                     "w/o ref + w cell type + w neighborhood", 
                                                     
                                                     "+ref (w/o cell type, w/o neighbourhood)", 
                                                     "w/o ref + w cell type + w/o neighborhood", 
                                                     
                                                     "base model (w/o ref, w/o cell type, w/o neighbourhood)"
                                                     
                                                     
) )

 

 

# panel c 

ggplot(comp_df, aes(x = model, y = Freq, fill = Var1)) + 
  geom_bar(position = "dodge", stat = "identity") + 
  scale_fill_manual(values = colour_choice)  + ggtitle("ground truth") + mytheme +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  facet_wrap(~Var1, scales="free_y", ncol = 9) 
 



# panel b

data <- read_csv( "panel_b.csv")

ggplot(data, aes(x = model, y = corr, fill = model, colour = model)) + 
  geom_boxplot( alpha = 0.7) + 
  theme_minimal() +  facet_wrap(~sample + type + n , scales= "free", ncol = 4) + ylim(0, 1) + 
  mytheme + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",  "#8172B2", "coral","#7FB800" , "#FF9999")) +
  scale_colour_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6", "#8172B2", "coral","#7FB800" , "#FF9999"))








