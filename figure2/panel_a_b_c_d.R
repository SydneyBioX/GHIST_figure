

library(ggplot2)
library(scattermore)

colour_choice <-c("#4E79A7" , "#F28E2B",  "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",    
                  "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,  "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,     
                            "T", "B", 
                            "unassigned" )




to_plot <- read_csv("panel_a_left_data.csv")

# panel a left 
ggplot(to_plot, aes(x = x_cord, y = y_cord, colour = group_broad_type)) + 
  geom_scattermore(pointsize = 1.3) + theme_minimal() + 
  scale_colour_manual(values = colour_choice ) + ggtitle("Ground truth")


to_plot <- read_csv("panel_a_right_data.csv")

# panel a right
ggplot(to_plot, aes(x = x_cord, y = y_cord, colour = group_broad_type)) + 
  geom_scattermore(pointsize = 1.3) + theme_minimal() + 
  scale_colour_manual(values = colour_choice ) + ggtitle("Predicted")



gt <- read_csv("panel_b_ground_truth_data.csv")
gt$model <- "ground truth"
pred <- read_csv("panel_b_pred_data.csv")
pred$model <- "predicted"

comp_df <- rbind( gt,   pred )
 
# panel b  
ggplot(comp_df, aes(x =  model, y = Freq, fill = Var1)) + 
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() + 
  scale_fill_manual(values = colour_choice) 




to_plot <- read_csv("panel_c_left_data.csv")

# panel c left 
ggplot(to_plot, aes(x = x_cord, y = y_cord, colour = group_broad_type)) + 
  geom_scattermore(pointsize = 1.3) + theme_minimal() + 
  scale_colour_manual(values = colour_choice ) + ggtitle("Ground truth")


to_plot <- read_csv("panel_c_right_data.csv")

# panel c right
ggplot(to_plot, aes(x = x_cord, y = y_cord, colour = group_broad_type)) + 
  geom_scattermore(pointsize = 1.3) + theme_minimal() + 
  scale_colour_manual(values = colour_choice ) + ggtitle("Predicted")



gt <- read_csv("panel_d_ground_truth_data.csv")
gt$model <- "ground truth"
pred <- read_csv("panel_d_pred_data.csv")
pred$model <- "predicted"

comp_df <- rbind( gt,   pred )

# panel c 
ggplot(comp_df, aes(x =  model, y = Freq, fill = Var1)) + 
  geom_bar(position = "fill", stat = "identity") + 
  theme_minimal() + 
  scale_fill_manual(values = colour_choice) 













