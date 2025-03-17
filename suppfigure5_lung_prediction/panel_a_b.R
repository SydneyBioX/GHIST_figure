
colour_choice <-c("#4E79A7" , "#F28E2B" ,
                  "#59A14F" ,    "#F1CE63", "#E15759",  
                  "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c( "Epithelium" ,  "Blood vessels"  ,     
                           "Fibroblast lineage" , "Lymphatic EC", "Lymphoid" ,  
                           "Myeloid", "Smooth muscle", 
                           "unassigned"  )


# panel a right 
pred <- read_csv("pred.csv")

ggplot(pred , aes(x = x_cord, y = y_cord, colour = group_broad_type)) + 
  geom_scattermore(pointsize = 1.3) + theme_minimal() + 
  scale_colour_manual(values = colour_choice ) + mytheme + ggtitle("predicted")



# panel a left 
gt <- read_csv("gt.csv")

ggplot(gt , aes(x = x_cord, y = y_cord, colour = group_broad_type)) + 
  geom_scattermore(pointsize = 1.3) + theme_minimal() + 
  scale_colour_manual(values = colour_choice ) + mytheme + ggtitle("ground truth")




combined <- read_csv("combined.csv")

# panel b 
ggplot( combined  , aes(x = Proportion.x, y =Proportion.y, 
                        fill = Var1 , colour = Var1, label = Var1)) + geom_point() + 
  theme_minimal() +   
  scale_fill_manual(values = colour_choice ) +
  scale_colour_manual( values = colour_choice ) + 
  ggrepel::geom_text_repel() +  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey")

