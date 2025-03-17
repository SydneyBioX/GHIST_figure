

mytheme <- theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1))


colour_choice <-c("#4E79A7" ,"#A0CBE8",  "#D37295",
                  "#FFBE7D", "#59A14F",  
                  "#B6992D",    "#F1CE63", "#FF9D9A", 
                  "#86BCB6", "#E15759", "#F28E2B", 
                  "#79706E", 
                  "#BAB0AC", "#FABFD2" , "#B07AA1",
                  "#499894"  )
names(colour_choice) <- c( 
  "B Cells" , "Dendritic Cells", "Endothelial Cells" , 
  "Fibroblasts" ,   "Macrophages", 
  "NK Cells" ,  "Photoreceptor Cells",  "Plasma Cells",
  "RPE Cells" , "T Cells",      "Tumor",   
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

