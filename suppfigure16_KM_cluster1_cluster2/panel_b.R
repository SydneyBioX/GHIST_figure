

data <- read_csv("panel_b_left_data.csv")


a <- ggplot(data, aes(x = variable, y = value, colour = cluster, fill = cluster)) + 
  geom_boxplot(alpha = 0.4)  + mytheme + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_colour_manual(values = c( "#9c3b8a" , "#fbb042")) + scale_fill_manual(values = c( "#9c3b8a" , "#fbb042"))

# panel b left 
a



data <- read_csv("panel_b_right_data.csv")


b <- ggplot(data, aes(x = variable, y = value, colour = cluster, fill = cluster)) + 
  geom_boxplot(alpha = 0.4)  + mytheme + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + scale_colour_manual(values = c( "#9c3b8a" , "#fbb042")) + scale_fill_manual(values = c( "#9c3b8a" , "#fbb042"))
# panel b right
b