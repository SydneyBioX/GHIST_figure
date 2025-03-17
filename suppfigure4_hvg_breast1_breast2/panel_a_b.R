


data <- read_csv("panel_a_data.csv")

# panel a 
ggplot(data, aes(x = n, y = corr, fill = n , 
                                  colour = n )) +
  geom_boxplot( alpha = 0.7) + 
  theme_minimal() +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + facet_wrap(~sample   , scales= "free", ncol = 4)  




draw_scatter <- function(data){
  
  ggplot( data, aes(x = gt, y = pred, colour = pred)) + geom_scattermore() +  
    stat_cor( aes(label = ..r.label..) , method = "pearson" ) + theme_minimal() + scale_colour_viridis_c() + 
    ggtitle( unique( data $this_gene)) + 
    ylab ("Predicted expression") + xlab("Ground truth expression")
  
}


IL7R <- read_csv("IL7R.csv")
CD3E <- read_csv("CD3E.csv")
LUM <- read_csv("LUM.csv")
ABCC11 <- read_csv("ABCC11.csv")
SCD <- read_csv("SCD.csv")
SERPINA3 <- read_csv("SERPINA3.csv")

# panel b 

draw_scatter(IL7R)

draw_scatter(CD3E)

draw_scatter(LUM)

draw_scatter(ABCC11)

draw_scatter(SCD)

draw_scatter(SERPINA3)



