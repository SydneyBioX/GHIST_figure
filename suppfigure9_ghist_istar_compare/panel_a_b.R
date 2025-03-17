

top20_SVG <- read_csv("top20_SVG.csv")

# panel a left 
ggplot(top20_SVG   , aes(x = method , y =  per_gene_cor, fill = method  , colour = method )  ) + 
  geom_boxplot( alpha = 0.4)   + mytheme +
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235"  )) + 
  scale_colour_manual( values = c( "#D43F3B" , "#EEA235"  ))





top20_nonSVG <- read_csv("top20_nonSVG.csv")

# panel a right 
ggplot( top20_nonSVG  , aes(x = method , y =  per_gene_cor, fill = method  , colour = method )  ) + 
  geom_boxplot( alpha = 0.4)   + mytheme +
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235"  )) + 
  scale_colour_manual( values = c( "#D43F3B" , "#EEA235"  ))






ground_truth <- read_csv("ground_truth.csv")


gt_plot <- ggplot(ground_truth, 
                                aes(x = x, y = y, fill = exprs, colour =   exprs) ) + 
  geom_tile() + scale_colour_viridis()+  scale_fill_viridis() + ggtitle("GHIST ground truth") + mytheme


 
ghist_pred <- read_csv("ghist_pred.csv")

ghist_pred_plot <- ggplot( ghist_pred , 
                                  aes(x = x, y = y, fill = exprs  , colour =   exprs) ) + 
  geom_tile() + scale_colour_viridis()+  scale_fill_viridis()  + ggtitle("GHIST prediction") + mytheme



istar_pred <- read_csv("istar_pred.csv")

istar_pred_plot <- ggplot( istar_pred  , 
                                  aes(x = x, y = y, fill =  exprs , colour =   exprs) ) + 
  geom_tile() + scale_colour_viridis()+  scale_fill_viridis() + ggtitle("istar prediction")  + mytheme



# panel b 
ggarrange(plotlist = list( gt_plot  ,
                          ghist_scatter_pred_plot ,
                          istar_scatter_pred_plot
), ncol = 3)


