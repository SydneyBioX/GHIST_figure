


data <- read_csv("data.csv")


ggplot(combined_df, aes( logFC, -log10(p_adj.glb)   ))+
  geom_point(aes(colour=-log10(p_adj.glb)), alpha=1/3, size=1) +
  scale_colour_gradient(low="blue",high="red")+
  xlab("log2 fold change") + ylab("-log10 p-value") +  facet_wrap(~cluster_id, ncol = 3, nrow = 3, scales= "free") + mytheme 
