

accuracy_df <- rbind(
  
  data.frame( model = "scClassify (final model)", 
              accu = mean( final_model$celltype  ==  final_model$celltype_gt )) ,  
  
  data.frame( model = "sctype", 
              accu = mean( sctype$celltype == sctype$celltype_gt )) ,
  
  data.frame( model = "singleR", 
              accu = mean( singleR$celltype  == singleR$celltype_gt))  , 
  
  data.frame( model = "clustifyr", 
              accu = mean( clustifyr$celltype == clustifyr$celltype_gt ))  
  
)


accuracy_df$model <- factor(accuracy_df$model, 
                            levels = c(  "scClassify (final model)", 
                                         "sctype", 
                                         "singleR", 
                                         "clustifyr") )

accuracy_comparison <- ggplot(accuracy_df, aes(x = model, y = accu, fill = model)) + geom_col( alpha = 0.8) + mytheme + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",
                                 "#8172B2", "coral"))


# panel a 
accuracy_comparison


# scatterplot 

colour_choice <-c("#4E79A7" , "#F28E2B",   "#59A14F" ,
                  "#F1CE63", "#499894" , "#E15759",       "#B07AA1" ,  "#FF9D9A" , 
                  "grey")
names(colour_choice) <- c(  "DC_Mast" , "Stromal" ,   "Macrophage"  ,
                            "Malignant" , "Myoepi", "Plasma" ,        "T", "B", 
                            "unassigned" )

comp_df <- NULL

to_plot <-  final_model
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "scClassify (final model)"
comp_df  <- rbind(comp_df ,  temp )




to_plot <- final_model
temp <- data.frame( table(to_plot$celltype_gt) )
temp$model <-  "scClassify ground truth"
comp_df  <- rbind(comp_df ,  temp )





to_plot <- sctype 
temp <- data.frame( table(to_plot$celltype_gt) )
temp$model <- "sctype ground truth"
comp_df  <- rbind(comp_df ,  temp )





to_plot <- sctype 
temp <- data.frame( table(to_plot$celltype) )
temp$model <- "sctype"
comp_df  <- rbind(comp_df ,  temp )






to_plot <- singleR 
temp <- data.frame( table(to_plot$celltype_gt) )
temp$model <- "singleR ground truth"
comp_df  <- rbind(comp_df ,  temp )




to_plot <- singleR 
temp <- data.frame( table( to_plot$celltype ) )
temp$model <- "singleR"
comp_df  <- rbind(comp_df ,  temp )





to_plot <-  clustifyr 
temp <- data.frame( table( to_plot$celltype_gt) )
temp$model <- "clustifyr ground truth"
comp_df  <- rbind(comp_df ,  temp )




to_plot <- clustifyr 
temp <- data.frame( table( to_plot$celltype) )
temp$model <- "clustifyr"
comp_df  <- rbind(comp_df ,  temp )









comp_df$model <- factor( comp_df$model , levels = c( "scClassify ground truth" , 
                                                     "scClassify (final model)", 
                                                     "sctype ground truth", 
                                                     "sctype", 
                                                     "singleR ground truth", 
                                                     "singleR" , 
                                                     "clustifyr ground truth",
                                                     "clustifyr" ) )

composition_comparison <- ggplot(comp_df, aes(x = model, y = Freq, fill = Var1)) + 
  geom_bar(position = "fill", stat = "identity") + 
  scale_fill_manual(values = colour_choice)  + ggtitle("Cell type composition") + mytheme +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) 


# panel b
composition_comparison




svg <- read_csv("svg.csv")

svg$model <- factor( svg$model ,    levels = c( "scClassify", 
                                                "sctype", 
                                                "singler" , 
                                                "clustifyr" 
))

# panel c 
ggplot( svg, aes(x = model, y = corr, fill = model, colour = model)) + 
  geom_boxplot( alpha = 0.7) + 
  theme_minimal() +  facet_wrap(~sample + type + n , scales= "free", ncol = 4) + ylim(0, 1) + 
  mytheme + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  scale_fill_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6",  "#8172B2", "coral")) +
  scale_colour_manual( values = c( "#D43F3B" , "#EEA235" ,  "#05A087" , "#4DBBD6", "#8172B2", "coral"))



