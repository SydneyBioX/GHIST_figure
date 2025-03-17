
library(ComplexHeatmap)


data <- read_csv("panel_b_data.csv")


cols <- c(   "#F8766D"   ,      "#00BFC4" )
names(cols ) <- c("ER_Negative_and_PR_Negative" , "ER_Positive_and_PR_Positive" )

group_id  <- read_csv( "panel_b_group_id.csv")

col_anno <- columnAnnotation(
  df = data.frame(group_id = group_id$x ),
  col = list(group_id = cols),
  gp = gpar(col = "white"),
  show_annotation_name = FALSE,
  annotation_legend_param = lgd_aes)





cols <- c( "#F8766D" ,  "#00BFC4"  )
names(cols) <- c("Macrophage" , "Stromal") 


row_anno <- rowAnnotation(
  df = data.frame(cluster_id =  
                    c("Macrophage", "Macrophage" ,"Macrophage" ,"Macrophage", "Stromal" )),
  col = list(cluster_id = cols),
  gp = gpar(col = "white"),
  show_annotation_name = FALSE )



 

col <-  c( "#440154FF" ,"#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF",
           "#1F9E89FF" ,"#35B779FF" ,"#6DCD59FF", "#B4DE2CFF", "#FDE725FF" )

# panel b 

 
Heatmap(matrix = data , col = col, 
        row_title = NULL, column_title = NULL,
        cluster_rows = FALSE, cluster_columns = FALSE,
        left_annotation = row_anno ,
        top_annotation = col_anno,
        split =  c("Macrophage"  , "Macrophage" , "Macrophage" , "Macrophage" , "Stromal") ,
        column_split = ei$group_id[m],
        row_names_gp = gpar(fontsize = 6), 
        column_names_gp = gpar(fontsize = 8),
        column_title_gp = gpar(fontface = "bold", fontsize = 10))

 




index <- which(group_id$x == "ER_Positive_and_PR_Positive" )

input <- data[ ,  index ]
 
# panel c 
Heatmap(matrix = input, col = col, 
        clustering_distance_columns =   "pearson",
        cluster_rows = FALSE, cluster_columns = T,
        left_annotation = row_anno , 
        top_annotation = col_anno[ index],
        split =  c("Macrophage"  , "Macrophage" , "Macrophage" , "Macrophage" , "Stromal") ,
        row_names_gp = gpar(fontsize = 6), 
        column_names_gp = gpar(fontsize = 8),
        column_title_gp = gpar(fontface = "bold", fontsize = 10),
        column_km = 2,
        show_column_names = T,
        column_km_repeats = 100)









