
library(ClassifyR)
library(ggplot2)

clinical_outcome <- read_csv("panel_d_clinical_outcome.csv")

scfeatures_result <- readRDS("panel_d_data.rds")
  
set.seed(1)
  
outcome <- clinical_outcome[match( rownames(scfeatures_result[[1]]), clinical_outcome$patientId), ]$interest
  
scfeatures_result_subset <- lapply(scfeatures_result, function(df) {
    df[which(!is.na(outcome)), ]
})
  
outcome <- outcome[ which(!is.na(outcome))]
  
  
classifyr_result <- crossValidate(scfeatures_result_subset ,
                                    outcome, 
                                    classifier = "SVM",
                                    nFolds = 3, 
                                    nRepeats = 100, 
                                    nCores = 20  )
  
classifyr_result <- lapply(classifyr_result, 
                             function(x) calcCVperformance(x, 
                                                           performanceType = "Balanced Accuracy"))
  
performance_result <-  lapply(classifyr_result, performance)
  
  
  

level_order <- names(scfeatures_result_subset)
  
  

performancePlot(classifyr_result,   characteristicsList = list(x = "auto",
                                                                 fillColour =  "Assay Name", lineColour = "Assay Name"), alpha = 0.3 ) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
    scale_x_discrete(limits = level_order)   + ylim(0, 1)  
  
 
