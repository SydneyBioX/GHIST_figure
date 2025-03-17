


library(survival)
library(survminer)


draw_surv_plot <- function(surv_data ){
  
  
  surv_data <- surv_data[ surv_data$risk_cat  %in% c("high risk", "low risk"),  ]
  
  
  surv_obj <- Surv(surv_data$days_to_last_follow_up, surv_data$vital_status)
  
  
  surv_fit = survfit(
    Surv(days_to_last_follow_up, vital_status) ~ risk_cat,
    data = surv_data
  )
  
  logrank_test <- survdiff(
    Surv(days_to_last_follow_up, vital_status) ~   risk_cat, 
    data = surv_data)
  print( broom::glance(   logrank_test )  )
  
  
  p <- ggsurvplot(
    fit = surv_fit,
    data = surv_data,
    risk.table = FALSE,
    surv.median.line = "hv",
    pval = TRUE,
    # pval.method = TRUE,
    pval.coord = c(0, 0.25),
    conf.int = TRUE,
    legend.title = "Risk Group",
    xlab = "Time (years)",
    ylab = "Survival Probability",
    title = "Kaplan-Meier Survival Curves by Risk Group",
    xscale = "d_y" ,
    break.time.by=365.25*2
  )
  
  return(p )
  
  
}


bulk_RNAseq <- read_csv("bulk_RNAseq.csv")

draw_surv_plot(bulk_RNAseq)



gene_cor_celltype <- read_csv("gene_cor_celltype.csv")

draw_surv_plot(gene_cor_celltype)





moran_I <- read_csv("moran_I.csv")

draw_surv_plot(moran_I)



 
nn_correlation <- read_csv("nn_correlation.csv")

draw_surv_plot(nn_correlation)





