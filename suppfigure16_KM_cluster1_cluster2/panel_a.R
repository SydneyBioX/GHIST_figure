

library(edgeR)
library(dplyr)
library(ggplot2)
library(survival)
library(glmnet)
library(survminer)



data <- read_csv( "panel_a_right_data.csv")


surv_obj <- survival::Surv(data$days_to_last_follow_up, data$vital_status)

surv_fit = survival::survfit(
  survival::Surv(days_to_last_follow_up, vital_status) ~ cluster,
  data = data
)


logrank_test <- survdiff(
  Surv(days_to_last_follow_up, vital_status) ~   cluster , 
  data = data)
print( broom::glance(   logrank_test )  )



# panel a right 
survminer:: ggsurvplot(
  fit = surv_fit,
  data = data,
  risk.table = FALSE,
  surv.median.line = "hv",
  pval = TRUE,
  palette  = c("#9b3a89" , "#fbb040") , 
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






data <- read_csv( "panel_a_left_data.csv")


surv_obj <- survival::Surv(data$days_to_last_follow_up, data$vital_status)

surv_fit = survival::survfit(
  survival::Surv(days_to_last_follow_up, vital_status) ~ ER_PR,
  data = data
)


logrank_test <- survdiff(
  Surv(days_to_last_follow_up, vital_status) ~   cluster , 
  data = data)
print( broom::glance(   logrank_test )  )


# panel a left 

survminer:: ggsurvplot(
  fit = surv_fit,
  data = data,
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








