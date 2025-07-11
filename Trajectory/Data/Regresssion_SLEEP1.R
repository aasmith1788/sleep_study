library (dplyr)
library (ggplot2)
library(nnet)
library(VGAM)
library(MASS)
library(tidyr)

# Import the data
data_set <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Sleep Trajectory/Data/sleep_profile_gait_eff_tst_waso.csv")
data_set$sex <- as.factor(data_set$sex)
data_set$Efficiency_Mean <- as.numeric(data_set$Efficiency_Mean)
data_set$tst_Mean <- as.numeric(data_set$tst_Mean)
data_set$waso_Mean <- as.numeric(data_set$waso_Mean)

# ClassMembership
data_set$class_eff4 <- as.factor(data_set$class_eff4)
data_set$class_waso3 <- as.factor(data_set$class_waso3)
data_set$class_waso4 <- as.factor(data_set$class_waso4)

# SUBSET
data1 <- data_set [data_set$week=="baseline", ]
data6 <- data_set [data_set$week=="week6", ] 
data12 <- data_set [data_set$week=="week12", ]
data18 <- data_set [data_set$week=="week18", ] 

# CHANGE: BASELINE - WEEK12 
data1$Efficiency_change <- data12$Efficiency_Mean - data1$Efficiency_Mean
data1$Efficiency_perchange <- ((data12$Efficiency_Mean - data1$Efficiency_Mean)/data1$Efficiency_Mean)*100
data1$tst_Mean_change <-  data12$tst_Mean - data1$tst_Mean
data1$tst_Mean_perchange <-  ((data12$tst_Mean - data1$tst_Mean)/data1$tst_Mean)*100
data1$waso_change <- data12$waso_Mean - data1$waso_Mean
data1$waso_perchange <- ((data12$waso_Mean - data1$waso_Mean)/data1$waso_Mean)*100
data1$dsis_change <- data12$dsis - data1$dsis
data1$dsis_perchange <- ((data12$dsis - data1$dsis)/data1$dsis)*100

data1$womac_pain_change <- data12$kd_womac_pain - data1$kd_womac_pain
data1$womac_pain_perchange <- ((data12$kd_womac_pain - data1$kd_womac_pain)/data1$kd_womac_pain)*100
data1$womac_func_change <- data12$kd_womac_func - data1$kd_womac_func
data1$womac_func_perchange <- ((data12$kd_womac_func - data1$kd_womac_func)/data1$kd_womac_func)*100

# GROC
data1$groc_resp_w6 <- ifelse(data6$groc_score >3, 1, 0)
data1$groc_resp_w6 <- as.factor(data1$groc_resp_w6)

data1$groc_resp_w12 <- ifelse(data12$groc_score >3, 1, 0)
data1$groc_resp_w12 <- as.factor(data1$groc_resp_w12)
## 
keywords <- c("record_id", "tst", "tib", "waso", "Efficiency", "Sleep_Fragmentation_Index", "womac", "cesd", "nrs")
col <- grep(paste(keywords, collapse = "|"), colnames(data12), value = TRUE)

filtered_data12 <- data12[, col]
combined_data <- merge(
  data1, 
  filtered_data12, 
  by = "record_id",
  suffixes = c("_data1", "_data12")
)

combined_data

## 
data1_eff1 <- combined_data[combined_data$class_eff4==1, ] # SLIGHT IMRPOVEMENT 8%
data1_eff2 <- combined_data[combined_data$class_eff4==2, ] # NO CHNAGE 
data1_eff3 <- combined_data[combined_data$class_eff4==3, ] # WORSENING
data1_eff4 <- combined_data[combined_data$class_eff4==4, ] # HUGE IMPROVEMENT


# EFFICIENCY CHANGE
mean(data1_eff1$Efficiency_Mean_data1, na.rm=TRUE)
mean(data1_eff2$Efficiency_Mean_data1, na.rm=TRUE)
mean(data1_eff3$Efficiency_Mean_data1, na.rm=TRUE)
mean(data1_eff4$Efficiency_Mean_data1, na.rm=TRUE)

mean(data1_eff1$Efficiency_Mean_data12, na.rm=TRUE)
mean(data1_eff2$Efficiency_Mean_data12, na.rm=TRUE)
mean(data1_eff3$Efficiency_Mean_data12, na.rm=TRUE)
mean(data1_eff4$Efficiency_Mean_data12, na.rm=TRUE)

mean(data1_eff1$Efficiency_perchange, na.rm=TRUE)
mean(data1_eff2$Efficiency_perchange, na.rm=TRUE)
mean(data1_eff3$Efficiency_perchange, na.rm=TRUE)
mean(data1_eff4$Efficiency_perchange, na.rm=TRUE)

mean(data1_eff1$womac_pain_perchange, na.rm=TRUE)
mean(data1_eff2$womac_pain_perchange, na.rm=TRUE)
mean(data1_eff3$womac_pain_perchange, na.rm=TRUE)
mean(data1_eff4$womac_pain_perchange, na.rm=TRUE)

mean(data1_eff1$cesd_score_dv, na.rm=TRUE)
mean(data1_eff2$cesd_score_dv, na.rm=TRUE)
mean(data1_eff3$cesd_score_dv, na.rm=TRUE)
mean(data1_eff4$cesd_score_dv, na.rm=TRUE)

mean(data1_eff1$oa_cadence_mean, na.rm=TRUE)
mean(data1_eff2$oa_cadence_mean, na.rm=TRUE)
mean(data1_eff3$oa_cadence_mean, na.rm=TRUE)
mean(data1_eff4$oa_cadence_mean, na.rm=TRUE)

## 
data1_waso1 <- data1[data1$class_waso4==1, ]
data1_waso2 <- data1[data1$class_waso4==2, ]  
data1_waso3 <- data1[data1$class_waso4==3, ] 
data1_waso4 <- data1[data1$class_waso4==4, ] 



# WASO CHANGE
mean(data1_waso1$waso_Mean, na.rm=TRUE)
mean(data1_waso2$waso_change, na.rm=TRUE)
mean(data1_waso3$waso_change, na.rm=TRUE)
mean(data1_waso4$waso_Mean, na.rm=TRUE)

median(data1_waso1$kd_, na.rm=TRUE)
median(data1_waso2$waso_perchange, na.rm=TRUE)
median(data1_waso3$waso_perchange, na.rm=TRUE)
median(data1_waso4$waso_perchange, na.rm=TRUE)

# 
library(stargazer)
data1$class_eff4 = relevel(data1$class_eff4, ref = 3)
# STEP SYMM
multi1 = multinom(class_eff4 ~ step_symm_mean + age+ sex+ bmi, data=data1)
summary(multi1)
stargazer(multi1, type="text", out="logit.htm")
# relative risk ratio
multi1.rrr = exp(coef(multi1))
stargazer(multi1, type="text", coef=list(multi1.rrr), p.auto=FALSE, out="multi1rrr.htm")

# CESD
multi1 = multinom(class_eff4 ~ cesd_score_dv + age+ sex+ bmi, data=data1)
summary(multi1)
stargazer(multi1, type="text", out="logit.htm")
# relative risk ratio
multi1.rrr = exp(coef(multi1))
stargazer(multi1, type="text", coef=list(multi1.rrr), p.auto=FALSE, out="multi1rrr.htm")

# CESD
multi1 = multinom(class_eff4 ~ kd_womac_pain + age+ sex+ bmi, data=data1)
summary(multi1)
stargazer(multi1, type="text", out="logit.htm")
# relative risk ratio
multi1.rrr = exp(coef(multi1))
stargazer(multi1, type="text", coef=list(multi1.rrr), p.auto=FALSE, out="multi1rrr.htm")

