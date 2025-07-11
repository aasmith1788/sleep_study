library (dplyr)
library (ggplot2)
library(nnet)
library(VGAM)
library(tidyr)

# Import the data
data_set <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/sleep_profile-10-11-2024.csv")
data_set$Efficiency_Mean <- as.numeric(data_set$Efficiency_Mean)
data_set$tst_Mean <- as.numeric(data_set$tst_Mean)
data_set$waso_Mean <- as.numeric(data_set$waso_Mean)
data_set$class_eff <- as.factor(data_set$class_eff)
data_set$class_tst <- as.factor(data_set$class_tst)

# SUBSET
data1 <- data_set [data_set$visit=="baseline", ]
data6 <- data_set [data_set$visit=="week6", ]
data12 <- data_set [data_set$visit=="week12", ]
data18 <- data_set [data_set$visit=="week18", ]

# CHANGE
data12$Efficiency_Mean_change <- ifelse(is.na(data12$Efficiency_Mean)| is.na(data1$Efficiency_Mean), NA, data1$Efficiency_Mean - data12$Efficiency_Mean)
data12$tst_Mean_change <- ifelse(is.na(data12$tst_Mean)| is.na(data1$tst_Mean), NA, data1$tst_Mean - data12$tst_Mean)
data12$waso_change <- ifelse(is.na(data12$waso_Mean)| is.na(data1$waso_Mean), NA, data1$waso_Mean - data12$waso_Mean)
data12$dsis_change <- ifelse(is.na(data12$dsis)| is.na(data1$dsis), NA, data1$dsis - data12$dsis)

data12$womac_pain_change <- ifelse(is.na(data12$kd_womac_pain)| is.na(data1$kd_womac_pain), NA, data1$kd_womac_pain - data12$kd_womac_pain)
data12$womac_func_change <- ifelse(is.na(data12$kd_womac_func)| is.na(data1$kd_womac_func), NA, data1$kd_womac_func - data12$kd_womac_func)

# GROC
data12$groc_resp_w6 <- ifelse(data6$groc_score >3, 1, 0)
data12$groc_resp_w6 <- as.factor(data12$groc_resp_w6)

data12$groc_resp_w12 <- ifelse(data12$groc_score >3, 1, 0)
data12$groc_resp_w12 <- as.factor(data12$groc_resp_w12)

# Setting the reference
model <- multinom(class_eff ~ womac_pain_change, data = data12, family = multinomial)
summary(model)
exp(coef(model))
confint(model)

# EFFICIENCY
model <- lm(groc_score ~ Efficiency_Mean, data = data12)
summary(model)



# DEMOGRAPHIC
chisq.test(table(class_eff, sex))

model <- multinom(class_eff ~ age, data = data)
summary(model)
exp(coef(model))
confint(model)

# Eff

