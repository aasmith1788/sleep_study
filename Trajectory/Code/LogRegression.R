library(dplyr)
library(readxl)
library(ggplot2)
library(broom)
library(ggeffects)
library(nnet) # multinomial logistic regression

# Import the data
outputDir <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results"
data <- read_excel("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/DataSet_02272024.xlsx")

# Data Cleaning
data$tscreen_sexatbirth <- ifelse(data$tscreen_sexatbirth == "Female", 2, 
                   ifelse(data$tscreen_sexatbirth == "Male", 1, data$tscreen_sexatbirth))
colnames(data)[colnames(data) == "tscreen_sexatbirth"] <- "sex"
colnames(data)[colnames(data) == "tscreen_age"] <- "age"
colnames(data)[colnames(data) == "consent_bmi"] <- "bmi"

data$sex <- as.factor (data$sex)
data$race <- as.factor (data$race)
data$kd_womac_func_class <- as.factor(data$kd_womac_func_class)
data$kd_womac_pain_class <- as.factor(data$kd_womac_pain_class)
data$ts_kn_dicho2 <- as.factor(data$ts_kn_dicho2)
data$ts_wrs_dicho2 <- as.factor(data$ts_wrs_dicho2)
data$CPMle1_mean_last2 <- as.factor(data$CPMle1_mean_last2)
data$EIHle1_knee_mean_last2 <- as.factor(data$EIHle1_knee_mean_last2)
data$EIHle1_wrist_mean_last2 <- as.factor(data$EIHle1_wrist_mean_last2)

data$walk6m_mep_max <- as.numeric(data$walk6m_mep_max)
data$s2s_time_mean <- as.numeric(data$s2s_time_mean)
data$isom_q_pktq_max <- as.numeric(data$isom_q_pktq_max)
data$isok60_pktq_away_mean <- as.numeric(data$isok60_pktq_away_mean)
data$koos_sr <- as.numeric(data$koos_sr)
data$koos_qol <- as.numeric(data$koos_qol)


#### Analysis
###### kd_womac_func_class
table(data$kd_womac_func_class) #7, 18, 34

# Average of WOMAC Pain
result <- aggregate(data$kd_womac_func, by=list(data$kd_womac_func_class), FUN=function(x) c(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE)))
print(result)

# PLOT 
ggplot(data %>% filter(kd_womac_func_class == "1" | 
                         kd_womac_func_class == "2"),
       aes(x = as.numeric(koos_pain), y = as.numeric(kd_womac_func_class))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic()

ggplot(data %>% filter(kd_womac_func_class == "2" | 
                         kd_womac_func_class == "3"),
       aes(x = as.numeric(koos_pain), y = as.numeric(kd_womac_func_class))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic()

ggplot(data %>% filter(kd_womac_func_class == "1" | 
                         kd_womac_func_class == "3"),
       aes(x = as.numeric(koos_pain), y = as.numeric(kd_womac_func_class))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic()

# Logitistic
data$kd_womac_func_class <- relevel(data$kd_womac_func_class, ref = "3")

# Patient-reported physical health: WOMAC, PGA-OA, PROMIS Sleep
model <- multinom(kd_womac_func_class ~ age + sex + bmi + kd_womac_pain, data = data) # 1 + 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + kd_womac_stiff, data = data) # 1 + 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + koos_sr, data = data) # 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + koos_adl, data = data) # 1 + 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + koos_qol, data = data) # 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

colnames(data)
model <- multinom(kd_womac_func_class ~ age + sex + bmi + cesd_score_dv, data = data) # 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + paindetect_score, data = data)
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + pcs_score, data = data) # 1 + 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

# Sensor Data
model <- multinom(kd_womac_func_class ~ age + sex + bmi + Total_MVPA_Mean, data = data) # 1 vs 3
summary(model)
tidy(model, conf.int = TRUE)

# Physical Performance Data
model <- multinom(kd_womac_func_class ~ age + sex + bmi + gait_28m_time_mean_minus30, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + s2s_time_mean, data = data) # 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + sct_time_mean, data = data) # 2 vs 3
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + stup_oa_reps_mean, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + walk6m_dist, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + isom_pf_pktq_max, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + isom_q_pktq_max, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + isok60_pktq_away_mean, data = data) 
summary(model)
tidy(model, conf.int = TRUE)


# QST Data
model <- multinom(kd_womac_func_class ~ age + sex + bmi + ts_kn_dicho2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + ts_wrs_dicho2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + ppt_knee_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + ppt_wrist_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + CPMle1_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + CPMpct_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + EIHle1_knee_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + EIHpct_knee_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + EIHle1_wrist_mean_last2, data = data) # 1 vs 3
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_func_class ~ age + sex + bmi + EIHpct_wrist_mean_last2, data = data) # 1 vs 3
summary(model)
tidy(model, conf.int = TRUE)

# Predicted Probability
ggeffect(model, terms = "Total_MVPA_Mean") %>%
  plot()


###### kd_womac_pain_class
table(data$kd_womac_pain_class) #13, 4, 14, 28

# Average of WOMAC Pain
result <- aggregate(data$kd_womac_pain, by=list(data$kd_womac_pain_class), FUN=function(x) c(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE)))
print(result)

# PLOT: Change x
ggplot(data %>% filter(kd_womac_pain_class == "1" | 
                         kd_womac_pain_class == "2"),
       aes(x = as.numeric(koos_pain), y = as.numeric(kd_womac_pain_class))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic()

ggplot(data %>% filter(kd_womac_pain_class == "2" | 
                         kd_womac_pain_class == "3"),
       aes(x = as.numeric(koos_pain), y = as.numeric(kd_womac_pain_class))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic()

ggplot(data %>% filter(kd_womac_pain_class == "1" | 
                         kd_womac_pain_class == "3"),
       aes(x = as.numeric(koos_pain), y = as.numeric(kd_womac_pain_class))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme_classic()



### Logitistic
# Relevel the kd_womac_pain_class #13, 4, 14, 28
data$kd_womac_pain_class <- relevel(data$kd_womac_pain_class, ref = "4")

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + Total_MVPA_Mean, data = data)
summary(model)
tidy(model, conf.int = TRUE)

# QST Data
model <- multinom(kd_womac_pain_class ~ age + sex + bmi + ts_kn_dicho2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + ts_wrs_dicho2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + ppt_knee_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + ppt_wrist_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + CPMle1_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + CPMpct_mean_last2, data = data) # 1+2 vs 4
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + EIHle1_knee_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + EIHpct_knee_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + EIHle1_wrist_mean_last2, data = data) # 2 vs 4
summary(model)
tidy(model, conf.int = TRUE)

model <- multinom(kd_womac_pain_class ~ age + sex + bmi + EIHpct_wrist_mean_last2, data = data) 
summary(model)
tidy(model, conf.int = TRUE)


# Predicted Probability
ggeffect(model, terms = "Total_MVPA_Mean") %>%
  plot()




