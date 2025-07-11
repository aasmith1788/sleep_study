library(dplyr)
library(ggplot2)
library(ggthemes)
#
library(ggplot2)
library(lcmm)
library(gbmt) # Group-Based Multivariate Trajectory Modeling
# library(mlogit) # multinomial logistic regression

40^2
1-pnorm(1.99)
# Import the data
outputDir <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Sleep Trajectory/Results"
outputDirFig <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Sleep Trajectory/Results/Figures"

##  IMPORT DATA 
weekly_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/gt9x_sleep_dsis_koos_pgaoa_weekly.csv")
weekly_data$dsis <- as.numeric(weekly_data$dsis)

weekly_data0 <- weekly_data %>% filter (redcap_event_name == 0)
weekly_data12 <- weekly_data %>% filter (redcap_event_name == 12)

# GT9X SLEEP COL
sleep_col <- c("Efficiency_Mean", "tst_Mean", "waso_Mean", "Sleep_Fragmentation_Index_Mean")
gt9x_baseline <- weekly_data0 %>%
  group_by(record_id) %>%
  summarise(across(all_of(sleep_col), \(x) mean(x, na.rm = TRUE)))

# KOOS PGAOA
koos_col <- c("record_id", "redcap_event_name", "dsis", "koos_pain", "koos_adl","kd_womac_pain","kd_womac_stiff", "kd_womac_func", "pgaoa")
koos_baseline <- weekly_data0[, koos_col]
koos_baseline <- koos_baseline %>% distinct()

# BASLINE
BASELINE <- gt9x_baseline %>% left_join(koos_baseline, by = 'record_id') %>%
  mutate(across(everything(), ~ replace(., . == ".", NA)))

# 
weekly_data <- weekly_data %>% filter (redcap_event_name>0) %>% filter (redcap_event_name<=12)
koos_weekly <- weekly_data[, koos_col]
dsis_weekly <- rbind(koos_baseline, koos_weekly)
dsis_weekly <- dsis_weekly  %>%  mutate(across(everything(), ~ replace(., . == ".", NA)))

# 
WEEK12 <- weekly_data12[, c(koos_col, sleep_col)] 
WEEK12 <- WEEK12 %>% mutate(across(everything(), ~ replace(., . == ".", NA)))

#
combined_data <- left_join(
  BASELINE, 
  WEEK12, 
  by = "record_id", 
  suffix = c("_w0", "_w12")
  ) %>%
  mutate(across(matches("_w0$|_w12$"), ~ as.numeric(as.character(.))))

colnames(combined_data)
# CALCULATE CHANGE
change_col <- c("Efficiency_Mean", "tst_Mean", "waso_Mean", "Sleep_Fragmentation_Index_Mean", 
                "dsis", "koos_pain", "koos_adl","kd_womac_pain","kd_womac_stiff", "kd_womac_func", "pgaoa")

for (col in valid_change_col) {
  w0_col <- paste0(col, "_w0") 
  w12_col <- paste0(col, "_w12")  
  change_col <- paste0(col, "_change")  
  

  if (w0_col %in% colnames(combined_data) & w12_col %in% colnames(combined_data)) {
    combined_data[[change_col]] <- combined_data[[w12_col]] - combined_data[[w0_col]]
  } else {
    message(paste("Columns missing for", col, "- skipping"))
  }
}
mean(combined_data$waso_Mean_change, na.rm=TRUE)

model <- lm(Efficiency_Mean_change ~ kd_womac_pain_change, data = combined_data)
model <- lm(tst_Mean_change ~ kd_womac_pain_change, data = combined_data)
model <- lm(waso_Mean_change ~ kd_womac_pain_change, data = combined_data)
summary(model)


#####################################
# GT9X
gt9x_weekly_data <- weekly_data %>% 
  filter(redcap_event_name != "") %>%
  filter(redcap_event_name != 0)

##### DSIS
dsis_col <- c('record_id', 'redcap_event_name', 'dsis','dsis_mean')
gt9x_dsis <- gt9x_weekly_data[, dsis_col] %>% filter(!is.na(dsis))

# LCMM uses maxium likelihood estimation (MLE) approach to estimate missingness
mlin <- lcmm(dsis ~ redcap_event_name , random = ~ redcap_event_name, subject = "record_id", data = gt9x_dsis)
mbeta <- lcmm(dsis ~ redcap_event_name , random = ~ redcap_event_name, subject = "record_id", link = 'beta', data = gt9x_dsis) 
mspl <- lcmm(dsis ~ redcap_event_name , random = ~ redcap_event_name, subject = "record_id", link = 'splines', data = gt9x_dsis) 
mspl5 <- lcmm(dsis ~ redcap_event_name , random = ~ redcap_event_name, subject = "record_id", link = '5-quant-splines', data = gt9x_dsis) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "conv", "npm", "AIC", "SABIC")) # LOWER THE BETTER


##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Quadratic
f_mspl1 <- lcmm(dsis ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_dsis) 
f_mspl2 <- lcmm(dsis ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_dsis, ng = 2, B = f_mspl1, mixture = ~ redcap_event_name) 
f_mspl3 <- lcmm(dsis ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_dsis, ng = 3, B = f_mspl1, mixture = ~ redcap_event_name)  
f_mspl4 <- lcmm(dsis ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_dsis, ng = 4, B = f_mspl1, mixture = ~ redcap_event_name) 
f_mspl5 <- lcmm(dsis ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_dsis, ng = 5, B = f_mspl1, mixture = ~ redcap_event_name) 
# f_linear6 <- lcmm(dsis ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_dsis, ng = 6, B = f_linear1, mixture = ~ redcap_event_name) 

# Compare models; AIC/BIC/SABIC (smaller, better fitting), 
# entropy (>0.8, well-separated class), ICL1/2 (lower, better balance between model fit)
summarytable(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5, 
            which=c("loglik", "conv", "npm", "AIC", "BIC","entropy","ICL", "SABIC"), bty="l",pch=20,col=2)
summary(f_mspl5)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_mspl5)

# PLOT
plot(f_mspl5, which = "fit",  var.time= "redcap_event_name")
# PLOT
png(paste0(outputDirFig, "/ClassMembership_dsis_beta5_weekly.png"), width = 2000, height = 2000, res = 300)
par(cex = 0.7)
plot(v, which = "fit",  var.time= "redcap_event_name")
par(cex=1)


##### Efficiency Mean
Eff_col <- c('record_id', 'redcap_event_name', 'Efficiency_Mean')
gt9x_Efficiency <- gt9x_weekly_data[, Eff_col] %>% filter(!is.na(Efficiency_Mean))

table(gt9x_Efficiency$redcap_event_name)
data_per_participant <- gt9x_weekly_data %>%
  group_by(record_id) %>%
  summarise(data_points = sum(!is.na(Efficiency_Mean))) %>%
  arrange(desc(data_points))

gt9x_Efficiency[gt9x_Efficiency$record_id==62, ]


# LCMM uses maxium likelihood estimation (MLE) approach to estimate missingness
mlin <- lcmm(Efficiency_Mean ~ redcap_event_name , random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency)
mbeta <- lcmm(Efficiency_Mean ~ redcap_event_name , random = ~ redcap_event_name, subject = "record_id", link = 'beta', data = gt9x_Efficiency) 
mspl <- lcmm(Efficiency_Mean ~ redcap_event_name , random = ~ redcap_event_name, subject = "record_id", link = 'splines', data = gt9x_Efficiency) 
mspl5 <- lcmm(Efficiency_Mean ~ redcap_event_name , random = ~ redcap_event_name, subject = "record_id", link = '5-quant-splines', data = gt9x_Efficiency) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "conv", "npm", "AIC", "SABIC")) # LOWER THE BETTER


##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Quadratic
f_mspl1 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "splines", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_Efficiency) 
f_mspl2 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "splines", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 2, B = f_mspl1, mixture = ~ redcap_event_name) 
f_mspl3 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "splines", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 3, B = f_mspl1, mixture = ~ redcap_event_name)  
f_mspl4 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "splines", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 4, B = f_mspl1, mixture = ~ redcap_event_name) 
f_mspl5 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "splines", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 5, B = f_mspl1, mixture = ~ redcap_event_name) 
# f_linear6 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 6, B = f_linear1, mixture = ~ redcap_event_name) 

# Compare models; AIC/BIC/SABIC (smaller, better fitting), 
# entropy (>0.8, well-separated class), ICL1/2 (lower, better balance between model fit)
summarytable(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5, 
            which=c("loglik", "conv", "npm", "AIC", "BIC","entropy","ICL", "SABIC"), bty="l",pch=20,col=2)
summary(f_linear5)
# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_mspl5)

# PLOT
plot(f_mspl5, which = "fit",  var.time= "redcap_event_name")

# Contingency table of two classifications
xclass(f_linear4, f_linear5)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_linear4)

# PLOT
plot(f_linear4, which = "fit",  var.time= "redcap_event_name")

# Classification and  Export
eff_mean <- f_linear3$pprob
write.csv(eff_mean, file = file.path(outputDir, "/ClassMembership_Eff_weekly.csv"), row.names = FALSE)

##### Efficiency
gt9x_Efficiency <- gt9x_daily_data %>%
  filter(!is.na(Efficiency),  
         redcap_event_name >= -6 & redcap_event_name <= 125)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(Efficiency ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_Efficiency) 
f_beta2 <- lcmm(Efficiency ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 2, B = f_beta1, mixture = ~ redcap_event_name)
f_beta3 <- lcmm(Efficiency ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 3, B = f_beta1, mixture = ~ redcap_event_name)

# Linear 
f_linear1 <- lcmm(Efficiency ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_Efficiency) 
f_linear2 <- lcmm(Efficiency ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 2, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear3 <- lcmm(Efficiency ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 3, B = f_linear1, mixture = ~ redcap_event_name)  
f_linear4 <- lcmm(Efficiency ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 4, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear5 <- lcmm(Efficiency ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 5, B = f_linear1, mixture = ~ redcap_event_name) 
# f_linear6 <- lcmm(Efficiency ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 6, B = f_linear1, mixture = ~ redcap_event_name) 
# f_linear7 <- lcmm(Efficiency ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency, ng = 7, B = f_linear1, mixture = ~ redcap_event_name) 

# Compare models; AIC/BIC/SABIC (smaller, better fitting), 
# entropy (>0.8, well-separated class), ICL1/2 (lower, better balance between model fit)
summarytable(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5, 
            which=c("loglik", "SABIC", "BIC","entropy"), bty="l",pch=20,col=2)
summary(f_linear3)

# Contingency table of two classifications
xclass(f_linear3, f_linear5)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_linear3)

# PLOT
plot(f_linear3, which = "fit",  var.time= "redcap_event_name")

# Classification and  Export
eff <- f_linear3$pprob
write.csv(eff, file = file.path(outputDir, "/ClassMembership_Eff_daily.csv"), row.names = FALSE)

# 
outputDirFig <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results/Figures"

####### Efficiency + LCMM group 
daily_eff_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results/gt9x_sleep_lcmm_daily.csv")
daily_eff_data$class_tst <- as.factor(daily_eff_data$class_tst)
daily_eff_data$class_eff <- as.factor(daily_eff_data$class_eff)

# Average EFF
avg_eff <- daily_eff_data %>%
  group_by(class_eff, redcap_event_name) %>%
  summarise(n = n_distinct(record_id),
            avg_Efficiency = mean(Efficiency, na.rm=TRUE), 
            sd_Efficiency = sd(Efficiency, na.rm = TRUE), 
            count = sum(!is.na(Efficiency)), 
            se_Efficiency = avg_Efficiency / sqrt(count),  
            ci_lower = avg_Efficiency - (1.96 * se_Efficiency),  
            ci_upper = avg_Efficiency + (1.96 * se_Efficiency))

avg_eff <- avg_eff_id %>%
  group_by(class, visit) %>%
  summarise(n = n_distinct(record_id),
    avg_eff = mean(avg_Efficiency, na.rm=TRUE), 
            sd_eff = sd(avg_Efficiency, na.rm = TRUE), 
            count = sum(!is.na(avg_Efficiency)), 
            se_eff = sd_eff / sqrt(count),  
            ci_lower = avg_eff - (1.96 * se_eff),  
            ci_upper = avg_eff + (1.96 * se_eff),
            .groups = "drop") 


# Efficiency Lint Plot
eff_plot <- ggplot(avg_eff, aes(x = redcap_event_name, y = avg_Efficiency, color = class_eff, group = class_eff)) +  
  geom_line(size = 1) +
  geom_point(size = 2) +
  xlim(0,125)+
  # geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Sleep Efficiency",
       y = "Sleep Efficiency (%)") + 
  theme_light() +
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", color = "black", size = 0.2),
        strip.text.x = element_text(size = 15, face = "bold", color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = c('black', 'red', 'green'),
                     labels = c("1" = "Class 1 (n=31)", 
                                "2" = "Class 2 (n=20)", 
                                "3" = "Class 3 (n=9)"))
eff_plot

##### tst
gt9x_tst <- gt9x_daily_data %>%
  filter(!is.na(tst),  
         redcap_event_name >= -6 & redcap_event_name <= 125)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(tst ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_tst) 
f_beta2 <- lcmm(tst ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_tst, ng = 2, B = f_beta1, mixture = ~ redcap_event_name)
f_beta3 <- lcmm(tst ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_tst, ng = 3, B = f_beta1, mixture = ~ redcap_event_name)

# Linear 
f_linear1 <- lcmm(tst ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_tst) 
f_linear2 <- lcmm(tst ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_tst, ng = 2, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear3 <- lcmm(tst ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_tst, ng = 3, B = f_linear1, mixture = ~ redcap_event_name)  
f_linear4 <- lcmm(tst ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_tst, ng = 4, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear5 <- lcmm(tst ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_tst, ng = 5, B = f_linear1, mixture = ~ redcap_event_name) 
# f_linear6 <- lcmm(tst ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_tst, ng = 6, B = f_linear1, mixture = ~ redcap_event_name) 
# f_linear7 <- lcmm(tst ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_tst, ng = 7, B = f_linear1, mixture = ~ redcap_event_name) 

# Compare models; AIC/BIC/SABIC (smaller, better fitting), 
# entropy (>0.8, well-separated class), ICL1/2 (lower, better balance between model fit)
summarytable(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5, 
            which=c("loglik", "SABIC", "BIC","entropy"), bty="l",pch=20,col=2)
summary(f_linear3)

# Contingency table of two classifications
xclass(f_linear3, f_linear5)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_linear3)

# PLOT
plot(f_linear3, which = "fit",  var.time= "redcap_event_name")

# Classification and  Export
tst <- f_linear3$pprob
write.csv(tst, file = file.path(outputDir, "/ClassMembership_tst_daily.csv"), row.names = FALSE)


####### tst + LCMM group 
daily_eff_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results/gt9x_sleep_lcmm_daily.csv")
daily_eff_data$class_tst <- as.factor(daily_eff_data$class_tst)

# Average EFF
avg_tst_id <- daily_eff_data %>%
  filter(visit !="")%>%
  group_by(record_id, visit) %>%
  summarise(class = first(class_tst), 
            avg_tst = mean(tst, na.rm=TRUE), 
            sd_tst = sd(tst, na.rm = TRUE), 
            count = sum(!is.na(tst)), 
            se_tst = avg_tst / sqrt(count),  
            ci_lower = avg_tst - (1.96 * se_tst),  
            ci_upper = avg_tst + (1.96 * se_tst))
daily_eff_data$redcap_event_name

avg_tst<- daily_eff_data %>%
  group_by(class_tst, redcap_event_name) %>%
  summarise(n = n_distinct(record_id),
            avg_eff = mean(tst, na.rm=TRUE), 
            sd_eff = sd(tst, na.rm = TRUE), 
            count = sum(!is.na(tst)), 
            se_eff = sd_eff / sqrt(count),  
            ci_lower = avg_eff - (1.96 * se_eff),  
            ci_upper = avg_eff + (1.96 * se_eff),
            .groups = "drop") 
avg_tst
# tst Lint Plot
tst_plot <- ggplot(avg_tst, aes(x = redcap_event_name, y = avg_eff, color = class_tst, group = class_tst)) +  
  geom_line(size = 1) +
  geom_point(size = 2) +
  xlim(0,125) + 
  # geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Total Sleep Time (min)",
       y = "Total Sleep Time (min))") + 
  theme_light() +
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", color = "black", size = 0.2),
        strip.text.x = element_text(size = 15, face = "bold", color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = c('black', 'red', 'green'),
                     labels = c("1" = "Class 1 (n=7)", 
                                "2" = "Class 2 (n=9)", 
                                "3" = "Class 3 (n=44)")) 

tst_plot

##### WASO ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### 
gt9x_waso <- gt9x_daily_data %>%
  filter(!is.na(waso),  
         redcap_event_name >= -6 & redcap_event_name <= 125)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(waso ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_waso) 
f_beta2 <- lcmm(waso ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_waso, ng = 2, B = f_beta1, mixture = ~ redcap_event_name)
f_beta3 <- lcmm(waso ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_waso, ng = 3, B = f_beta1, mixture = ~ redcap_event_name)

# Linear 
f_linear1 <- lcmm(waso ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_waso) 
f_linear2 <- lcmm(waso ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_waso, ng = 2, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear3 <- lcmm(waso ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_waso, ng = 3, B = f_linear1, mixture = ~ redcap_event_name)  
f_linear4 <- lcmm(waso ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_waso, ng = 4, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear5 <- lcmm(waso ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_waso, ng = 5, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear6 <- lcmm(waso ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_waso, ng = 6, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear7 <- lcmm(waso ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_waso, ng = 7, B = f_linear1, mixture = ~ redcap_event_name) 

# Compare models; AIC/BIC/SABIC (smaller, better fitting), 
# entropy (>0.8, well-separated class), ICL1/2 (lower, better balance between model fit)
summarytable(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5,f_linear6, f_linear7,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5, 
            which=c("loglik", "SABIC", "BIC","entropy"), bty="l",pch=20,col=2)
summary(f_linear3)

# Contingency table of two classifications
xclass(f_linear3, f_linear6)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_linear3)

# PLOT
plot(f_linear3, which = "fit",  var.time= "redcap_event_name")

# Classification and  Export
waso <- f_linear3$pprob
write.csv(waso, file = file.path(outputDir, "/ClassMembership_waso_daily.csv"), row.names = FALSE)

# 
outputDirFig <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results/Figures"

####### waso + LCMM group 
daily_eff_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results/gt9x_sleep_lcmm_daily.csv")
daily_eff_data$class_waso <- as.factor(daily_eff_data$class_waso)

# Average EFF
avg_waso_id <- daily_eff_data %>%
  filter(visit !="")%>%
  group_by(record_id, visit) %>%
  summarise(class = first(class_waso), 
            avg_waso = mean(waso, na.rm=TRUE), 
            sd_waso = sd(waso, na.rm = TRUE), 
            count = sum(!is.na(waso)), 
            se_waso = avg_waso / sqrt(count),  
            ci_lower = avg_waso - (1.96 * se_waso),  
            ci_upper = avg_waso + (1.96 * se_waso))

avg_waso<- avg_waso_id %>%
  group_by(class, visit) %>%
  summarise(n = n_distinct(record_id),
            avg_eff = mean(avg_waso, na.rm=TRUE), 
            sd_eff = sd(avg_waso, na.rm = TRUE), 
            count = sum(!is.na(avg_waso)), 
            se_eff = sd_eff / sqrt(count),  
            ci_lower = avg_eff - (1.96 * se_eff),  
            ci_upper = avg_eff + (1.96 * se_eff),
            .groups = "drop") 



# waso Lint Plot
waso_plot <- ggplot(avg_waso, aes(x = visit, y = avg_eff, color = class, group = class)) +  
  geom_line(size = 1) +
  geom_point(size = 2) +
  ylim(0,1500) +   
  scale_x_discrete(limits = c("baseline", "week6", "week12", "week18")) + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Total Sleep Time (min) - Visit Level",
       x = "Visit",
       y = "Total Sleep Time (min))") + 
  theme_light() +
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", color = "black", size = 0.2),
        strip.text.x = element_text(size = 15, face = "bold", color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = c('black', 'red', 'green'),
                     labels = c("1" = "Class 1 (n=7)", 
                                "2" = "Class 2 (n=9)", 
                                "3" = "Class 3 (n=44)")) 

waso_plot

##### DSIS DAILY ########################################################################################################################
avg_dsis$count[110]
dsis_daily <- daily_data %>%
  filter(!is.na(dsis),  
         redcap_event_name >= 0 & redcap_event_name <= 125)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(dsis ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = dsis_daily) 
f_beta2 <- lcmm(dsis ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = dsis_daily, ng = 2, B = f_beta1, mixture = ~ redcap_event_name)
f_beta3 <- lcmm(dsis ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = dsis_daily, ng = 3, B = f_beta1, mixture = ~ redcap_event_name)

# Linear 
f_linear1 <- lcmm(dsis ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = dsis_daily) 
f_linear2 <- lcmm(dsis ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = dsis_daily, ng = 2, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear3 <- lcmm(dsis ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = dsis_daily, ng = 3, B = f_linear1, mixture = ~ redcap_event_name)  
f_linear4 <- lcmm(dsis ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = dsis_daily, ng = 4, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear5 <- lcmm(dsis ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = dsis_daily, ng = 5, B = f_linear1, mixture = ~ redcap_event_name) 
# f_linear6 <- lcmm(dsis ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = dsis_daily, ng = 6, B = f_linear1, mixture = ~ redcap_event_name) 
# f_linear7 <- lcmm(dsis ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = dsis_daily, ng = 7, B = f_linear1, mixture = ~ redcap_event_name) 

# Compare models; AIC/BIC/SABIC (smaller, better fitting), 
# entropy (>0.8, well-separated class), ICL1/2 (lower, better balance between model fit)
summarytable(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5, 
            which=c("loglik", "SABIC", "BIC","entropy"), bty="l",pch=20,col=2)
summary(f_linear3)

# Contingency table of two classifications
xclass(f_linear4, f_linear5)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_linear3)

# PLOT
plot(f_linear5, which = "fit", marg=FALSE, var.time= "redcap_event_name", bty="n")


# Classification and  Export
dsis_prob <- f_linear3$pprob
write.csv(dsis_prob, file = file.path(outputDir, "/ClassMembership_dsis_daily.csv"), row.names = FALSE)

 
###### DSIS + GROUP
daily_eff_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results/gt9x_sleep_lcmm_daily.csv")
daily_eff_data$class <- as.factor(daily_eff_data$class)

# Average DSIS
avg_eff_id <- daily_eff_data %>%
  filter(visit !="")%>%
  group_by(record_id, visit) %>%
  summarise(class = first(class), 
            avg_dsis = mean(dsis, na.rm=TRUE), 
            sd_dsis = sd(dsis, na.rm = TRUE), 
            count = sum(!is.na(dsis)), 
            se_dsis = avg_dsis / sqrt(count),  
            ci_lower = avg_dsis - (1.96 * se_dsis),  
            ci_upper = avg_dsis + (1.96 * se_dsis))

avg_eff <- avg_eff_id %>%
  group_by(class, visit) %>%
  summarise(n = n_distinct(record_id),
    avg_eff = mean(avg_dsis, na.rm=TRUE), 
            sd_eff = sd(avg_dsis, na.rm = TRUE), 
            count = sum(!is.na(avg_dsis)), 
            se_eff = sd_eff / sqrt(count),  
            ci_lower = avg_eff - (1.96 * se_eff),  
            ci_upper = avg_eff + (1.96 * se_eff),
            .groups = "drop") 



# Plot
eff_plot <- ggplot(avg_eff, aes(x = visit, y = avg_eff, color = class, group = class)) +  
  geom_line(size = 1) +
  geom_point(size = 2) +
  ylim(50, 100) +   
  scale_x_discrete(limits = c("baseline", "week6", "week12", "week18")) + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "Sleep dsis - Visit Level",
       x = "Visit",
       y = "Sleep dsis (%)") + 
  theme_light() +
  theme(axis.ticks.x = element_blank()) + 
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),
        title = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.background = element_rect(fill = "grey80", color = "black", size = 0.2),
        strip.text.x = element_text(size = 15, face = "bold", color = "black"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) + 
  scale_color_manual(values = c('black', 'red', 'green'),
                     labels = c("1" = "Class 1 (n=31)", 
                                "2" = "Class 2 (n=20)", 
                                "3" = "Class 3 (n=9)")) 

eff_plot

