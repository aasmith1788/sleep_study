library(dplyr)
library(ggplot2)
library(ggthemes)

# Import the data
outputDirFig <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results/Figures"
##  IMPORT DATA 
# GT9X
gt9x_daily_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/gt9x_sleep_daily.csv")
gt9x_weekly_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/gt9x_sleep_weekly.csv")

# DSIS
dsis_daily_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/dsis_daily.csv")
dsis_daily_data$dsis <- as.numeric(dsis_daily_data$dsis)

dsis_weekly_data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/dsis_weekly.csv")
dsis_weekly_data$dsis <- as.numeric(dsis_weekly_data$dsis)

# FILTER DATAEST
# CALCULATE MAX DAY OF WEEK12
max_days <- gt9x_daily_data %>%
  filter(visit == "week12") %>%
  group_by(record_id) %>%
  summarise(max_day_from_first_ptvisit = max(day_from_first_ptvisit)) %>%
  ungroup()

# GT9X
filtered_gt9x_data <- gt9x_daily_data %>%
  inner_join(max_days, by = "record_id") %>%  # Join to get max day for each record_id
  filter((day_from_first_ptvisit >= -2) &
           day_from_first_ptvisit <= max_day_from_first_ptvisit) %>%
  arrange(record_id, day_from_first_ptvisit)  
write.csv(filtered_gt9x_data,"filtered_gt9x_data.csv", row.names = FALSE)

filtered_gt9x_data <- gt9x_daily_data %>%
  inner_join(max_days, by = "record_id") %>%  # Join to get max day for each record_id
  filter((day_from_first_ptvisit >= -2) &
           day_from_first_ptvisit <= 86) %>%
  arrange(record_id, day_from_first_ptvisit)  


# FILTER DATAEST
filtered_dsis_data <- dsis_daily_data %>%
  inner_join(max_days, by = "record_id") %>%  # Join to get max day for each record_id
  filter( day_from_first_ptvisit >= 0 &  day_from_first_ptvisit <= max_day_from_first_ptvisit) %>%
  arrange(record_id, day_from_first_ptvisit) 

######### PLOT #########
avg_gt9x_daily <- filtered_gt9x_data  %>%
  group_by(day_from_first_ptvisit) %>%
  summarise(count = sum(!is.na(Efficiency)),
            avg_Efficiency = mean(Efficiency, na.rm=TRUE), 
            sd_Efficiency = sd(Efficiency, na.rm = TRUE), 
            se_Efficiency = sd_Efficiency / sqrt(count),  
            ci_lower_Efficiency  = avg_Efficiency - (1.96 * se_Efficiency),  
            ci_upper_Efficiency  = avg_Efficiency + (1.96 * se_Efficiency), 
            
            avg_tib = mean(tib, na.rm=TRUE), 
            sd_tib = sd(tib, na.rm = TRUE), 
            se_tib = sd_tib / sqrt(count),  
            ci_lower_tib = avg_tib - (1.96 * se_tib),  
            ci_upper_tib = avg_tib + (1.96 * se_tib),     
            
            avg_tst = mean(tst, na.rm=TRUE), 
            sd_tst = sd(tst, na.rm = TRUE), 
            se_tst = sd_tst / sqrt(count),  
            ci_lower_tst = avg_tst - (1.96 * se_tst),  
            ci_upper_tst = avg_tst + (1.96 * se_tst), 
            
            avg_waso = mean(waso, na.rm=TRUE), 
            sd_waso = sd(waso, na.rm = TRUE), 
            se_waso = sd_waso / sqrt(count),  
            ci_lower_waso = avg_waso - (1.96 * se_waso),  
            ci_upper_waso = avg_waso + (1.96 * se_waso), 
            
            avg_mi = mean(MovementIndex, na.rm=TRUE), 
            sd_mi = sd(MovementIndex, na.rm = TRUE), 
            se_mi = sd_mi / sqrt(count),  
            ci_lower_mi = avg_mi - (1.96 * se_mi),  
            ci_upper_mi = avg_mi + (1.96 * se_mi),
            
            avg_fi = mean(FragmentationIndex, na.rm=TRUE), 
            sd_fi = sd(FragmentationIndex, na.rm = TRUE), 
            se_fi = sd_fi / sqrt(count),  
            ci_lower_fi = avg_fi - (1.96 * se_fi),  
            ci_upper_fi = avg_fi + (1.96 * se_fi),
            
            avg_sfi = mean(Sleep_Fragmentation_Index, na.rm=TRUE), 
            sd_sfi = sd(Sleep_Fragmentation_Index, na.rm = TRUE), 
            se_sfi = sd_sfi / sqrt(count),  
            ci_lower_sfi = avg_sfi - (1.96 * se_sfi),  
            ci_upper_sfi = avg_sfi + (1.96 * se_sfi))
setwd("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory")
write.csv(avg_gt9x_daily,"avg_gt9x_daily.csv", row.names = FALSE)


# Plot
setwd("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results")
daily_eff_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_Efficiency)) +
  geom_line() +  
  geom_point() +
  scale_y_continuous(limits = c(70,100), 
                     breaks=seq(70, 100, by = 10)) + 
  scale_x_continuous(limits = c(-2, 86), breaks = seq(-2, 86, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_Efficiency, ymax = ci_upper_Efficiency), width = 0.2) +
  labs(title = "Sleep Efficiency Daily",
       x = "Days",
       y = "Sleep Efficiency (%)")+ theme_light()
daily_eff_plot
ggsave(file.path(outputDirFig,"avg_Efficiency_daily.png"), plot = daily_eff_plot, width = 16, height = 3.5, dpi = 300)

#
daily_tib_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_tib)) +
  geom_line() +  
  geom_point() + 
  ylim (200,850)+
  scale_x_continuous(limits = c(-2, 86), breaks = seq(-2, 86, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_tib, ymax = ci_upper_tib), width = 0.2) +
  labs(title = "Total Sleep Time Daily",
       x = "Days",
       y = "Total Time in Bed (min)")+ theme_light()
daily_tib_plot
ggsave(file.path(outputDirFig,"avg_tib_daily.png"), plot = daily_tib_plot, width = 16, height = 3.5, dpi = 300)

#
daily_tst_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_tst)) +
  geom_line() +  
  geom_point() + 
  ylim (200,850)+
  scale_x_continuous(limits = c(-2, 86), breaks = seq(-2, 86, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_tst, ymax = ci_upper_tst), width = 0.2) +
  labs(title = "Total Sleep Time Daily",
       x = "Days",
       y = "Total Sleep Time (min)")+ theme_light()
daily_tst_plot
ggsave(file.path(outputDirFig,"avg_TST_daily.png"), plot = daily_tst_plot, width = 16, height = 3.5, dpi = 300)

#
daily_waso_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_waso)) +
  geom_line() +  
  geom_point() +
  ylim (50,200)+
  scale_x_continuous(limits = c(-2, 86), breaks = seq(-2, 86, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_waso, ymax = ci_upper_waso), width = 0.2) +
  labs(title = "Wakefulness Aftesr Sleep Onset Daily",
       x = "Days",
       y = "Wakefulness After Sleep Onset (min)") + theme_light()
daily_waso_plot
ggsave(file.path(outputDirFig,"avg_WASO_daily.png"), plot = daily_waso_plot, width = 16, height = 3.5, dpi = 300)

#
daily_mi_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_mi)) +
  geom_line() +  
  geom_point() +
 ylim (50,80)+
  scale_x_continuous(limits = c(-2, 86), breaks = seq(-2, 86, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_mi, ymax = ci_upper_mi), width = 0.2) +
  labs(title = "Wakefulness Aftesr Sleep Onset Daily",
       x = "Days",
       y = "Movement Index (%)") + theme_light()
daily_mi_plot
ggsave(file.path(outputDirFig,"avg_MI_daily.png"), plot = daily_waso_plot, width = 16, height = 3.5, dpi = 300)

#
daily_fi_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_fi)) +
  geom_line() +  
  geom_point() +
  ylim (20,70)+
  scale_x_continuous(limits = c(-2, 86), breaks = seq(-2, 86, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_fi, ymax = ci_upper_fi), width = 0.2) +
  labs(title = "Wakefulness Aftesr Sleep Onset Daily",
       x = "Days",
       y = "Fragmentation Index (%)") + theme_light()
daily_fi_plot
ggsave(file.path(outputDirFig,"avg_FI_daily.png"), plot = daily_waso_plot, width = 16, height = 3.5, dpi = 300)

#
daily_sfi_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_sfi)) +
  geom_line() +  
  geom_point() +
  ylim (70,120)+
  scale_x_continuous(limits = c(-2, 86), breaks = seq(-2, 86, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_sfi, ymax = ci_upper_sfi), width = 0.2) +
  labs(title = "Wakefulness Aftesr Sleep Onset Daily",
       x = "Days",
       y = "Sleep Fragmentation Index") + theme_light()
daily_sfi_plot
ggsave(file.path(outputDirFig,"avg_SFI_daily.png"), plot = daily_waso_plot, width = 16, height = 3.5, dpi = 300)


#####################################
library(dplyr)
library(ggplot2)
library(lcmm)
library(gbmt) # Group-Based Multivariate Trajectory Modeling
# library(mlogit) # multinomial logistic regression
outputDir <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results"

## Efficiency
gt9x_Efficiency <- filtered_gt9x_data %>%
  filter(!is.na(Efficiency))

mlin <- lcmm(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency)
mlin2 <- hlme(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency) 
mbeta <- lcmm(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'beta', data = gt9x_Efficiency) 
mspl <- lcmm(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'splines', data = gt9x_Efficiency) 
mspl5 <- lcmm(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = '5-quant-splines', data = gt9x_Efficiency) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "AIC", "BIC",  "SABIC","ICL1", "ICL2")) 

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_Efficiency) 
f_beta2 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 2, B = f_beta1, mixture = ~ day_from_first_ptvisit)
f_beta3 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 3, B = f_beta1, mixture = ~ day_from_first_ptvisit)
f_beta4 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 4, B = f_beta1, mixture = ~ day_from_first_ptvisit)
f_beta5 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 5, B = f_beta1, mixture = ~ day_from_first_ptvisit)
f_beta6 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 6, B = f_beta1, mixture = ~ day_from_first_ptvisit)

dev.off()

# Compare models; AIC/BIC/SABIC (smaller, better fitting), 
# entropy (>0.8, well-separated class), ICL1/2 (lower, better balance between model fit)
summarytable(f_beta1, f_beta2, f_beta3, f_beta4, f_beta5, f_beta6,
             which = c("G", "loglik", "AIC", "BIC", "SABIC", "entropy", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_beta1, f_beta2, f_beta3, f_beta4, f_beta5, f_beta6,
            which=c("loglik", "AIC", "SABIC","BIC", "entropy","ICL"), bty="l",pch=20,col=2)

# Contingency table of two classifications
xclass(f_beta3, f_beta4)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_beta4)

# PLOT
png(paste0(outputDirFig, "/ClassMembership_Eff_beta4_daily.png"), width = 2000, height = 2000, res = 300)
par(cex = 0.7)
plot(f_beta4, which = "fit",  var.time= "day_from_first_ptvisit")
par(cex=1)
dev.off()

# Classification and  Export
eff_mean <- f_beta4$pprob
write.csv(eff_mean, file = file.path(outputDir, "/ClassMembership_Eff_beta4_daily.csv"), row.names = FALSE)


##### TST 
gt9x_tst <- filtered_gt9x_data %>%  filter(!is.na(tst))

mlin <- lcmm(tst ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst)
mlin2 <- hlme(tst ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst) 
mbeta <- lcmm(tst ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'beta', data = gt9x_tst) 
mspl <- lcmm(tst ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'splines', data = gt9x_tst) 
mspl5 <- lcmm(tst ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = '5-quant-splines', data = gt9x_tst) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "AIC", "BIC",  "SABIC","ICL1", "ICL2")) 

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# QUADRATIC
f_mspl1 <- lcmm(tst ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_tst) 
f_mspl2 <- lcmm(tst ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 2, B = f_mspl1, mixture = ~ day_from_first_ptvisit)
f_mspl3 <- lcmm(tst ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 3, B = f_mspl1, mixture = ~ day_from_first_ptvisit)
f_mspl4 <- lcmm(tst ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 4, B = f_mspl1, mixture = ~ day_from_first_ptvisit)
f_mspl5 <- lcmm(tst ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 5, B = f_mspl1, mixture = ~ day_from_first_ptvisit)
f_mspl6 <- lcmm(tst ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 6, B = f_mspl1, mixture = ~ day_from_first_ptvisit)

# Compare models
summarytable(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5, 
            which=c("loglik", "AIC", "SABIC","BIC", "entropy","ICL"), bty="l",pch=20,col=2)

summary(f_mspl3)

# Contingency table of two classifications
xclass(f_mspl5, f_mspl3)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_mspl5)

# PLOT
png(paste0(outputDirFig, "/ClassMembership_TST_mspl5_daily.png"), width = 2000, height = 2000, res = 300)
par(cex = 0.7)
plot(f_mspl5, which = "fit",  var.time= "day_from_first_ptvisit")
par(cex=1)
dev.off()

# Classification and  Export
tst <- f_mspl5$pprob
write.csv(tst, file = file.path(outputDir, "/ClassMembership_tst_mspl5_daily.csv"), row.names = FALSE)

##### WASO 
gt9x_waso <- filtered_gt9x_data %>%  filter(!is.na(waso))

mlin <- lcmm(waso ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso)
mlin2 <- hlme(waso ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso) 
mbeta <- lcmm(waso ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'beta', data = gt9x_waso) 
mspl <- lcmm(waso ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'splines', data = gt9x_waso) 
mspl5 <- lcmm(waso ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = '5-quant-splines', data = gt9x_waso) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "AIC", "BIC",  "SABIC","ICL1", "ICL2")) 

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# BETA
waso_beta1 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_waso) 
waso_beta2 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 2, B = waso_beta1, mixture = ~ day_from_first_ptvisit)
waso_beta3 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 3, B = waso_beta1, mixture = ~ day_from_first_ptvisit)
waso_beta4 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 4, B = waso_beta1, mixture = ~ day_from_first_ptvisit)
waso_beta5 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 5, B = waso_beta1, mixture = ~ day_from_first_ptvisit)
waso_beta6 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 6, B = waso_beta1, mixture = ~ day_from_first_ptvisit)

# Compare models
summarytable(waso_beta1, waso_beta2, waso_beta3,  waso_beta4,  waso_beta5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(waso_beta1, waso_beta2, waso_beta3,  waso_beta4,  waso_beta5,
            which=c("loglik", "AIC", "SABIC","BIC", "entropy","ICL"), bty="l",pch=20,col=2)

summary(waso_beta4)

# Contingency table of two classifications
xclass(waso_beta3, waso_beta4)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(waso_beta4)

# PLOT
png(paste0(outputDirFig, "/ClassMembership_waso_beta3_daily.png"), width = 2000, height = 2000, res = 300)
par(cex = 0.7)
plot(waso_beta3, which = "fit",  var.time= "day_from_first_ptvisit")
par(cex=1)
dev.off()

# Classification and  Export
waso <- waso_beta3$pprob
write.csv(waso, file = file.path(outputDir, "/ClassMembership_waso_beta3_daily.csv"), row.names = FALSE)



##### SFI 
gt9x_sfi <- filtered_gt9x_data %>%  filter(!is.na(Sleep_Fragmentation_Index))

mlin <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_sfi)
mlin2 <- hlme(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_sfi) 
mbeta <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'beta', data = gt9x_sfi) 
mspl <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'splines', data = gt9x_sfi) 
mspl5 <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = '5-quant-splines', data = gt9x_sfi) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "AIC", "BIC",  "SABIC","ICL1", "ICL2")) 

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# QUADRATIC
sfi_mspl1 <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_sfi) 
sfi_mspl2 <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_sfi, ng = 2, B = sfi_mspl1, mixture = ~ day_from_first_ptvisit)
sfi_mspl3 <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_sfi, ng = 3, B = sfi_mspl1, mixture = ~ day_from_first_ptvisit)
sfi_mspl4 <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_sfi, ng = 4, B = sfi_mspl1, mixture = ~ day_from_first_ptvisit)
# sfi_mspl5 <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_sfi, ng = 5, B = sfi_mspl1, mixture = ~ day_from_first_ptvisit)
# sfi_mspl6 <- lcmm(Sleep_Fragmentation_Index ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_sfi, ng = 6, B = sfi_mspl1, mixture = ~ day_from_first_ptvisit)

# Compare models
summarytable(sfi_mspl1, sfi_mspl2, sfi_mspl3,  sfi_mspl4,  sfi_mspl5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(sfi_mspl1, sfi_mspl2, sfi_mspl3,  sfi_mspl4,
            which=c("loglik", "AIC", "SABIC","BIC", "entropy","ICL"), bty="l",pch=20,col=2)

summary(sfi_mspl4)

# Contingency table of two classifications
xclass(f_mspl5, f_mspl3)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(sfi_mspl4)

# PLOT
png(paste0(outputDirFig, "/ClassMembership_sfi_mspl4_daily.png"), width = 2000, height = 2000, res = 300)
par(cex = 0.7)
plot(sfi_mspl4, which = "fit",  var.time= "day_from_first_ptvisit")
par(cex=1)
dev.off()

# Classification and  Export
sfi <- sfi_mspl4$pprob
write.csv(sfi, file = file.path(outputDir, "/ClassMembership_sfi_mspl4_daily.csv"), row.names = FALSE)


##### mi 
gt9x_mi <- filtered_gt9x_data %>%  filter(!is.na(MovementIndex))

mlin <- lcmm(MovementIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_mi)
mlin2 <- hlme(MovementIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_mi) 
mbeta <- lcmm(MovementIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'beta', data = gt9x_mi) 
mspl <- lcmm(MovementIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'splines', data = gt9x_mi) 
mspl5 <- lcmm(MovementIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = '5-quant-splines', data = gt9x_mi) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "AIC", "BIC",  "SABIC","ICL1", "ICL2")) 

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# QUADRATIC
mi_mspl1 <- lcmm(MovementIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_mi) 
mi_mspl2 <- lcmm(MovementIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_mi, ng = 2, B = mi_mspl1, mixture = ~ day_from_first_ptvisit)
mi_mspl3 <- lcmm(MovementIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_mi, ng = 3, B = mi_mspl1, mixture = ~ day_from_first_ptvisit)
mi_mspl4 <- lcmm(MovementIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_mi, ng = 4, B = mi_mspl1, mixture = ~ day_from_first_ptvisit)
mi_mspl5 <- lcmm(MovementIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_mi, ng = 5, B = mi_mspl1, mixture = ~ day_from_first_ptvisit)
mi_mspl6 <- lcmm(MovementIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_mi, ng = 6, B = mi_mspl1, mixture = ~ day_from_first_ptvisit)

# Compare models
summarytable(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5, 
            which=c("loglik", "AIC", "SABIC","BIC", "entropy","ICL"), bty="l",pch=20,col=2)

summary(f_mspl3)

# Contingency table of two classifications
xclass(mi_mspl5, mi_mspl2)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(mi_mspl5)

# PLOT
png(paste0(outputDirFig, "/ClassMembership_MI_mspl5_daily.png"), width = 2000, height = 2000, res = 300)
par(cex = 0.7)
plot(mi_mspl5, which = "fit",  var.time= "day_from_first_ptvisit")
par(cex=1)
dev.off()

# Classification and  Export
mi <- f_mspl5$pprob
write.csv(mi, file = file.path(outputDir, "/ClassMembership_mi_mspl5_daily.csv"), row.names = FALSE)


##### fi 
gt9x_fi <- filtered_gt9x_data %>%  filter(!is.na(FragmentationIndex))

mlin <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_fi)
mlin2 <- hlme(FragmentationIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_fi) 
mbeta <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'beta', data = gt9x_fi) 
mspl <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'splines', data = gt9x_fi) 
mspl5 <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = '5-quant-splines', data = gt9x_fi) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "AIC", "BIC",  "SABIC","ICL1", "ICL2")) 

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# QUADRATIC
f_mspl1 <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_fi) 
f_mspl2 <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_fi, ng = 2, B = f_mspl1, fixture = ~ day_from_first_ptvisit)
f_mspl3 <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_fi, ng = 3, B = f_mspl1, fixture = ~ day_from_first_ptvisit)
f_mspl4 <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_fi, ng = 4, B = f_mspl1, fixture = ~ day_from_first_ptvisit)
f_mspl5 <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_fi, ng = 5, B = f_mspl1, fixture = ~ day_from_first_ptvisit)
# f_mspl6 <- lcmm(FragmentationIndex ~ day_from_first_ptvisit , link = "splines", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_fi, ng = 6, B = f_mspl1, fixture = ~ day_from_first_ptvisit)

# Compare models
summarytable(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_mspl1, f_mspl2, f_mspl3,  f_mspl4,  f_mspl5, 
            which=c("loglik", "AIC", "SABIC","BIC", "entropy","ICL"), bty="l",pch=20,col=2)

summary(f_mspl3)

# Contingency table of two classifications
xclass(f_mspl5, f_mspl3)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_mspl5)

# PLOT
png(paste0(outputDirFig, "/ClassMembership_fi_mspl5_daily.png"), width = 2000, height = 2000, res = 300)
par(cex = 0.7)
plot(f_mspl5, which = "fit",  var.time= "day_from_first_ptvisit")
par(cex=1)
dev.off()

# Classification and  Export
fi <- f_mspl5$pprob
write.csv(fi, file = file.path(outputDir, "/ClassMembership_fi_mspl5_daily.csv"), row.names = FALSE)








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
daily_eff_data$day_from_first_ptvisit

avg_tst<- daily_eff_data %>%
  group_by(class_tst, day_from_first_ptvisit) %>%
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
tst_plot <- ggplot(avg_tst, aes(x = day_from_first_ptvisit, y = avg_eff, color = class_tst, group = class_tst)) +  
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
         day_from_first_ptvisit >= -6 & day_from_first_ptvisit <= 125)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_waso) 
f_beta2 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 2, B = f_beta1, mixture = ~ day_from_first_ptvisit)
f_beta3 <- lcmm(waso ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 3, B = f_beta1, mixture = ~ day_from_first_ptvisit)

# Linear 
f_linear1 <- lcmm(waso ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_waso) 
f_linear2 <- lcmm(waso ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 2, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear3 <- lcmm(waso ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 3, B = f_linear1, mixture = ~ day_from_first_ptvisit)  
f_linear4 <- lcmm(waso ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 4, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear5 <- lcmm(waso ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 5, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear6 <- lcmm(waso ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 6, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear7 <- lcmm(waso ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_waso, ng = 7, B = f_linear1, mixture = ~ day_from_first_ptvisit) 

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
plot(f_linear3, which = "fit",  var.time= "day_from_first_ptvisit")

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
         day_from_first_ptvisit >= 0 & day_from_first_ptvisit <= 125)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(dsis ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = dsis_daily) 
f_beta2 <- lcmm(dsis ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = dsis_daily, ng = 2, B = f_beta1, mixture = ~ day_from_first_ptvisit)
f_beta3 <- lcmm(dsis ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = dsis_daily, ng = 3, B = f_beta1, mixture = ~ day_from_first_ptvisit)

# Linear 
f_linear1 <- lcmm(dsis ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = dsis_daily) 
f_linear2 <- lcmm(dsis ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = dsis_daily, ng = 2, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear3 <- lcmm(dsis ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = dsis_daily, ng = 3, B = f_linear1, mixture = ~ day_from_first_ptvisit)  
f_linear4 <- lcmm(dsis ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = dsis_daily, ng = 4, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear5 <- lcmm(dsis ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = dsis_daily, ng = 5, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
# f_linear6 <- lcmm(dsis ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = dsis_daily, ng = 6, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
# f_linear7 <- lcmm(dsis ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = dsis_daily, ng = 7, B = f_linear1, mixture = ~ day_from_first_ptvisit) 

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
plot(f_linear5, which = "fit", marg=FALSE, var.time= "day_from_first_ptvisit", bty="n")


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

