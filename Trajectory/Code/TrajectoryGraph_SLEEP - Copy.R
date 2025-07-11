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
  filter((visit == "baseline" | day_from_first_ptvisit >= 0) &
           day_from_first_ptvisit <= max_day_from_first_ptvisit) %>%
  arrange(record_id, day_from_first_ptvisit)  

# FILTER DATAEST
filtered_dsis_data <- dsis_daily_data %>%
  inner_join(max_days, by = "record_id") %>%  # Join to get max day for each record_id
  filter( redcap_event_name >= 0 &  redcap_event_name <= max_day_from_first_ptvisit) %>%
  arrange(record_id, redcap_event_name) 





# Average DSIS
avg_dsis <- daily_data %>%
  group_by(redcap_event_name) %>%
  summarise(avg_dsis = mean(dsis, na.rm=TRUE), 
            sd_dsis = sd(dsis, na.rm = TRUE), 
            count = sum(!is.na(dsis)), 
            se_dsis = sd_dsis / sqrt(count),  
            ci_lower = avg_dsis - (1.96 * se_dsis),  
            ci_upper = avg_dsis + (1.96 * se_dsis), 
            n= n()) 

ggplot(avg_dsis, aes(x = as.factor(redcap_event_name), y = avg_dsis)) +
  geom_boxplot() +
  labs(x = "Time (redcap_event_name)", y = "DSIS") +
  theme_minimal()


# Plot
dsis_plot <- ggplot(avg_dsis, aes(x = redcap_event_name, y = avg_dsis)) +
  geom_line() +
  ylim(0, 5) + scale_x_continuous(limits = c(0, 136), breaks = seq(0, 136, by = 15)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "DSIS Daily",
       x = "Days",
       y = "Averaged DSIS (0-10)")+ theme_light()
dsis_plot

# Export
ggsave(file.path(outputDirFig,"avg_dsis_daily.png"), plot = dsis_plot, width = 16, height = 3.5, dpi = 300)

## WEEKLY _ VIST???
# Average DSIS
avg_dsis_weekly <- weekly_data %>%
  group_by(redcap_event_name) %>%
  summarise(avg_dsis = mean(dsis, na.rm=TRUE), 
            sd_dsis = sd(dsis, na.rm = TRUE), 
            count = sum(!is.na(dsis)), 
            se_dsis = sd_dsis / sqrt(count),  
            ci_lower = avg_dsis - (1.96 * se_dsis),  
            ci_upper = avg_dsis + (1.96 * se_dsis))
avg_dsis_weekly_filtered <- avg_dsis_weekly %>%
  filter(redcap_event_name >= 0 & redcap_event_name <= 22)

# Plot
dsis_plot <- ggplot(avg_dsis_weekly_filtered, aes(x = redcap_event_name, y = avg_dsis)) +
  geom_line(group = 1) +  
  ylim(0, 5) + 
  scale_x_continuous(breaks = seq(0, 22, by = 1), 
                     labels = paste(avg_dsis_weekly_filtered$redcap_event_name, "\n(n=", avg_dsis_weekly_filtered$count, ")", sep = "")) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  labs(title = "DSIS WEEKLY",
       x = "Weeks",
       y = "Averaged DSIS (0-10)")+ 
  theme_hc(base_size = 14)

ggsave(file.path(outputDirFig,"avg_dsis_weekly.png"), plot = dsis_plot, width = 16, height = 3.5, dpi = 300)



# Average DSIS
avg_gt9x_daily <- gt9x_daily_data %>%
  group_by(day_from_first_ptvisit) %>%
  summarise(count = sum(!is.na(Efficiency)),
            avg_Efficiency = mean(Efficiency, na.rm=TRUE), 
            sd_Efficiency = sd(Efficiency, na.rm = TRUE), 
            se_Efficiency = sd_Efficiency / sqrt(count),  
            ci_lower_Efficiency  = avg_Efficiency - (1.96 * se_Efficiency),  
            ci_upper_Efficiency  = avg_Efficiency + (1.96 * se_Efficiency), 
            
            avg_tst = mean(tst, na.rm=TRUE), 
            sd_tst = sd(tst, na.rm = TRUE), 
            se_tst = sd_tst / sqrt(count),  
            ci_lower_tst = avg_tst - (1.96 * se_tst),  
            ci_upper_tst = avg_tst + (1.96 * se_tst), 
            
            avg_waso = mean(waso, na.rm=TRUE), 
            sd_waso = sd(waso, na.rm = TRUE), 
            se_waso = sd_waso / sqrt(count),  
            ci_lower_waso = avg_waso - (1.96 * se_waso),  
            ci_upper_waso = avg_waso + (1.96 * se_waso))


# Plot
daily_eff_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_Efficiency)) +
  geom_line() +  
  geom_point() +
  scale_y_continuous(limits = c(60,100), 
                     breaks=seq(60, 100, by = 10)) + 
  scale_x_continuous(limits = c(-15, 135), breaks = seq(-15, 135, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_Efficiency, ymax = ci_upper_Efficiency), width = 0.2) +
  labs(title = "Sleep Efficiency Daily",
       x = "Days",
       y = "Sleep Efficiency (%)")+ theme_light()
daily_eff_plot
ggsave(file.path(outputDirFig,"avg_Efficiency_daily_part.png"), plot = daily_eff_plot, width = 16, height = 3.5, dpi = 300)

#
daily_tst_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_tst)) +
  geom_line() +  
  geom_point() + 
  ylim (0,850)+
  scale_x_continuous(limits = c(-15, 135), breaks = seq(-15, 135, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_tst, ymax = ci_upper_tst), width = 0.2) +
  labs(title = "Total Sleep Time Daily",
       x = "Days",
       y = "Total Sleep Time (min)")+ theme_light()
daily_tst_plot
ggsave(file.path(outputDirFig,"avg_tst_daily_part.png"), plot = daily_tst_plot, width = 16, height = 3.5, dpi = 300)

#
daily_waso_plot <- ggplot(avg_gt9x_daily, aes(x = day_from_first_ptvisit, y = avg_waso)) +
  geom_line() +  
  geom_point() +
  ylim (0,200)+
  scale_x_continuous(limits = c(-15, 135), breaks = seq(-15, 135, by = 5)) +
  geom_errorbar(aes(ymin = ci_lower_waso, ymax = ci_upper_waso), width = 0.2) +
  labs(title = "Wakefulness After Sleep Onset Daily",
       x = "Days",
       y = "Wakefulness After Sleep Onset (min)") + theme_light()
daily_waso_plot
ggsave(file.path(outputDirFig,"avg_waso_daily_part.png"), plot = daily_waso_plot, width = 16, height = 3.5, dpi = 300)

# WEEKLY
colnames(gt9x_weekly_data)

avg_gt9x_weekly_by_record <- gt9x_weekly_data %>%
  filter(redcap_event_name != "") %>%
  group_by(record_id, redcap_event_name) %>%
  summarise(ave_Efficiency_Mean = mean(Efficiency_Mean, na.rm=TRUE, .groups = 'drop'), 
            ave_Efficiency_Median = mean(Efficiency_Median, na.rm=TRUE), 

            ave_tst_Mean = mean(tst_Mean, na.rm=TRUE), 
            ave_tst_Median = mean(tst_Median, na.rm=TRUE), 
            
            ave_waso_Mean = mean(waso_Mean, na.rm=TRUE), 
            ave_waso_Median = mean(waso_Median, na.rm=TRUE))

avg_gt9x_weekly <- avg_gt9x_weekly_by_record %>%
  filter(redcap_event_name != "") %>%
  group_by(redcap_event_name) %>%
  summarise(count = sum(!is.na(ave_Efficiency_Mean)),
            avg_Efficiency_Mean = mean(ave_Efficiency_Mean, na.rm=TRUE), 
            sd_Efficiency_Mean = sd(ave_Efficiency_Mean, na.rm = TRUE), 
            se_Efficiency_Mean = sd_Efficiency_Mean / sqrt(count),  
            ci_lower_Efficiency_Mean  = avg_Efficiency_Mean - (1.96 * se_Efficiency_Mean),  
            ci_upper_Efficiency_Mean  = avg_Efficiency_Mean + (1.96 * se_Efficiency_Mean),
            
            avg_Efficiency_Median = mean(ave_Efficiency_Median, na.rm=TRUE), 
            sd_Efficiency_Median = sd(ave_Efficiency_Median, na.rm = TRUE), 
            se_Efficiency_Median = sd_Efficiency_Median / sqrt(count),  
            ci_lower_Efficiency_Median  = avg_Efficiency_Median - (1.96 * se_Efficiency_Median),  
            ci_upper_Efficiency_Median  = avg_Efficiency_Median + (1.96 * se_Efficiency_Median), 
            
            avg_tst_Mean = mean(ave_tst_Mean, na.rm=TRUE), 
            sd_tst_Mean = sd(ave_tst_Mean, na.rm = TRUE), 
            se_tst_Mean = sd_tst_Mean / sqrt(count),  
            ci_lower_tst_Mean = avg_tst_Mean - (1.96 * se_tst_Mean),  
            ci_upper_tst_Mean = avg_tst_Mean + (1.96 * se_tst_Mean),             
            
            avg_tst_Median = mean(ave_tst_Median, na.rm=TRUE), 
            sd_tst_Median = sd(ave_tst_Median, na.rm = TRUE), 
            se_tst_Median = sd_tst_Median / sqrt(count),  
            ci_lower_tst_Median = avg_tst_Median - (1.96 * se_tst_Median),  
            ci_upper_tst_Median = avg_tst_Median + (1.96 * se_tst_Median), 
            
            avg_waso_Mean = mean(ave_waso_Mean, na.rm=TRUE), 
            sd_waso_Mean = sd(ave_waso_Mean, na.rm = TRUE), 
            se_waso_Mean = sd_waso_Mean / sqrt(count),  
            ci_lower_waso_Mean = avg_waso_Mean - (1.96 * se_waso_Mean),  
            ci_upper_waso_Mean = avg_waso_Mean + (1.96 * se_waso_Mean), 
            
            avg_waso_Median = mean(ave_waso_Median, na.rm=TRUE), 
            sd_waso_Median = sd(ave_waso_Median, na.rm = TRUE), 
            se_waso_Median = sd_waso_Median / sqrt(count),  
            ci_lower_waso_Median = avg_waso_Median - (1.96 * se_waso_Median),  
            ci_upper_waso_Median = avg_waso_Median + (1.96 * se_waso_Median))

avg_gt9x_weekly_filtered <- avg_gt9x_weekly %>%
  filter(redcap_event_name >= 0 & redcap_event_name <= 22)

# Plot
weekly_eff_mean_plot <- ggplot(avg_gt9x_weekly_filtered, aes(x = redcap_event_name , y = avg_Efficiency_Mean)) +
  geom_line() +  
  geom_point() +
  scale_y_continuous(limits = c(60,100), 
                     breaks=seq(60, 100, by = 10)) + 
  scale_x_continuous(limits = c(0, 22), breaks = seq(0,22, by = 1), 
                     labels = paste(avg_gt9x_weekly_filtered$redcap_event_name, "\n(n=", avg_gt9x_weekly_filtered$count, ")", sep = "")) +

  geom_errorbar(aes(ymin = ci_lower_Efficiency_Mean, ymax = ci_upper_Efficiency_Mean), width = 0.2) +
  labs(title = "Mean Sleep Efficiency across the week",
       x = "Weeks",
       y = "Sleep Efficiency (%)")+ theme_light()
ggsave(file.path(outputDirFig,"avg_Efficiency_mean_weekly.png"), plot = weekly_eff_mean_plot, width = 16, height = 3.5, dpi = 300)

weekly_eff_Median_plot <- ggplot(avg_gt9x_weekly_filtered, aes(x = redcap_event_name , y = avg_Efficiency_Median)) +
  geom_line() +  
  geom_point() +
  scale_y_continuous(limits = c(60,100), 
                     breaks=seq(60, 100, by = 10)) + 
  scale_x_continuous(limits = c(0, 22), breaks = seq(0,22, by = 1), 
                     labels = paste(avg_gt9x_weekly_filtered$redcap_event_name, "\n(n=", avg_gt9x_weekly_filtered$count, ")", sep = "")) +
  
  geom_errorbar(aes(ymin = ci_lower_Efficiency_Median, ymax = ci_upper_Efficiency_Median), width = 0.2) +
  labs(title = "Median Sleep Efficiency across the week",
       x = "Weeks",
       y = "Sleep Efficiency (%)")+ theme_light()

ggsave(file.path(outputDirFig,"avg_Efficiency_Median_weekly.png"), plot = weekly_eff_Median_plot, width = 16, height = 3.5, dpi = 300)


#
weekly_tst_mean_plot <- ggplot(avg_gt9x_weekly_filtered, aes(x = redcap_event_name , y = avg_tst_Mean)) +
  geom_line() +  
  geom_point() +  ylim (0,850)+ 
  scale_x_continuous(limits = c(0, 22), breaks = seq(0,22, by = 1), 
                     labels = paste(avg_gt9x_weekly_filtered$redcap_event_name, "\n(n=", avg_gt9x_weekly_filtered$count, ")", sep = "")) +
  
  geom_errorbar(aes(ymin = ci_lower_tst_Mean, ymax = ci_upper_tst_Mean), width = 0.2) +
  labs(title = "Mean Total Sleep Time across the week",
       x = "Weeks",
       y = "Total Sleep Time (min)")+ theme_light()
weekly_tst_mean_plot
ggsave(file.path(outputDirFig,"avg_tst_mean_weekly.png"), plot = weekly_tst_mean_plot, width = 16, height = 3.5, dpi = 300)

weekly_tst_Median_plot <- ggplot(avg_gt9x_weekly_filtered, aes(x = redcap_event_name , y = avg_tst_Median)) +
  geom_line() +  
  geom_point() +  ylim (0,850)+
  scale_x_continuous(limits = c(0, 22), breaks = seq(0,22, by = 1), 
                     labels = paste(avg_gt9x_weekly_filtered$redcap_event_name, "\n(n=", avg_gt9x_weekly_filtered$count, ")", sep = "")) +
  
  geom_errorbar(aes(ymin = ci_lower_tst_Median, ymax = ci_upper_tst_Median), width = 0.2) +
  labs(title = "Median Total Sleep Time across the week",
       x = "Weeks",
       y = "Total Sleep Time (min)")+ theme_light()
weekly_tst_Median_plot
ggsave(file.path(outputDirFig,"avg_tst_Median_weekly.png"), plot = weekly_tst_Median_plot, width = 16, height = 3.5, dpi = 300)

# WASO
weekly_waso_mean_plot <- ggplot(avg_gt9x_weekly_filtered, aes(x = redcap_event_name , y = avg_waso_Mean)) +
  geom_line() +  
  geom_point() +  ylim (0,200)+ 
  scale_x_continuous(limits = c(0, 22), breaks = seq(0,22, by = 1), 
                     labels = paste(avg_gt9x_weekly_filtered$redcap_event_name, "\n(n=", avg_gt9x_weekly_filtered$count, ")", sep = "")) +
  
  geom_errorbar(aes(ymin = ci_lower_waso_Mean, ymax = ci_upper_waso_Mean), width = 0.2) +
  labs(title = "Mean Wakefulness After Sleep Onset across the week",
       x = "Weeks",
       y = "Wakefulness After Sleep Onset (min)")+ theme_light()
weekly_waso_mean_plot
ggsave(file.path(outputDirFig,"avg_waso_mean_weekly.png"), plot = weekly_waso_mean_plot, width = 16, height = 3.5, dpi = 300)

weekly_waso_Median_plot <- ggplot(avg_gt9x_weekly_filtered, aes(x = redcap_event_name , y = avg_waso_Median)) +
  geom_line() +  
  geom_point() +  ylim (0,200)+
  scale_x_continuous(limits = c(0, 22), breaks = seq(0,22, by = 1), 
                     labels = paste(avg_gt9x_weekly_filtered$redcap_event_name, "\n(n=", avg_gt9x_weekly_filtered$count, ")", sep = "")) +
  
  geom_errorbar(aes(ymin = ci_lower_waso_Median, ymax = ci_upper_waso_Median), width = 0.2) +
  labs(title = "Median Wakefulness After Sleep Onset across the week",
       x = "Weeks",
       y = "Wakefulness After Sleep Onset (min)")+ theme_light()
weekly_waso_Median_plot
ggsave(file.path(outputDirFig,"avg_waso_Median_weekly.png"), plot = weekly_waso_Median_plot, width = 16, height = 3.5, dpi = 300)



#####################################
library(dplyr)
library(ggplot2)
library(lcmm)
library(gbmt) # Group-Based Multivariate Trajectory Modeling
# library(mlogit) # multinomial logistic regression
outputDir <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results"

###GT9X
gt9x_weekly_data <- gt9x_weekly_data %>% 
  filter(redcap_event_name != "") %>%
  filter(redcap_event_name != 0)
  
avg_gt9x_weekly_filtered <- avg_gt9x_weekly_filtered %>%
  filter(redcap_event_name != 0)

##### Efficiency Mean
gt9x_Efficiency_Mean <- gt9x_weekly_data %>%
  filter(!is.na(Efficiency_Mean),  
         redcap_event_name >= 0 & day_from_first_ptvisit <= 20)

gt9x_Efficiency <- gt9x_daily_data %>%
  filter(!is.na(Efficiency),  
         day_from_first_ptvisit >= 0 & day_from_first_ptvisit <= 85)

mlin <- lcmm(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency)
mlin2 <- hlme(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency) 
mbeta <- lcmm(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'beta', data = gt9x_Efficiency) 
mspl <- lcmm(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = 'splines', data = gt9x_Efficiency) 
mspl5 <- lcmm(Efficiency ~ day_from_first_ptvisit , random = ~ day_from_first_ptvisit, subject = "record_id", link = '5-quant-splines', data = gt9x_Efficiency) 

summarytable(mlin, mbeta, mspl, mspl5,which = c("loglik", "conv", "npm", "AIC", "SABIC")) # LOWER THE BETTER? 

col <- rainbow(5)
plot(mlin, which="linkfunction", bty='l', ylab="Efficiency", col=col[1], lwd=2, xlab="underlying latent process")
plot(mbeta, which="linkfunction", add=TRUE, col=col[2], lwd=2)
plot(mspl, which="linkfunction", add=TRUE, col=col[3], lwd=2)
plot(mspl5, which="linkfunction", add=TRUE, col=col[4], lwd=2)
legend(x="topleft", legend=c("linear", "beta","splines (5equidistant)","splines (5 at quantiles)"), lty=1, col=col, bty="n", lwd=2)

linkspl5q <- predictlink(mspl5,ndraws=2000)
plot(linkspl5q, col=col[4], lty=2, shades=TRUE)
legend(x="left", legend=c("95% confidence bands","for splines at quantiles"),lty=c(2,NA), col=c(col[4],NA), bty="n", lwd=1, cex=0.8)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_Efficiency_Mean) 
f_beta2 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 2, B = f_beta1, mixture = ~ redcap_event_name)
f_beta3 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "beta", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 3, B = f_beta1, mixture = ~ redcap_event_name)

# Linear 
f_linear1 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", ng = 1, data = gt9x_Efficiency_Mean) 
f_linear2 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 2, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear3 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 3, B = f_linear1, mixture = ~ redcap_event_name)  
f_linear4 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 4, B = f_linear1, mixture = ~ redcap_event_name) 
f_linear5 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 5, B = f_linear1, mixture = ~ redcap_event_name) 
# f_linear6 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 6, B = f_linear1, mixture = ~ redcap_event_name) 
# f_linear7 <- lcmm(Efficiency_Mean ~ redcap_event_name , link = "linear", random = ~ redcap_event_name, subject = "record_id", data = gt9x_Efficiency_Mean, ng = 7, B = f_linear1, mixture = ~ redcap_event_name) 

# Compare models; AIC/BIC/SABIC (smaller, better fitting), 
# entropy (>0.8, well-separated class), ICL1/2 (lower, better balance between model fit)
summarytable(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5, 
            which=c("loglik", "conv", "npm", "AIC", "BIC","entropy","ICL", "SABIC"), bty="l",pch=20,col=2)
summary(f_linear5)

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
         day_from_first_ptvisit >= -6 & day_from_first_ptvisit <= 125)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_Efficiency) 
f_beta2 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 2, B = f_beta1, mixture = ~ day_from_first_ptvisit)
f_beta3 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 3, B = f_beta1, mixture = ~ day_from_first_ptvisit)

# Linear 
f_linear1 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_Efficiency) 
f_linear2 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 2, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear3 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 3, B = f_linear1, mixture = ~ day_from_first_ptvisit)  
f_linear4 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 4, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear5 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 5, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
# f_linear6 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 6, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
# f_linear7 <- lcmm(Efficiency ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_Efficiency, ng = 7, B = f_linear1, mixture = ~ day_from_first_ptvisit) 

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
plot(f_linear3, which = "fit",  var.time= "day_from_first_ptvisit")

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
  group_by(class_eff, day_from_first_ptvisit) %>%
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
eff_plot <- ggplot(avg_eff, aes(x = day_from_first_ptvisit, y = avg_Efficiency, color = class_eff, group = class_eff)) +  
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
         day_from_first_ptvisit >= -6 & day_from_first_ptvisit <= 125)

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(tst ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_tst) 
f_beta2 <- lcmm(tst ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 2, B = f_beta1, mixture = ~ day_from_first_ptvisit)
f_beta3 <- lcmm(tst ~ day_from_first_ptvisit , link = "beta", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 3, B = f_beta1, mixture = ~ day_from_first_ptvisit)

# Linear 
f_linear1 <- lcmm(tst ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", ng = 1, data = gt9x_tst) 
f_linear2 <- lcmm(tst ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 2, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear3 <- lcmm(tst ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 3, B = f_linear1, mixture = ~ day_from_first_ptvisit)  
f_linear4 <- lcmm(tst ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 4, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
f_linear5 <- lcmm(tst ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 5, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
# f_linear6 <- lcmm(tst ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 6, B = f_linear1, mixture = ~ day_from_first_ptvisit) 
# f_linear7 <- lcmm(tst ~ day_from_first_ptvisit , link = "linear", random = ~ day_from_first_ptvisit, subject = "record_id", data = gt9x_tst, ng = 7, B = f_linear1, mixture = ~ day_from_first_ptvisit) 

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
plot(f_linear3, which = "fit",  var.time= "day_from_first_ptvisit")

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

