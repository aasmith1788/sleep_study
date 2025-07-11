library(dplyr)
library(ggplot2)
library(lcmm)
library(gbmt) # Group-Based Multivariate Trajectory Modeling
# library(mlogit) # multinomial logistic regression

# Import the data
outputDir <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results"

####### KOOS + WOMAC
data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/KOOS_weekly.csv")

# KOOS + WOMAC 
data$koos_pain  <- as.numeric(data$koos_pain, na.rm=TRUE)
data$koos_adl  <- as.numeric(data$koos_adl, na.rm=TRUE)
data$kd_womac_pain <- as.numeric(data$kd_womac_pain, na.rm=TRUE)
data$kd_womac_stiff <- as.numeric(data$kd_womac_stiff, na.rm=TRUE)
data$kd_womac_func <- as.numeric(data$kd_womac_func, na.rm=TRUE)

# Create Mean + SD (Characteristics)
avg_koos <- data %>%
  group_by(redcap_repeat_instance_dv) %>%
  summarise(avg_koosp = mean(koos_pain, na.rm=TRUE), 
            sd_koosp = sd(koos_pain, na.rm = TRUE), 
            n_koosp = sum(!is.na(koos_pain)), 
            
            avg_koosadl = mean(koos_adl, na.rm=TRUE), 
            sd_koosadl = sd(koos_adl, na.rm = TRUE), 
            n_koosadl = sum(!is.na(koos_adl)), 
            
            avg_kdp = mean(kd_womac_pain, na.rm=TRUE), 
            sd_kdp = sd(kd_womac_pain, na.rm = TRUE), 
            n_kdp = sum(!is.na(kd_womac_pain)), 
            
            avg_kdf = mean(kd_womac_func, na.rm=TRUE), 
            sd_kdf = sd(kd_womac_func, na.rm = TRUE), 
            n_kdf = sum(!is.na(kd_womac_func)), 
            
            avg_kds = mean(kd_womac_stiff, na.rm=TRUE), 
            sd_kds = sd(kd_womac_stiff, na.rm = TRUE), 
            n_kds = sum(!is.na(kd_womac_stiff)))

##### WOMAC Function
data_func<- data[complete.cases(data$kd_womac_func), c("record_id", "redcap_repeat_instance_dv", "kd_womac_func")] 
colnames(data_func)[colnames(data_func) == "redcap_repeat_instance_dv"] <- "weeks"
data_func <- data_func[data_func$weeks >= 0 & data_func$weeks <= 19, ]
data_func <- data_func[data_func$record_id != '108', ] # Exclude the outlier

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
f_beta1 <- lcmm(kd_womac_func ~ weeks , link = "beta", random = ~ weeks, subject = "record_id", ng = 1, data = data_func) 
f_beta2 <- lcmm(kd_womac_func ~ weeks , link = "beta", random = ~ weeks, subject = "record_id", data = data_func, ng = 2, B = f_beta1, mixture = ~ weeks)
f_beta3 <- lcmm(kd_womac_func ~ weeks , link = "beta", random = ~ weeks, subject = "record_id", data = data_func, ng = 3, B = f_beta1, mixture = ~ weeks)

# Linear 
f_linear1 <- lcmm(kd_womac_func ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", ng = 1, data = data_func) 
f_linear2 <- lcmm(kd_womac_func ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_func, ng = 2, B = f_linear1, mixture = ~ weeks) 
f_linear3 <- lcmm(kd_womac_func ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_func, ng = 3, B = f_linear1, mixture = ~ weeks)  
f_linear4 <- lcmm(kd_womac_func ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_func, ng = 4, B = f_linear1, mixture = ~ weeks) 
f_linear5 <- lcmm(kd_womac_func ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_func, ng = 5, B = f_linear1, mixture = ~ weeks) 
f_linear6 <- lcmm(kd_womac_func ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_func, ng = 6, B = f_linear1, mixture = ~ weeks) 
f_linear7 <- lcmm(kd_womac_func ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_func, ng = 7, B = f_linear1, mixture = ~ weeks) 

# Compare models; BIC/SABIC (smaller, better fitting), entropy (>0.8, well-separated class), ICL (lower, better balance between model fit)
summarytable(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5, f_linear6, f_linear7,
             which = c("G","AIC", "BIC", "SABIC", "entropy", "ICL", "ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(f_linear1, f_linear2, f_linear3,  f_linear4,  f_linear5, f_linear6, f_linear7,which=c("loglik", "conv", "npm", "AIC", "BIC","entropy","ICL", "SABIC"), bty="l",pch=20,col=2)
summary(f_linear3)

# Contingency table of two classifications
xclass(f_linear3, f_linear2)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(f_linear3)

# PLOT
plot(f_linear3, which = "fit", marg=FALSE, var.time= "weeks", bty="n")

# Classification and  Export
func_df <- f_linear3$pprob
write.csv(func_df, file = file.path(outputDir, "/ClassMembership_womac_func_wo108.csv"), row.names = FALSE)

##### WOMAC PAIN
data_pain <- data[complete.cases(data$kd_womac_pain), c("record_id", "redcap_repeat_instance_dv", "kd_womac_pain")] 
colnames(data_pain)[colnames(data_pain) == "redcap_repeat_instance_dv"] <- "weeks"
data_pain <- data_pain[data_pain$weeks >= 0 & data_pain$weeks <= 19, ]
data_pain <- data_pain[data_pain$record_id != '108', ] # Exclude the outlier

##### LCMM PACKAGE: Univaritate (LCMM)
# LINK: "linear" (linear transformation), "beta"(rescaled Beta CDF), "thresholds" (cumulative probit model), "splines"(I-splines)
# Beta
p_beta1 <- lcmm(kd_womac_pain ~ weeks , link = "beta", random = ~ weeks, subject = "record_id", ng = 1, data = data_pain) 
p_beta2 <- lcmm(kd_womac_pain ~ weeks , link = "beta", random = ~ weeks, subject = "record_id", data = data_pain, ng = 2, B = p_beta1, mixture = ~ weeks)
p_beta3 <- lcmm(kd_womac_pain ~ weeks , link = "beta", random = ~ weeks, subject = "record_id", data = data_pain, ng = 3, B = p_beta1, mixture = ~ weeks)

# Linear 
p_linear1 <- lcmm(kd_womac_pain ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", ng = 1, data = data_pain) 
p_linear2 <- lcmm(kd_womac_pain ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_pain, ng = 2, B = p_linear1, mixture = ~ weeks) 
p_linear3 <- lcmm(kd_womac_pain ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_pain, ng = 3, B = p_linear1, mixture = ~ weeks)  
p_linear4 <- lcmm(kd_womac_pain ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_pain, ng = 4, B = p_linear1, mixture = ~ weeks) 
p_linear5 <- lcmm(kd_womac_pain ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_pain, ng = 5, B = p_linear1, mixture = ~ weeks) 
p_linear6 <- lcmm(kd_womac_pain ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_pain, ng = 6, B = p_linear1, mixture = ~ weeks) 
p_linear7 <- lcmm(kd_womac_pain ~ weeks , link = "linear", random = ~ weeks, subject = "record_id", data = data_pain, ng = 7, B = p_linear1, mixture = ~ weeks) 

# Compare models; BIC/SABIC (smaller, better fitting), entropy (>0.8, well-separated class), ICL (lower, better balance between model fit)
summarytable(p_linear1, p_linear2, p_linear3, p_linear4, p_linear5,  p_linear6,  p_linear7, 
             which = c("G", "AIC", "BIC", "SABIC", "entropy", "ICL","ICL1", "ICL2", "%class"))

par(mfrow=c(1, 1))
summaryplot(p_linear1, p_linear2, p_linear3, p_linear4, p_linear5, p_linear6,  p_linear7,  which=c("loglik", "conv", "npm", "AIC", "BIC","entropy","ICL", "SABIC"), bty="l",pch=20,col=2)
summary(p_linear3)

# Contingency table of two classifications
xclass(p_linear3, p_linear4)

# Average Posterior Probability of Assignment (APPA, > 0.7 for each class)
postprob(p_linear4)

# PLOT
plot(p_linear4, which = "fit", marg=FALSE, var.time= "weeks", bty="n")


# Classification and  Export
pain_df <- p_linear4$pprob
write.csv(pain_df, file = file.path(outputDir, "/ClassMembership_womac_pain_wo108.csv"), row.names = FALSE)


##### KOOS PAIN
# KOOS Pain
data$koos_wk_p1 <- as.numeric(data$koos_wk_p1, na.rm=TRUE)
data$koos_wk_p2 <- as.numeric(data$koos_wk_p2, na.rm=TRUE)
data$koos_wk_p3 <- as.numeric(data$koos_wk_p3, na.rm=TRUE)
data$koos_wk_p4 <- as.numeric(data$koos_wk_p4, na.rm=TRUE)
data$koos_wk_p5 <- as.numeric(data$koos_wk_p5, na.rm=TRUE)
data$koos_wk_p6 <- as.numeric(data$koos_wk_p6, na.rm=TRUE)
data$koos_wk_p7 <- as.numeric(data$koos_wk_p7, na.rm=TRUE)
data$koos_wk_p8 <- as.numeric(data$koos_wk_p8, na.rm=TRUE)
data$koos_wk_p9 <- as.numeric(data$koos_wk_p9, na.rm=TRUE)

avg_koosp <- data %>%
  group_by(redcap_repeat_instance_dv) %>%
  summarise(avg_koosp1 = mean(koos_wk_p1, na.rm=TRUE), 
            sd_koosp1 = sd(koos_wk_p1, na.rm = TRUE), 
            
            avg_koosp2 = mean(koos_wk_p2, na.rm=TRUE), 
            sd_koosp2 = sd(koos_wk_p2, na.rm = TRUE), 
            avg_koosp3 = mean(koos_wk_p3, na.rm=TRUE), 
            sd_koosp3 = sd(koos_wk_p3, na.rm = TRUE), 
            avg_koosp4 = mean(koos_wk_p4, na.rm=TRUE), 
            sd_koosp4 = sd(koos_wk_p4, na.rm = TRUE), 
            avg_koosp5 = mean(koos_wk_p5, na.rm=TRUE), 
            sd_koosp5 = sd(koos_wk_p5, na.rm = TRUE), 
            avg_koosp6 = mean(koos_wk_p6, na.rm=TRUE), 
            sd_koosp6 = sd(koos_wk_p6, na.rm = TRUE), 
            avg_koosp7 = mean(koos_wk_p7, na.rm=TRUE), 
            sd_koosp7 = sd(koos_wk_p7, na.rm = TRUE), 
            avg_koosp8 = mean(koos_wk_p8, na.rm=TRUE), 
            sd_koosp8 = sd(koos_wk_p8, na.rm = TRUE), 
            avg_koosp9 = mean(koos_wk_p9, na.rm=TRUE), 
            sd_koosp9 = sd(koos_wk_p9, na.rm = TRUE), 
            
            n_koos = sum(!is.na(koos_pain)))
#### DATAFRAME
data_koos <- data[complete.cases(data$koos_pain), c("record_id", "redcap_repeat_instance_dv", "koos_pain")] 
colnames(data_koos)[colnames(data_koos) == "redcap_repeat_instance_dv"] <- "weeks"
data_koos <- data_koos[data_koos$weeks >= 0 & data_koos$weeks <= 18, ]

# KOOS PAIN
varNames <- c("koos_pain")
m3_1 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=1, ng=3, data=data_koos, scaling=0)
m3_2 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=2, ng=3, data=data_koos, scaling=0)
m3_3 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=3, ng=3, data=data_koos, scaling=0)
m3_4 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=4, ng=3, data=data_koos, scaling=0)

rbind(m3_4$ic, m3_2$ic) #compare models

# Summary of regressions by group
summary(m3_1)
m3_1$assign.list
m3_1$fitted

# PLOT
plot(m3_2, n.ahead=3, mar=mar0) ## overlapped groups
plot(m3_2, group=1, n.ahead=3, mar=mar0) ## group 1
plot(m3_2, group=2, n.ahead=3, mar=mar0) ## group 2
plot(m3_2, group=3, n.ahead=3, mar=mar0) ## group 3


#### Run with gbmt (Expectation-Maximization (EM) algorithm)
# Higher log-likelihood is better
# scaling: 0 (no normalisation), 1 (centering), 2 (standardization), 3 (ratio to the mean) and 4 (logarithmic ratio to the mean) 
# ng = number of group 
# d = polynomial degree of group trajectories



# Information Criteria values (Likelihood-based)
varNames <- c("kd_womac_func")
m3_1 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=1, ng=3, data=data_func, scaling=0)
m4_1 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=1, ng=4, data=data_func, scaling=0)
m3_2 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=2, ng=4, data=data_func, scaling=0)
m3_3 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=3, ng=5, data=data_func, scaling=0)
m3_4 <- gbmt(x.names=varNames, unit="record_id", time="weeks", d=4, ng=4, data=data_func, scaling=0)

# Calculate the log-likelihood 
log_likelihood_m3_1 <- logLik(m3_1)
log_likelihood_m4_1 <- logLik(m4_1)
log_likelihood_m3_2 <- logLik(m3_2)
log_likelihood_m3_3 <- logLik(m3_3)
log_likelihood_m3_4 <- logLik(m3_4)

# Count the number of estimated parameters
k_m3_1 <- length(coef(m3_1))
k_m4_1 <- length(coef(m4_1))
k_m3_2 <- length(coef(m3_2))
k_m3_3 <- length(coef(m3_3))
k_m3_4 <- length(coef(m3_4))

# sample size
n <- length(unique(data_func$record_id))

# Calculate the SABIC for each model
SABIC_m3_1 <- -2 * log_likelihood_m3_1 + k_m3_1 * log(n)
SABIC_m4_1 <- -2 * log_likelihood_m4_1 + k_m4_1 * log(n)
SABIC_m3_2 <- -2 * log_likelihood_m3_2 + k_m3_2 * log(n)
SABIC_m3_3 <- -2 * log_likelihood_m3_3 + k_m3_3 * log(n)
SABIC_m3_4 <- -2 * log_likelihood_m3_4 + k_m3_4 * log(n)

#### COMPARE MODELS
# SABIC (smaller, better fitting)
SABIC_m3_1
SABIC_m4_1
SABIC_m3_2
SABIC_m3_3 #SMALLEST
SABIC_m3_4

# AIC, BIC (smaller, better fitting)
rbind(m3_3$ic, m3_4$ic) 


# Print or use the average posterior probability


# Summary of regressions by group
summary(m3_3)
m3_3$assign.list
m3_3$fitted