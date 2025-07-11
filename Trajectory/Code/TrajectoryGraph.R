library(dplyr)
library(ggplot2)
library(ggthemes)

# Import the data
outputDir <- "R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Results"

####### DSIS 
data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/DSIS_daily.csv")
data$dsis_02bdbc <- as.numeric(data$dsis_02bdbc, na.rm=TRUE)

data
# Average DSIS
avg_dsis <- data %>%
  group_by(redcap_repeat_instance_dv) %>%
  summarise(avg_dsis = mean(dsis_02bdbc, na.rm=TRUE), 
            sd_dsis = sd(dsis_02bdbc, na.rm = TRUE), 
            count = sum(!is.na(dsis_02bdbc)))

# Plot
dsis_plot <- ggplot(avg_dsis, aes(x = redcap_repeat_instance_dv, y = avg_dsis)) +
  geom_line() +
  ylim(-2,10) + scale_x_continuous(limits = c(0, 136), breaks = seq(0, 136, by = 15)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_dsis - sd_dsis, ymax = avg_dsis + sd_dsis), width = 0.2) +
  labs(title = "Averaged DSIS by days",
       x = "Days",
       y = "Averaged DSIS (0-10)")+ 
  theme_hc(base_size = 14)
dsis_plot

# Export
write.csv(avg_dsis, file = file.path(outputDir, "/avg_dsis.csv"), row.names = FALSE)
ggsave(file.path(outputDir,"avg_dsis.png"), plot = dsis_plot, width = 16, height = 6, dpi = 300)

####### KOOS + WOMAC
data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/KOOS_weekly.csv")

# KOOS
data$koos_pain  <- as.numeric(data$koos_pain, na.rm=TRUE)
data$koos_adl  <- as.numeric(data$koos_adl, na.rm=TRUE)
data$kd_womac_pain <- as.numeric(data$kd_womac_pain, na.rm=TRUE)
data$kd_womac_stiff <- as.numeric(data$kd_womac_stiff, na.rm=TRUE)
data$kd_womac_func <- as.numeric(data$kd_womac_func, na.rm=TRUE)
 
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

# Plot KOOS Pain Questions
ggplot(avg_koosp, aes(x = redcap_repeat_instance_dv, y = avg_koosp6)) +
  geom_line() +
  ylim(0, 4) + scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_koosp6 - sd_koosp1, ymax = avg_koosp6 + sd_koosp1), width = 0.2) +
  labs(title = "Averaged KOOS Pain Question by weeks",
       x = "Weeks",
       y = "Averaged KOOS Pain Question (0-4)") + 
  theme_hc(base_size = 14)

# Plot KOOS
ggplot(data, aes(x = redcap_repeat_instance_dv, y = koos_pain, group = record_id, color = as.factor(record_id))) +
  geom_line() +
  geom_point() +
  labs(title = "Individual Pain Trajectories Over Time",
       x = "weeks",
       y = "Pain Score") +
  theme_minimal()


# Plot KOOS
koosp_plot <- ggplot(avg_koos, aes(x = redcap_repeat_instance_dv, y = avg_koosp)) +
  geom_line() +
  ylim(0, 100) + scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_koosp - sd_koosp, ymax = avg_koosp + sd_koosp), width = 0.2) +
  labs(title = "Averaged KOOS Pain by weeks",
       x = "Weeks",
       y = "Averaged KOOS Pain (0-100)")+ 
  theme_hc(base_size = 14)
koosp_plot

koosadl_plot <- ggplot(avg_koos, aes(x = redcap_repeat_instance_dv, y = avg_koosadl)) +
  geom_line() +
  ylim(0, 100) + scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_koosadl - sd_koosadl, ymax = avg_koosadl + sd_koosadl), width = 0.2) +
  labs(title = "Averaged KOOS ADL by weeks",
       x = "Weeks",
       y = "Averaged KOOS ADL (0-100)")+ 
  theme_hc(base_size = 14)
koosadl_plot

womacp_plot <- ggplot(avg_koos, aes(x = redcap_repeat_instance_dv, y = avg_kdp)) +
  geom_line() +
  ylim(0, 20) + scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_kdp - sd_kdp, ymax = avg_kdp + sd_kdp), width = 0.2) +
  labs(title = "Averaged WOMAC Pain by weeks",
       x = "Weeks",
       y = "Averaged WOMAC Pain (0-20)")+ 
  theme_hc(base_size = 14)
womacp_plot

womacf_plot <- ggplot(avg_koos, aes(x = redcap_repeat_instance_dv, y = avg_kdf)) +
  geom_line() +
  ylim(0, 68) + scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, by = 3)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_kdf - sd_kdf, ymax = avg_kdf + sd_kdf), width = 0.2) +
  labs(title = "Averaged WOMAC Function by weeks",
       x = "Weeks",
       y = "Averaged WOMAC Function (0-68)")+ 
  theme_hc(base_size = 14)
womacf_plot

womacs_plot <- ggplot(avg_koos, aes(x = redcap_repeat_instance_dv, y = avg_kds)) +
  geom_line() +
  ylim(0, 8) + scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, by = 3)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_kds - sd_kds, ymax = avg_kds + sd_kds), width = 0.2) +
  labs(title = "Averaged WOMAC Stiffness by weeks",
       x = "Weeks",
       y = "Averaged WOMAC Stiffness (0-8)")+ 
  theme_hc(base_size = 14)
womacs_plot

# Export
write.csv(avg_koos, file = file.path(outputDir, "/avg_koos.csv"), row.names = FALSE)
write.csv(avg_koosp, file = file.path(outputDir, "/avg_koos_painq.csv"), row.names = FALSE)
ggsave(file.path(outputDir,"avg_koosp.png"), plot = koosp_plot, width = 16, height = 6, dpi = 300)
ggsave(file.path(outputDir,"avg_koosadl.png"), plot = koosadl_plot, width = 16, height = 6, dpi = 300)
ggsave(file.path(outputDir,"avg_womacp.png"), plot = womacp_plot, width = 16, height = 6, dpi = 300)
ggsave(file.path(outputDir,"avg_womacf.png"), plot = womacf_plot, width = 16, height = 6, dpi = 300)
ggsave(file.path(outputDir,"avg_womacs.png"), plot = womacs_plot, width = 16, height = 6, dpi = 300)

####### PGAOA
data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/pgaoa_weekly.csv")
data$pgaoa <- as.numeric(data$pgaoa, na.rm=TRUE)

# Average PGA-OA
avg_pgaoa <- data %>%
  group_by(redcap_repeat_instance_dv) %>%
  summarise(avg_pgaoa = mean(pgaoa, na.rm=TRUE), 
            sd_pgaoa = sd(pgaoa, na.rm = TRUE), 
            n_pgaoa = sum(!is.na(pgaoa)))

# Plot
pgaoa_plot <- ggplot(avg_pgaoa, aes(x = redcap_repeat_instance_dv, y = avg_pgaoa)) +
  geom_line() +
  ylim(0,100) + scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, by = 3)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_pgaoa - sd_pgaoa, ymax = avg_pgaoa + sd_pgaoa), width = 0.2) +
  labs(title = "Averaged patient global assessment (PGA-OA) by weeks",
       x = "Weeks",
       y = "Averaged PGA-OA (0-100)")+ 
  theme_hc(base_size = 14)
pgaoa_plot

# Export
write.csv(avg_pgaoa, file = file.path(outputDir, "/avg_pgaoa.csv"), row.names = FALSE)
ggsave(file.path(outputDir,"avg_pgaoa.png"), plot = pgaoa_plot, width = 16, height = 6, dpi = 300)

####### NRSna
data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/nrsna_weekly.csv")
data$nrsna_pain <- as.numeric(data$nrsna_pain, na.rm=TRUE)

# Average PGA-OA
avg_nrsna <- data %>%
  group_by(redcap_repeat_instance_dv) %>%
  summarise(avg_nrsna = mean(nrsna_pain, na.rm=TRUE), 
            sd_nrsna = sd(nrsna_pain, na.rm = TRUE), 
            n_nrsna = sum(!is.na(nrsna_pain)))

# Plot
nrsna_plot <- ggplot(avg_nrsna, aes(x = redcap_repeat_instance_dv, y = avg_nrsna)) +
  geom_line() +
  ylim(0,10) + scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_nrsna - sd_nrsna, ymax = avg_nrsna + sd_nrsna), width = 0.2) +
  labs(title = "Averaged pain during nominated activity (NRSna) by weeks",
       x = "Weeks",
       y = "Averaged NRSna (0-10)")+ 
  theme_hc(base_size = 14)
nrsna_plot

# Export
write.csv(avg_nrsna, file = file.path(outputDir, "/avg_nrsna.csv"), row.names = FALSE)
ggsave(file.path(outputDir,"avg_nrsna.png"), plot = nrsna_plot, width = 16, height = 6, dpi = 300)

####### PT Visit
data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/pt_nrs_weekly.csv")
data$eval2_tx_mod1 <- as.numeric(data$eval2_tx_mod1, na.rm=TRUE)
data$eval2_end_pain <- as.numeric(data$eval2_end_pain, na.rm=TRUE)
data$eval2_mep <- data$eval2_end_pain - data$eval2_tx_mod1 

# Average PT NRS
avg_pt_nrs <- data %>%
  group_by(eval2_visit) %>%
  summarise(avg_prept = mean(eval2_tx_mod1, na.rm=TRUE), 
            sd_prept = sd(eval2_tx_mod1, na.rm = TRUE), 
            n_prept = sum(!is.na(eval2_tx_mod1)), 
            
            avg_postpt = mean(eval2_end_pain, na.rm=TRUE), 
            sd_postpt = sd(eval2_end_pain, na.rm = TRUE), 
            n_postpt = sum(!is.na(eval2_end_pain)),
            
            avg_meppt = mean(eval2_mep, na.rm=TRUE), 
            sd_meppt = sd(eval2_mep, na.rm = TRUE), 
            n_meppt = sum(!is.na(eval2_mep)))

# Plot
pt_nrs_plot <- ggplot(avg_pt_nrs, aes(x = eval2_visit, y = avg_meppt)) +
  geom_line() +
  ylim(-2,10) + scale_x_continuous(limits = c(1, 18), breaks = seq(1, 18, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_meppt - sd_meppt, ymax = avg_meppt + sd_meppt), width = 0.2) +
  labs(title = "Averaged pain flare evoked by PT by sessions",
       x = "Sessions",
       y = "Averaged pain flare evoked by PT (0-10)")+ 
  theme_hc(base_size = 14)
pt_nrs_plot

# Export
write.csv(avg_pt_nrs, file = file.path(outputDir, "/avg_pt_nrs.csv"), row.names = FALSE)
ggsave(file.path(outputDir,"avg_pt_nrs_mep_plot.png"), plot = pt_nrs_plot, width = 16, height = 6, dpi = 300)


####### PT Attend
data <- read.csv("R:/KumarLab3/PROJECTS/wesens/Data/Analysis/Kim_E/Pain Trajectory/Data/pt_survey_weekly.csv")

data$kos_adl <- as.numeric(data$kos_adl, na.rm=TRUE)
data$kos_global <- as.numeric(data$kos_global, na.rm=TRUE)
data$kos_pain <- as.numeric(data$kos_pain, na.rm=TRUE)
data$ps_1 <- as.numeric(data$ps_1, na.rm=TRUE)
data$ps_2 <- as.numeric(data$ps_2, na.rm=TRUE)
data$ps_3 <- as.numeric(data$ps_3, na.rm=TRUE)

# Average PT Survey
avg_pt <- data %>%
  group_by(eval2_visit) %>%
  summarise(avg_kosa = mean(kos_adl, na.rm=TRUE), 
            sd_kosa = sd(kos_adl, na.rm = TRUE), 
            n_kosa = sum(!is.na(kos_adl)), 
            
            avg_kosg = mean(kos_global, na.rm=TRUE), 
            sd_kosg = sd(kos_global, na.rm = TRUE), 
            n_kosg = sum(!is.na(kos_global)),
            
            avg_kosp = mean(kos_pain, na.rm=TRUE), 
            sd_kosp = sd(kos_pain, na.rm = TRUE), 
            n_kosp = sum(!is.na(kos_pain)), 
            
            avg_ps1 = mean(ps_1, na.rm=TRUE), 
            sd_ps1 = sd(ps_1, na.rm = TRUE), 
            n_ps1 = sum(!is.na(ps_1)), 
            avg_ps2 = mean(ps_2, na.rm=TRUE), 
            sd_ps2 = sd(ps_2, na.rm = TRUE), 
            n_ps2 = sum(!is.na(ps_2)),
            avg_ps3 = mean(ps_3, na.rm=TRUE), 
            sd_ps3 = sd(ps_3, na.rm = TRUE), 
            n_ps3 = sum(!is.na(ps_3)))

# Plot
kosa_plot <- ggplot(avg_pt, aes(x = eval2_visit, y = avg_kosa)) +
  geom_line() +
  ylim(0,100) + scale_x_continuous(limits = c(1, 18), breaks = seq(1, 18, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_kosa - sd_kosa, ymax = avg_kosa + sd_kosa), width = 0.2) +
  labs(title = "Averaged KOS ADL (0-100) by PT sessions",
       x = "PT Sessions",
       y = "Averaged KOS ADL (0-100)")+ 
  theme_hc(base_size = 14)
kosa_plot

kosg_plot <- ggplot(avg_pt, aes(x = eval2_visit, y = avg_kosg)) +
  geom_line() +
  ylim(0,100) + scale_x_continuous(limits = c(1, 18), breaks = seq(1, 18, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_kosg - sd_kosg, ymax = avg_kosg + sd_kosg), width = 0.2) +
  labs(title = "Averaged overall function of knee (0-100) by PT sessions",
       x = "PT Sessions",
       y = "Averaged overall function of knee (0-100)")+ 
  theme_hc(base_size = 14)
kosg_plot

ps1_plot <- ggplot(avg_pt, aes(x = eval2_visit, y = avg_ps1)) +
  geom_line() +
  ylim(-1,10) + scale_x_continuous(limits = c(1, 18), breaks = seq(1, 18, by = 1)) +
  geom_point() +
  geom_errorbar(aes(ymin = avg_ps1 - sd_ps1, ymax = avg_ps1 + sd_ps1), width = 0.2) +
  labs(title = "Averaged current knee pain (0-10) by PT sessions",
       x = "PT Sessions",
       y = "Averaged current knee pain (0-10)")+ 
  theme_hc(base_size = 14)
ps1_plot

# Export
write.csv(avg_pt, file = file.path(outputDir, "/avg_pt.csv"), row.names = FALSE)
ggsave(file.path(outputDir,"avg_pt_kosa_plot.png"), plot = kosa_plot, width = 16, height = 6, dpi = 300)
ggsave(file.path(outputDir,"avg_pt_kosg_plot.png"), plot = kosg_plot, width = 16, height = 6, dpi = 300)
ggsave(file.path(outputDir,"avg_pt_ps1_plot.png"), plot = ps1_plot, width = 16, height = 6, dpi = 300)
