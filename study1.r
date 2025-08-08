library(dplyr)
library(readr)
library(ctsem)
library(purrr)

# ===========================================================
# CONFIGURATION
# ===========================================================
MIN_SHORT_GAP_PAIRS <- 5  # Minimum number of short-gap pairs required per participant

# ===========================================================
# 1. Load and prepare data
# ===========================================================
file_path <- "C:/Users/aasmi/Downloads/gt9x_sleep_dsis_koos_pgaoa_weekly (1).csv"

raw <- read_csv(file_path, show_col_types = FALSE)

panel <- raw %>%
  select(id       = record_id,
         time     = redcap_event_name,      # weekly stamp
         sleep    = Efficiency_Mean,
         pain_raw = koos_pain) %>%
  mutate(
    sleep = as.numeric(sleep),
    pain  = as.numeric(pain_raw),            # keep KOOS Pain as-is (higher = less pain)
    week  = as.integer(time)             # CHANGED: use actual week numbers
  ) %>%
  filter(!is.na(week)) %>%                   # CHANGED: drop rows with missing week
  arrange(id, week) %>%                      # CHANGED: sort by actual week
  group_by(id) %>%
  mutate(
    sleep_c  = sleep - mean(sleep, na.rm = TRUE),
    pain_c   = pain  - mean(pain,  na.rm = TRUE)
  ) %>%
  ungroup()

cat("\nMissing after coercion → sleep:",
    sum(is.na(panel$sleep)), " | pain:", sum(is.na(panel$pain)), "\n")

# ===========================================================
# 2. Collapse duplicate person-weeks and rebuild centred series
# ===========================================================
panel_clean <- panel %>%
  group_by(id, week) %>%
  summarise(
    sleep = mean(sleep, na.rm = TRUE),
    pain  = mean(pain,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(id) %>%
  mutate(
    sleep_c = sleep - mean(sleep, na.rm = TRUE),
    pain_c  = pain  - mean(pain,  na.rm = TRUE)
  ) %>%
  ungroup()

cat("Rows after averaging duplicates:", nrow(panel_clean), "\n")
cat("Remaining missing values → sleep:",
    sum(is.na(panel_clean$sleep)), "| pain:", sum(is.na(panel_clean$pain)), "\n\n")

# ===========================================================
# 2.5. STANDARDIZE MANIFEST VARIABLES ACROSS POOLED SAMPLE
# ===========================================================
cat("=== STANDARDIZING MANIFEST VARIABLES ===\n")

# Calculate pooled sample statistics for person-centered variables
sleep_c_pooled_mean <- mean(panel_clean$sleep_c, na.rm = TRUE)
sleep_c_pooled_sd <- sd(panel_clean$sleep_c, na.rm = TRUE)
pain_c_pooled_mean <- mean(panel_clean$pain_c, na.rm = TRUE)
pain_c_pooled_sd <- sd(panel_clean$pain_c, na.rm = TRUE)

cat("Pre-standardization pooled statistics:\n")
cat("  sleep_c: M =", round(sleep_c_pooled_mean, 4), ", SD =", round(sleep_c_pooled_sd, 4), "\n")
cat("  pain_c:  M =", round(pain_c_pooled_mean, 4), ", SD =", round(pain_c_pooled_sd, 4), "\n")

# Standardize to pooled sample mean=0, SD=1
panel_clean <- panel_clean %>%
  mutate(
    sleep_c = (sleep_c - sleep_c_pooled_mean) / sleep_c_pooled_sd,
    pain_c  = (pain_c  - pain_c_pooled_mean)  / pain_c_pooled_sd
  )

# Verify standardization
cat("\nPost-standardization pooled statistics:\n")
cat("  sleep_c: M =", round(mean(panel_clean$sleep_c, na.rm = TRUE), 4), 
    ", SD =", round(sd(panel_clean$sleep_c, na.rm = TRUE), 4), "\n")
cat("  pain_c:  M =", round(mean(panel_clean$pain_c, na.rm = TRUE), 4), 
    ", SD =", round(sd(panel_clean$pain_c, na.rm = TRUE), 4), "\n\n")

# ===========================================================
# 3. Keep only participants with sufficient short-gap week pairs
# ===========================================================

# Filter out rows with missing data first
panel_valid <- panel_clean %>%
  filter(!is.na(sleep) & !is.na(pain))

# Count short-gap pairs (1-2 week differences) for each participant
short_gap_counts <- panel_valid %>%
  arrange(id, week) %>%
  group_by(id) %>%
  summarise(
    week_diffs = list(diff(week)),
    .groups = "drop"
  ) %>%
  mutate(
    short_gap_pairs = map_int(week_diffs, ~ sum(.x %in% c(1, 2)))
  ) %>%
  select(id, short_gap_pairs)

cat("=== SHORT-GAP PAIR ANALYSIS ===\n")
cat("Short-gap pairs per participant (1-2 week differences):\n")
print(summary(short_gap_counts$short_gap_pairs))
cat("\nDistribution of short-gap pairs:\n")
print(table(short_gap_counts$short_gap_pairs))

# Identify participants meeting the threshold
valid_ids <- short_gap_counts %>%
  filter(short_gap_pairs >= MIN_SHORT_GAP_PAIRS) %>%
  pull(id)

cat("\nParticipants with ≥", MIN_SHORT_GAP_PAIRS, "short-gap pairs:", length(valid_ids), "\n")
cat("Participants excluded:", length(unique(panel_valid$id)) - length(valid_ids), "\n")

# Filter main dataset to include all weeks for qualifying participants
panel_clean <- panel_clean %>%
  filter(id %in% valid_ids)

cat("Remaining rows after filtering for short-gap pairs:", nrow(panel_clean), "\n")

# Now remove rows with missing data
panel_clean <- panel_clean %>%
  filter(!is.na(sleep) & !is.na(pain))

cat("Final rows after removing missing data:", nrow(panel_clean), "\n\n")

# Store count before
n_before <- nrow(panel_clean)

# Robust outlier detection
panel_clean <- panel_clean %>%
  group_by(id) %>%
  filter(n() >= 3) %>%
  mutate(
    sleep_trend = predict(lm(sleep ~ week, na.action = na.exclude)),
    pain_trend = predict(lm(pain ~ week, na.action = na.exclude)),
    sleep_resid = sleep - sleep_trend,
    pain_resid = pain - pain_trend,
    
    # Use median ± 3*MAD instead of mean ± 3*SD
    sleep_outlier = abs(sleep_resid - median(sleep_resid, na.rm = TRUE)) > 3 * mad(sleep_resid, na.rm = TRUE),
    pain_outlier = abs(pain_resid - median(pain_resid, na.rm = TRUE)) > 3 * mad(pain_resid, na.rm = TRUE)
  ) %>%
  filter(!sleep_outlier, !pain_outlier) %>%
  select(-contains("trend"), -contains("resid"), -contains("outlier")) %>%
  ungroup()

# Report removal
cat("Rows removed:", n_before - nrow(panel_clean), "\n")
# ===========================================================
# 5. SANITY CHECK: Print within-ID week differences
# ===========================================================
cat("=== SANITY CHECK: Within-ID week differences ===\n")
week_diffs <- panel_clean %>%
  arrange(id, week) %>%
  group_by(id) %>%
  summarise(
    week_diffs = list(diff(week)),
    .groups = "drop"
  ) %>%
  pull(week_diffs) %>%
  unlist()

cat("Week differences summary:\n")
print(table(week_diffs))
cat("Most common difference:", names(sort(table(week_diffs), decreasing = TRUE))[1], "\n")
cat("Range of differences:", min(week_diffs), "to", max(week_diffs), "\n\n")

# ===========================================================
# 6. Build and fit the continuous-time model
# ===========================================================
ctmodel <- ctModel(
  type          = "stanct",
  manifestNames = c("sleep_c", "pain_c"),
  latentNames   = c("sleep", "pain"),
  LAMBDA        = diag(2),
  DRIFT         = matrix(c("ar_s","cl_ps",
                           "cl_sp","ar_p"), 2, 2, byrow = TRUE),
  CINT          = matrix(c("trend_s","trend_p"), 2, 1),
  DIFFUSION     = diag(c(0.1, 0.1)),
  MANIFESTVAR   = diag(c(1.0, 1.0)),
  T0MEANS       = matrix(c(0, 0), 2, 1),
  T0VAR         = diag(c(1.0, 1.0))
)

fit <- ctStanFit(
  datalong    = panel_clean %>% select(id, time = week, sleep_c, pain_c),
  ctstanmodel = ctmodel,
  chains      = 4,
  cores       = 4,
  iter        = 4000,
  control     = list(adapt_delta = 0.95),
  verbose     = FALSE
)

# ===========================================================
# 7. Extract and label dynamic parameters
# ===========================================================
full     <- summary(fit, digits = 3)
paramtab <- full$parmatrices

dynamic <- paramtab %>%
  filter(matrix %in% c("DRIFT", "CINT")) %>%
  mutate(param_label = case_when(
    matrix == "DRIFT" & row == 1 & col == 1 ~ "ar_s (sleep autoregression per week)",
    matrix == "DRIFT" & row == 1 & col == 2 ~ "cl_ps (pain→sleep cross-lag per week)",
    matrix == "DRIFT" & row == 2 & col == 1 ~ "cl_sp (sleep→pain cross-lag per week)",
    matrix == "DRIFT" & row == 2 & col == 2 ~ "ar_p (pain autoregression per week)",
    matrix == "CINT"  & row == 1            ~ "trend_s (sleep trend per week)",
    matrix == "CINT"  & row == 2            ~ "trend_p (pain trend per week)",
    TRUE ~ NA_character_
  )) %>%
  select(param_label, everything()) %>%
  filter(!is.na(param_label))

cat("=== DYNAMIC PATH ESTIMATES (PER WEEK) ===\n")
print(dynamic, row.names = FALSE)

# ===========================================================
# 8. Posterior Predictive Check
# ===========================================================
cat("\nRunning posterior predictive checks...\n")
ppc <- ctStanPostPredict(fit)
ctPlot(ppc, subjects = 1:4)

# ===========================================================
# 9. Debug: Participant and Time Coverage
# ===========================================================
cat("\n=== DEBUG SUMMARY ===\n")
n_participants <- panel_clean %>% pull(id) %>% unique() %>% length()
cat("Number of participants used:", n_participants, "\n")

week_counts <- panel_clean %>%
  group_by(id) %>%
  summarise(n_weeks = n(), .groups = "drop")

cat("Weeks per participant (min / median / max):",
    min(week_counts$n_weeks), "/", median(week_counts$n_weeks), "/", max(week_counts$n_weeks), "\n")

model_input <- panel_clean %>% select(id, time = week, sleep_c, pain_c)
na_counts <- colSums(is.na(model_input))
cat("Missing values in model input:\n")
print(na_counts)

used_weeks <- panel_clean %>% pull(week) %>% unique() %>% sort()
cat("Total unique weeks modeled:", length(used_weeks), "\n")
cat("Week range:", min(used_weeks), "to", max(used_weeks), "\n")



install.packages("moments")
library(moments)
# ===========================================================
# DEBUG: RESIDUAL NORMALITY CHECK
# ===========================================================

cat("\n=== RESIDUAL NORMALITY DIAGNOSTICS ===\n")

# Recreate residuals for final dataset
residual_data <- panel_clean %>%
  group_by(id) %>%
  filter(n() >= 3) %>%
  mutate(
    sleep_trend = predict(lm(sleep ~ week, na.action = na.exclude)),
    pain_trend = predict(lm(pain ~ week, na.action = na.exclude)),
    sleep_resid = sleep - sleep_trend,
    pain_resid = pain - pain_trend
  ) %>%
  ungroup() %>%
  filter(!is.na(sleep_resid), !is.na(pain_resid))

# Basic descriptive statistics
cat("SLEEP RESIDUALS:\n")
cat("  Mean:", round(mean(residual_data$sleep_resid), 4), "\n")
cat("  SD:", round(sd(residual_data$sleep_resid), 4), "\n")
cat("  Median:", round(median(residual_data$sleep_resid), 4), "\n")
cat("  MAD:", round(mad(residual_data$sleep_resid), 4), "\n")
cat("  Skewness:", round(moments::skewness(residual_data$sleep_resid), 3), "\n")
cat("  Kurtosis:", round(moments::kurtosis(residual_data$sleep_resid), 3), "\n")

cat("\nPAIN RESIDUALS:\n")
cat("  Mean:", round(mean(residual_data$pain_resid), 4), "\n")
cat("  SD:", round(sd(residual_data$pain_resid), 4), "\n")
cat("  Median:", round(median(residual_data$pain_resid), 4), "\n")
cat("  MAD:", round(mad(residual_data$pain_resid), 4), "\n")
cat("  Skewness:", round(moments::skewness(residual_data$pain_resid), 3), "\n")
cat("  Kurtosis:", round(moments::kurtosis(residual_data$pain_resid), 3), "\n")

# Normality tests (if you have moments package)
if(requireNamespace("moments", quietly = TRUE)) {
  # Jarque-Bera test (good for larger samples)
  jb_sleep <- moments::jarque.test(residual_data$sleep_resid)
  jb_pain <- moments::jarque.test(residual_data$pain_resid)
  
  cat("\nJARQUE-BERA NORMALITY TESTS:\n")
  cat("  Sleep residuals: p =", round(jb_sleep$p.value, 4))
  if(jb_sleep$p.value < 0.05) cat(" (NON-NORMAL)") else cat(" (Normal)")
  cat("\n")
  
  cat("  Pain residuals: p =", round(jb_pain$p.value, 4))
  if(jb_pain$p.value < 0.05) cat(" (NON-NORMAL)") else cat(" (Normal)")
  cat("\n")
} else {
  cat("\nInstall 'moments' package for additional normality tests\n")
}

# Shapiro-Wilk test (if sample size < 5000)
if(nrow(residual_data) <= 5000) {
  sw_sleep <- shapiro.test(residual_data$sleep_resid)
  sw_pain <- shapiro.test(residual_data$pain_resid)
  
  cat("\nSHAPIRO-WILK NORMALITY TESTS:\n")
  cat("  Sleep residuals: p =", round(sw_sleep$p.value, 4))
  if(sw_sleep$p.value < 0.05) cat(" (NON-NORMAL)") else cat(" (Normal)")
  cat("\n")
  
  cat("  Pain residuals: p =", round(sw_pain$p.value, 4))
  if(sw_pain$p.value < 0.05) cat(" (NON-NORMAL)") else cat(" (Normal)")
  cat("\n")
} else {
  cat("\nSample too large for Shapiro-Wilk test (n > 5000)\n")
}

# Visual inspection guidance
cat("\n=== VISUAL INSPECTION PLOTS ===\n")
cat("Run these commands to visually check normality:\n\n")

cat("# Sleep residual plots:\n")
cat("par(mfrow=c(2,2))\n")
cat("hist(residual_data$sleep_resid, main='Sleep Residuals', breaks=30)\n")
cat("qqnorm(residual_data$sleep_resid, main='Sleep Q-Q Plot')\n")
cat("qqline(residual_data$sleep_resid)\n")
cat("plot(residual_data$week, residual_data$sleep_resid, main='Sleep Residuals vs Week')\n")
cat("plot(residual_data$sleep_trend, residual_data$sleep_resid, main='Sleep Residuals vs Fitted')\n\n")

cat("# Pain residual plots:\n")
cat("par(mfrow=c(2,2))\n")
cat("hist(residual_data$pain_resid, main='Pain Residuals', breaks=30)\n")
cat("qqnorm(residual_data$pain_resid, main='Pain Q-Q Plot')\n")
cat("qqline(residual_data$pain_resid)\n")
cat("plot(residual_data$week, residual_data$pain_resid, main='Pain Residuals vs Week')\n")
cat("plot(residual_data$pain_trend, residual_data$pain_resid, main='Pain Residuals vs Fitted')\n\n")

# Interpretation guide
cat("=== INTERPRETATION GUIDE ===\n")
cat("NORMALITY INDICATORS:\n")
cat("  • Skewness close to 0 (±0.5 is reasonable)\n")
cat("  • Kurtosis close to 3 (2-4 is reasonable)\n")
cat("  • p-value > 0.05 in normality tests\n")
cat("  • Q-Q plot points follow straight line\n")
cat("  • Histogram looks bell-shaped\n\n")

cat("IF RESIDUALS ARE NON-NORMAL:\n")
cat("  • Consider robust outlier detection (median ± 3*MAD)\n")
cat("  • Use quantile-based thresholds (e.g., 1st/99th percentiles)\n")
cat("  • Check if linear trends are appropriate\n")
cat("  • Consider non-linear trend models\n\n")

# Clean up
rm(residual_data)


