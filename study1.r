library(dplyr)
library(readr)
library(ctsem)

set.seed(123)

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
    pain  = as.numeric(pain_raw)            # keep KOOS Pain as-is (higher = less pain)
  ) %>%
  arrange(id, time) %>%
  group_by(id) %>%
  mutate(
    week     = row_number() - 1,
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
# 3. Keep only participants with at least one stretch of 3 consecutive valid weeks
# ===========================================================

# Flag valid rows (non-missing)
panel_valid <- panel_clean %>%
  filter(!is.na(sleep) & !is.na(pain))

# Identify valid 3-week stretches
valid_ids <- panel_valid %>%
  arrange(id, week) %>%
  group_by(id) %>%
  mutate(
    lead1 = lead(week, 1),
    lead2 = lead(week, 2),
    is_consecutive = (lead1 == week + 1) & (lead2 == week + 2)
  ) %>%
  summarise(has_consec3 = any(is_consecutive, na.rm = TRUE), .groups = "drop") %>%
  filter(has_consec3) %>%
  pull(id)

# Filter main dataset
panel_clean <- panel_clean %>%
  filter(id %in% valid_ids)

cat("Remaining participants with ≥3 consecutive valid weeks:", length(unique(panel_clean$id)), "\n")
cat("Remaining rows after filtering for consecutive weeks:", nrow(panel_clean), "\n")

# ===========================================================
# 4. Remove outliers (z > 3 or z < -3 in sleep or pain)
# ===========================================================
panel_clean <- panel_clean %>%
  group_by(id) %>%
  mutate(
    z_sleep = (sleep - mean(sleep, na.rm = TRUE)) / sd(sleep, na.rm = TRUE),
    z_pain  = (pain  - mean(pain,  na.rm = TRUE)) / sd(pain,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(abs(z_sleep) <= 3, abs(z_pain) <= 3) %>%
  select(-z_sleep, -z_pain)

cat("Rows after outlier removal:", nrow(panel_clean), "\n\n")

# ===========================================================
# 5. Build and fit the continuous-time model
# ===========================================================
ctmodel <- ctModel(
  type          = "stanct",
  manifestNames = c("sleep_c", "pain_c"),
  latentNames   = c("sleep", "pain"),
  LAMBDA        = diag(2),
  DRIFT         = matrix(c("ar_s","cl_ps",
                           "cl_sp","ar_p"), 2, 2, byrow = TRUE),
  CINT          = matrix(c("trend_s","trend_p"), 2, 1),
  DIFFUSION     = diag(c("diff_sleep", "diff_pain")),
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

 diag_raw <- rstan::summary(fit$stanfit)$summary
diag_table <- as.data.frame(diag_raw)[, c("Rhat", "n_eff")]
diag_table$param_name <- rownames(diag_raw)
diag_table <- diag_table[, c("param_name", "Rhat", "n_eff")]

 cat("=== PARAMETER DIAGNOSTICS ===\n")
 print(diag_table, row.names = FALSE)

 if (any(diag_table$Rhat > 1.01)) {
   stop("R-hat exceeds 1.01 for some parameters")
 }
 if (any(diag_table$n_eff < 400)) {
   stop("Effective sample size below 400 for some parameters")
 }

 paramtab <- summary(fit, digits = 3)$parmatrices

# ===========================================================
# 6. Extract and label dynamic parameters
# ===========================================================
dynamic <- paramtab %>%
  filter(matrix %in% c("DRIFT", "CINT")) %>%
  mutate(param_label = case_when(
    matrix == "DRIFT" & row == 1 & col == 1 ~ "ar_s",
    matrix == "DRIFT" & row == 1 & col == 2 ~ "cl_ps",
    matrix == "DRIFT" & row == 2 & col == 1 ~ "cl_sp",
    matrix == "DRIFT" & row == 2 & col == 2 ~ "ar_p",
    matrix == "CINT"  & row == 1            ~ "trend_s",
    matrix == "CINT"  & row == 2            ~ "trend_p",
    TRUE ~ NA_character_
  )) %>%
  select(param_label, everything()) %>%
  filter(!is.na(param_label))

cat("=== DYNAMIC PATH ESTIMATES ===\n")
print(dynamic, row.names = FALSE)

# ===========================================================
# 7. Posterior Predictive Check
# ===========================================================
cat("\nRunning posterior predictive checks...\n")
ppc <- ctStanPostPredict(fit)
ctPlot(ppc, subjects = 1:4)

# ===========================================================
# 8. Debug: Participant and Time Coverage
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
cat("Weeks used:", paste(used_weeks, collapse = ", "), "\n")

cat("\nSession information:\n")
print(sessionInfo())


