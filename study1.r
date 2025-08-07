library(dplyr)
library(readr)

file_path <- "C:/Users/aasmi/Downloads/gt9x_sleep_dsis_koos_pgaoa_weekly (1).csv"

raw <- read_csv(file_path, show_col_types = FALSE)

panel <- raw %>%
  select(id       = record_id,
         time     = redcap_event_name,   # weekly stamp
         sleep    = Efficiency_Mean,
         pain     = koos_pain) %>%
  mutate(across(c(sleep, pain), as.numeric)) %>%           # coerce to numeric
  arrange(id, time) %>%
  group_by(id) %>%
  mutate(week     = row_number() - 1,                      # 0,1,2…
         sleep_c  = sleep - mean(sleep, na.rm = TRUE),
         pain_c   = pain  - mean(pain,  na.rm = TRUE)) %>%
  ungroup()

print(head(panel, 10))   # inspect first 10 cleaned rows

cat("\nMissing after coercion → sleep:",
    sum(is.na(panel$sleep)), " | pain:", sum(is.na(panel$pain)), "\n")

# ===========================================================
# 2. collapse duplicate person-weeks and rebuild centred series
# ===========================================================
panel_clean <- panel %>%
  group_by(id, week) %>%                               # average any duplicates
  summarise(
    sleep = mean(sleep, na.rm = TRUE),
    pain  = mean(pain,  na.rm = TRUE),
    .groups = "drop") %>%
  group_by(id) %>%
  mutate(
    sleep_c = sleep - mean(sleep, na.rm = TRUE),
    pain_c  = pain  - mean(pain,  na.rm = TRUE)
  ) %>%
  ungroup()

cat("Rows after averaging duplicates:", nrow(panel_clean), "\n")
cat("Remaining missing values → sleep:", sum(is.na(panel_clean$sleep)),
    "| pain:", sum(is.na(panel_clean$pain)), "\n\n")

# ===========================================================
# 3. build and fit the continuous-time DSEM with numeric starts
# ===========================================================
library(ctsem)

ctmodel <- ctModel(
  type          = "stanct",
  manifestNames = c("sleep_c", "pain_c"),
  latentNames   = c("sleep",   "pain"),
  LAMBDA        = diag(2),
  DRIFT         = matrix(c("ar_s","cl_ps",
                           "cl_sp","ar_p"), 2, 2, byrow = TRUE),
  CINT          = matrix(c("trend_s","trend_p"), 2, 1),
  DIFFUSION     = diag(c(0.1, 0.1)),     # numeric starting values
  MANIFESTVAR   = diag(c(1.0, 1.0)),     # numeric starting values
  T0MEANS       = matrix(c(0, 0), 2, 1), # numeric starting values
  T0VAR         = diag(c(1.0, 1.0))      # numeric starting values
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
# 4. extract the six dynamic paths and weekly trends
# ===========================================================
full     <- summary(fit, digits = 3)
paramtab <- full$parmatrices                 # <-- correct table
wanted   <- c("ar_s","ar_p","cl_ps","cl_sp","trend_s","trend_p")

name_col <- intersect(c("param", "label", "name"), colnames(paramtab))[1]
if (is.na(name_col)) stop("No parameter label column found.")
dynamic  <- paramtab[paramtab[[name_col]] %in% wanted, ]
print(dynamic, row.names = FALSE)
