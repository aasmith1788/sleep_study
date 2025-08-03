## =====================================================================
##  RI-CLPMs for Weekly Sleep Metrics and KOOS Pain (3 Waves - Yamada Approach)
##  Using weeks 0, 9, 18 to match Yamada et al. 2022
## =====================================================================

## --------------------------- 0. Packages ------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(lavaan)
})

## --------------------------- 1. User Options --------------------------
file_path      <- "C:/Users/aasmi/Downloads/gt9x_sleep_dsis_koos_pgaoa_weekly.csv"
sleep_metrics  <- c("efficiency_mean",
                    "tst_mean",
                    "waso_mean",
                    "number_of_awakenings_mean",
                    "sleep_fragmentation_index_mean")
pain_var       <- "koos_pain"
keep_weeks     <- c(0, 9, 18)  # Changed to 3 strategic timepoints
n_waves        <- 3            # Changed from 6 to 3
standardize_xy <- TRUE
max_iter       <- 10000

## --------------------------- 2. Load & Basic Clean --------------------
raw <- read_csv(file_path,
                na = c("", "NA", ".")) %>%   
  clean_names()

raw <- raw %>%
  mutate(week = as.integer(redcap_event_name)) %>%
  arrange(record_id, week)

dat <- raw %>%
  group_by(record_id, week) %>%
  mutate(.occ = row_number()) %>%
  filter(.occ == 2 | (n() == 1 & .occ == 1)) %>%
  select(-.occ) %>%
  ungroup()

## --------------------------- 3. Helper Functions ----------------------

reshape_wide <- function(df, vars, n_waves) {
  week_map <- tibble(week = keep_weeks,
                     wave = paste0("w", 1:n_waves))
  df %>%
    filter(week %in% keep_weeks) %>%
    select(record_id, week, all_of(vars)) %>%
    inner_join(week_map, by = "week") %>%
    pivot_wider(id_cols = record_id,
                names_from = wave,
                values_from = all_of(vars),
                names_sep = "_")
}

obs_names <- function(base, n) paste0(base, "_w", 1:n)

build_ri_clpm_yamada <- function(x_obs, y_obs, stationary = TRUE) {
  n <- length(x_obs)
  syn <- ""
  syn <- paste0(
    syn,
    "RI_X =~ ", paste0("1*", x_obs, collapse=" + "), "\n",
    "RI_Y =~ ", paste0("1*", y_obs, collapse=" + "), "\n",
    "RI_X ~~ RI_X\nRI_Y ~~ RI_Y\nRI_X ~~ RI_Y\n",
    "RI_X ~ 1\nRI_Y ~ 1\n"
  )
  for (i in seq_len(n)) {
    syn <- paste0(
      syn,
      "wX", i, " =~ 1*", x_obs[i], "\n",
      "wY", i, " =~ 1*", y_obs[i], "\n",
      "RI_X ~~ 0*wX", i, "\n",
      "RI_Y ~~ 0*wY", i, "\n",
      "wX", i, " ~ 0*1\n",
      "wY", i, " ~ 0*1\n",
      x_obs[i], " ~~ 0*", x_obs[i], "\n",
      y_obs[i], " ~~ 0*", y_obs[i], "\n"
    )
  }
  syn <- paste0(syn, "wX1 ~~ wX1\nwY1 ~~ wY1\nwX1 ~~ wY1\n")
  
  # With only 3 waves, we have just 2 cross-lagged transitions
  if (stationary) {
    for (i in 2:n) {
      p <- i - 1
      syn <- paste0(
        syn,
        "wX", i, " ~ a*wX", p, " + b*wY", p, "\n",
        "wY", i, " ~ c*wY", p, " + d*wX", p, "\n",
        "wX", i, " ~~ cxy*wY", i, "\n",
        "wX", i, " ~~ wX", i, "\n",
        "wY", i, " ~~ wY", i, "\n"
      )
    }
  } else {
    for (i in 2:n) {
      p <- i - 1
      syn <- paste0(
        syn,
        "wX", i, " ~ a", i, "*wX", p, " + b", i, "*wY", p, "\n",
        "wY", i, " ~ c", i, "*wY", p, " + d", i, "*wX", p, "\n",
        "wX", i, " ~~ cxy", i, "*wY", i, "\n",
        "wX", i, " ~~ wX", i, "\n",
        "wY", i, " ~~ wY", i, "\n"
      )
    }
  }
  syn
}

build_ri_clpm_relaxed <- function(x_obs, y_obs) {
  n <- length(x_obs)
  syn <- ""
  syn <- paste0(
    syn,
    "RI_X =~ ", paste0("1*", x_obs, collapse=" + "), "\n",
    "RI_Y =~ ", paste0("1*", y_obs, collapse=" + "), "\n",
    "RI_X ~~ RI_X\nRI_Y ~~ 0*RI_Y\nRI_X ~~ RI_Y\n",
    "RI_X ~ 1\nRI_Y ~ 1\n"
  )
  for (i in seq_len(n)) {
    syn <- paste0(
      syn,
      "wX", i, " =~ 1*", x_obs[i], "\n",
      "wY", i, " =~ 1*", y_obs[i], "\n",
      "RI_X ~~ 0*wX", i, "\n",
      "RI_Y ~~ 0*wY", i, "\n",
      "wX", i, " ~ 0*1\n",
      "wY", i, " ~ 0*1\n",
      x_obs[i], " ~~ 0*", x_obs[i], "\n",
      y_obs[i], " ~~ 0*", y_obs[i], "\n"
    )
  }
  syn <- paste0(
    syn,
    "wX1 ~~ vx*wX1\n",
    "wX2 ~~ vx*wX2\n",
    "wX3 ~~ vx*wX3\n",
    "wY1 ~~ vy*wY1\n",
    "wY2 ~~ vy*wY2\n",
    "wY3 ~~ vy*wY3\n",
    "wX1 ~~ cxy1*wY1\n"
  )
  syn <- paste0(
    syn,
    "wX2 ~ a2*wX1 + b2*wY1\n",
    "wY2 ~ c2*wY1 + d2*wX1\n",
    "wX3 ~ a3*wX2 + b3*wY2\n",
    "wY3 ~ c3*wY2 + d3*wX2\n"
  )
  syn
}

fit_one_pair <- function(sleep_var, pain_var, dat_long, n_waves,
                         standardize_xy = TRUE,
                         max_iter = 10000) {
  
  df_pair <- dat_long %>%
    filter(week %in% keep_weeks) %>%
    select(record_id, week, all_of(c(sleep_var, pain_var)))
  
  if (!is.numeric(df_pair[[pain_var]]))
    df_pair[[pain_var]] <- suppressWarnings(as.numeric(df_pair[[pain_var]]))
  
  if (!is.numeric(df_pair[[sleep_var]]))
    stop(paste("Sleep variable", sleep_var, "is not numeric after cleaning."))
  
  if (standardize_xy) {
    df_pair <- df_pair %>%
      mutate(
        !!sleep_var := as.numeric(scale(.data[[sleep_var]])),
        !!pain_var  := as.numeric(scale(.data[[pain_var]]))
      )
  }
  
  dat_wide <- reshape_wide(df_pair, c(sleep_var, pain_var), n_waves)
  
  x_obs <- obs_names(sleep_var, n_waves)
  y_obs <- obs_names(pain_var,  n_waves)
  
  syntax_relaxed <- build_ri_clpm_relaxed(x_obs, y_obs)
  ctrl <- list(iter.max = max_iter)

  fit_stat <- sem(model = syntax_relaxed,
                  data = dat_wide,
                  missing = "fiml",
                  estimator = "MLR",
                  control = ctrl)
  if (!lavInspect(fit_stat, "converged"))
    stop(paste("Model failed to converge for", sleep_var))

  pe <- parameterEstimates(fit_stat, standardized = TRUE)

  grab_param <- function(lab) {
    idx <- which(pe$label == lab)
    if (length(idx) == 0) return(tibble(param = lab, est = NA_real_, se = NA_real_, p = NA_real_))
    tibble(param = lab,
           est = pe$std.all[idx][1],
           se = pe$se[idx][1],
           p = pe$pvalue[idx][1])
  }
  grabs <- map_dfr(c("a2","b2","c2","d2","a3","b3","c3","d3","vx","vy","cxy1"), grab_param)
  
  ri_cov <- pe %>%
    filter(lhs == "RI_X", rhs == "RI_Y", op == "~~") %>%
    transmute(ri_cov_est = est, ri_cov_se = se, ri_cov_p = pvalue, ri_cov_std = std.all)
  
  summary_row <- grabs %>%
    pivot_wider(names_from = param,
                values_from = c(est,se,p),
                names_sep = "_") %>%
    mutate(
      sleep_metric = sleep_var,
      ri_cov_est   = ri_cov$ri_cov_est,
      ri_cov_se    = ri_cov$ri_cov_se,
      ri_cov_p     = ri_cov$ri_cov_p,
      ri_cov_std   = ri_cov$ri_cov_std
    ) %>%
    relocate(sleep_metric)
  
  # Calculate ICC for the sleep variable (like Yamada did for fatigue)
  var_ri_x <- pe %>% filter(lhs == "RI_X", rhs == "RI_X", op == "~~") %>% pull(est)
  var_total_x <- var(df_pair[[sleep_var]], na.rm = TRUE)
  icc_x <- var_ri_x / var_total_x
  
  summary_row <- summary_row %>% mutate(icc_sleep = icc_x)

  list(fit = fit_stat,
       summary = summary_row)
}

## --------------------------- 4. Run Models -----------------------------

all_results <- vector("list", length(sleep_metrics))
names(all_results) <- sleep_metrics

for (sv in sleep_metrics) {
  message("\n=== Fitting RI-CLPM for ", sv, " and ", pain_var, " ===")
  all_results[[sv]] <- fit_one_pair(
    sleep_var      = sv,
    pain_var       = pain_var,
    dat_long       = dat,
    n_waves        = n_waves,
    standardize_xy = standardize_xy,
    max_iter       = max_iter
  )
}

ri_results_summary <- bind_rows(lapply(all_results, `[[`, "summary"))

## --------------------------- 5. Output Summary ------------------------

print(ri_results_summary)

# Quick readable interpretation lines (matching Yamada's presentation)
interpret <- ri_results_summary %>%
  mutate(
    a2_sig = ifelse(p_a2 < .05, "YES", "no"),
    b2_sig = ifelse(p_b2 < .05, "YES", "no"),
    c2_sig = ifelse(p_c2 < .05, "YES", "no"),
    d2_sig = ifelse(p_d2 < .05, "YES", "no"),
    a3_sig = ifelse(p_a3 < .05, "YES", "no"),
    b3_sig = ifelse(p_b3 < .05, "YES", "no"),
    c3_sig = ifelse(p_c3 < .05, "YES", "no"),
    d3_sig = ifelse(p_d3 < .05, "YES", "no")
  ) %>%
  select(sleep_metric,
         est_a2, p_a2, a2_sig,
         est_b2, p_b2, b2_sig,
         est_c2, p_c2, c2_sig,
         est_d2, p_d2, d2_sig,
         est_a3, p_a3, a3_sig,
         est_b3, p_b3, b3_sig,
         est_c3, p_c3, c3_sig,
         est_d3, p_d3, d3_sig,
         ri_cov_std, icc_sleep)

cat("\n=== Cross-Lag Significance Overview (3-wave analysis like Yamada) ===\n")
print(interpret)

save(all_results, ri_results_summary, file = "ri_clpm_sleep_koos_pain_3waves_yamada.RData")
cat("\nSaved: ri_clpm_sleep_koos_pain_3waves_yamada.RData\n")

## --------------------------- 6. Fit Indices Summary (WITH ERROR HANDLING) ------------------------

fit_stats <- c("chisq","df","pvalue","cfi","tli","rmsea","srmr","aic","bic")

pull_fit <- function(fit_obj) {
  fm <- try(fitMeasures(fit_obj, fit_stats), silent = TRUE)
  if (inherits(fm, "try-error")) {
    # Return NAs if fit measures fail
    return(tibble(
      chisq  = NA_real_,
      df     = NA_real_,
      pvalue = NA_real_,
      cfi    = NA_real_,
      tli    = NA_real_,
      rmsea  = NA_real_,
      srmr   = NA_real_,
      aic    = NA_real_,
      bic    = NA_real_
    ))
  }
  tibble(
    chisq  = unname(fm["chisq"]),
    df     = unname(fm["df"]),
    pvalue = unname(fm["pvalue"]),
    cfi    = unname(fm["cfi"]),
    tli    = unname(fm["tli"]),
    rmsea  = unname(fm["rmsea"]),
    srmr   = unname(fm["srmr"]),
    aic    = unname(fm["aic"]),
    bic    = unname(fm["bic"])
  )
}

# Build fit indices table with error handling
fit_indices_list <- list()
for (i in seq_along(all_results)) {
  metric_name <- names(all_results)[i]
  cat("Processing fit indices for:", metric_name, "\n")
  
  try({
    fi <- pull_fit(all_results[[i]]$fit)
    N <- lavInspect(all_results[[i]]$fit, "nobs")
    fit_indices_list[[metric_name]] <- fi %>%
      mutate(
        sleep_metric = metric_name,
        N = N
      ) %>%
      relocate(sleep_metric, N)
  }, silent = FALSE)
}

fit_indices_summary <- bind_rows(fit_indices_list)

# Continue with the rest...
full_summary <- ri_results_summary %>%
  left_join(fit_indices_summary, by = "sleep_metric") %>%
  relocate(sleep_metric, N,
           chisq, df, pvalue, cfi, tli, rmsea, srmr, aic, bic)

cat("\n=== MODEL FIT INDICES (3-wave RI-CLPM per sleep metric) ===\n")
print(fit_indices_summary)

cat("\n=== FULL SUMMARY (fit + dynamic paths) ===\n")
print(full_summary)

# Check which models had issues
cat("\n=== MODEL CONVERGENCE CHECK ===\n")
for (sv in sleep_metrics) {
  converged <- lavInspect(all_results[[sv]]$fit, "converged")
  cat(sv, "- Converged:", converged, "\n")
}

# Compare to Yamada's results
cat("\n=== COMPARISON TO YAMADA ET AL. 2022 ===\n")
cat("Yamada found:\n")
cat("- Pain → Fatigue: β = 0.55 (p = 0.02) and β = 0.36 (p = 0.001)\n")
cat("- Fatigue → Pain: Not significant\n")
cat("- Model fit: CFI = 1.00, TLI = 1.00, RMSEA < 0.001\n")
cat("- ICC for fatigue = 0.53\n")

save(full_summary, fit_indices_summary,
     file = "ri_clpm_sleep_koos_pain_3waves_fit_and_params.RData")
cat("\nSaved: ri_clpm_sleep_koos_pain_3waves_fit_and_params.RData\n")
