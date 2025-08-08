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
standardize_xy <- FALSE
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

build_ri_clpm_yamada <- function(x_obs, y_obs, fix_pain_ri_variance = TRUE) {
  n <- length(x_obs)
  syn <- ""
  syn <- paste0(
    syn,
    "RI_X =~ ", paste0("1*", x_obs, collapse=" + "), "\n",
    "RI_Y =~ ", paste0("1*", y_obs, collapse=" + "), "\n",
    "RI_X ~~ RI_X\n",
    if (fix_pain_ri_variance) "RI_Y ~~ 0*RI_Y\n" else "RI_Y ~~ RI_Y\n",
    "RI_X ~~ RI_Y\n",
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
      y_obs[i], " ~~ 0*", y_obs[i], "\n",
      "wX", i, " ~~ wX", i, "\n",
      "wY", i, " ~~ wY", i, "\n",
      "wX", i, " ~~ cxy", i, "*wY", i, "\n"
    )
  }
  for (i in 2:n) {
    p <- i - 1
    syn <- paste0(
      syn,
      "wX", i, " ~ a", i, "*wX", p, " + b", i, "*wY", p, "\n",
      "wY", i, " ~ c", i, "*wY", p, " + d", i, "*wX", p, "\n"
    )
  }
  syn
}

fit_one_pair <- function(sleep_var, pain_var, dat_long, n_waves,
                         standardize_xy = FALSE,
                         fix_pain_ri_variance = TRUE,
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

  syntax <- build_ri_clpm_yamada(x_obs, y_obs,
                                 fix_pain_ri_variance = fix_pain_ri_variance)
  ctrl <- list(iter.max = max_iter)

  fit <- sem(model = syntax,
             data = dat_wide,
             missing = "fiml",
             estimator = "MLR",
             control = ctrl)

  fixed_pain_ri <- TRUE

  pe <- parameterEstimates(fit, standardized = TRUE)

  labels <- c("a2","b2","c2","d2","a3","b3","c3","d3",
              "cxy1","cxy2","cxy3")
  grabs <- tibble(
    param = labels,
    est   = sapply(labels, \(lab) pe$std.all[match(lab, pe$label)]),
    se    = sapply(labels, \(lab) pe$se[match(lab, pe$label)]),
    p     = sapply(labels, \(lab) pe$pvalue[match(lab, pe$label)])
  )
  
  ri_cov <- pe %>%
    filter(lhs == "RI_X", rhs == "RI_Y", op == "~~") %>%
    transmute(ri_cov_est = est, ri_cov_se = se, ri_cov_p = pvalue, ri_cov_std = std.all)
  
  summary_row <- grabs %>%
    pivot_wider(names_from = param,
                values_from = c(est,se,p),
                names_sep = "_") %>%
    mutate(
      sleep_metric  = sleep_var,
      fixed_pain_ri = fixed_pain_ri,
      ri_cov_est    = ri_cov$ri_cov_est,
      ri_cov_se     = ri_cov$ri_cov_se,
      ri_cov_p      = ri_cov$ri_cov_p,
      ri_cov_std    = ri_cov$ri_cov_std
    ) %>%
    relocate(sleep_metric)
  
  # Calculate ICC for the sleep variable (like Yamada did for fatigue)
  var_ri_x <- pe %>% filter(lhs == "RI_X", rhs == "RI_X", op == "~~") %>% pull(est)
  var_total_x <- var(df_pair[[sleep_var]], na.rm = TRUE)
  icc_x <- var_ri_x / var_total_x
  
  summary_row <- summary_row %>% mutate(icc_sleep = icc_x)

  list(fit = fit,
       fixed_pain_ri = fixed_pain_ri,
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
    fix_pain_ri_variance = TRUE,
    max_iter       = max_iter
  )
}

ri_results_summary <- bind_rows(lapply(all_results, `[[`, "summary"))

## --------------------------- 5. Output Summary ------------------------

print(ri_results_summary)

# Quick readable interpretation lines (matching Yamada's presentation)
interpret <- ri_results_summary %>%
  mutate(
    pain_to_sleep_sig_w2 = ifelse(p_b2 < .05, "YES", "no"),
    pain_to_sleep_sig_w3 = ifelse(p_b3 < .05, "YES", "no"),
    sleep_to_pain_sig_w2 = ifelse(p_d2 < .05, "YES", "no"),
    sleep_to_pain_sig_w3 = ifelse(p_d3 < .05, "YES", "no")
  ) %>%
  select(sleep_metric,
         b2_est = est_b2, b2_p = p_b2, pain_to_sleep_sig_w2,
         b3_est = est_b3, b3_p = p_b3, pain_to_sleep_sig_w3,
         d2_est = est_d2, d2_p = p_d2, sleep_to_pain_sig_w2,
         d3_est = est_d3, d3_p = p_d3, sleep_to_pain_sig_w3,
         a2_est = est_a2, c2_est = est_c2,
         a3_est = est_a3, c3_est = est_c3,
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
  relocate(sleep_metric, fixed_pain_ri,
           N,
           chisq, df, pvalue, cfi, tli, rmsea, srmr, aic, bic,
           est_a2, se_a2, p_a2, est_c2, se_c2, p_c2,
           est_b2, se_b2, p_b2, est_d2, se_d2, p_d2,
           est_a3, se_a3, p_a3, est_c3, se_c3, p_c3,
           est_b3, se_b3, p_b3, est_d3, se_d3, p_d3,
           est_cxy1, se_cxy1, p_cxy1, est_cxy2, se_cxy2, p_cxy2,
           est_cxy3, se_cxy3, p_cxy3,
           ri_cov_est, ri_cov_se, ri_cov_p, ri_cov_std, icc_sleep)

cat("\n=== MODEL FIT INDICES (3-wave RI-CLPM per sleep metric) ===\n")
print(fit_indices_summary)

cat("\n=== FULL SUMMARY (fit + dynamic paths) ===\n")
print(full_summary)

# Check which models had issues
cat("\n=== MODEL CONVERGENCE CHECK ===\n")
for (sv in sleep_metrics) {
  converged <- lavInspect(all_results[[sv]]$fit, "converged")
  cat(sv, "- Converged:", converged, "- Fixed pain RI:", all_results[[sv]]$fixed_pain_ri, "\n")
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
