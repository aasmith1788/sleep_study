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
stationary     <- TRUE       
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

build_ri_clpm <- function(x_obs, y_obs, stationary = TRUE) {
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

fit_one_pair <- function(sleep_var, pain_var, dat_long, n_waves,
                         standardize_xy = TRUE,
                         stationary = TRUE,
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
  
  syntax_stat  <- build_ri_clpm(x_obs, y_obs, stationary = TRUE)
  ctrl <- list(iter.max = max_iter)
  
  fit_stat <- try(
    sem(model = syntax_stat,
        data = dat_wide,
        missing = "fiml",
        estimator = "MLR",
        control = ctrl),
    silent = TRUE
  )
  
  used_stationary <- TRUE
  if (inherits(fit_stat, "try-error") || !lavInspect(fit_stat, "converged")) {
    syntax_un <- build_ri_clpm(x_obs, y_obs, stationary = FALSE)
    fit_stat <- sem(model = syntax_un,
                    data = dat_wide,
                    missing = "fiml",
                    estimator = "MLR",
                    control = ctrl)
    used_stationary <- FALSE
    if (!lavInspect(fit_stat, "converged"))
      stop(paste("Model failed to converge even unconstrained for", sleep_var))
  }
  
  pe <- parameterEstimates(fit_stat, standardized = TRUE)
  
  if (used_stationary) {
    grabs <- tibble(
      param = c("a","b","c","d"),
      est   = sapply(c("a","b","c","d"), \(lab) pe$std.all[match(lab, pe$label)]),
      se    = sapply(c("a","b","c","d"), \(lab) pe$se[match(lab, pe$label)]),
      p     = sapply(c("a","b","c","d"), \(lab) pe$pvalue[match(lab, pe$label)])
    )
  } else {
    # For 3 waves, we might have a2,b2,c2,d2 and a3,b3,c3,d3
    pull_multi <- function(prefix) {
      rows <- pe %>% filter(grepl(paste0("^", prefix, "[0-9]"), label))
      if(nrow(rows) == 0) return(list(mean_std = NA, any_sig = NA))
      tibble(
        lags = paste(rows$label, collapse=";"),
        mean_std = mean(rows$std.all, na.rm = TRUE),
        any_sig = any(rows$pvalue < .05, na.rm = TRUE)
      )
    }
    grabs <- tibble(
      param = c("a","b","c","d"),
      est   = NA_real_,
      se    = NA_real_,
      p     = NA_real_
    )
    a_info <- pull_multi("a")
    b_info <- pull_multi("b")
    c_info <- pull_multi("c")
    d_info <- pull_multi("d")
    grabs$est <- c(a_info$mean_std, b_info$mean_std, c_info$mean_std, d_info$mean_std)
    grabs$p   <- c(ifelse(is.na(a_info$any_sig), NA, as.numeric(a_info$any_sig)),
                   ifelse(is.na(b_info$any_sig), NA, as.numeric(b_info$any_sig)),
                   ifelse(is.na(c_info$any_sig), NA, as.numeric(c_info$any_sig)),
                   ifelse(is.na(d_info$any_sig), NA, as.numeric(d_info$any_sig)))
  }
  
  ri_cov <- pe %>%
    filter(lhs == "RI_X", rhs == "RI_Y", op == "~~") %>%
    transmute(ri_cov_est = est, ri_cov_se = se, ri_cov_p = pvalue, ri_cov_std = std.all)
  
  summary_row <- grabs %>%
    pivot_wider(names_from = param,
                values_from = c(est,se,p),
                names_sep = "_") %>%
    mutate(
      sleep_metric = sleep_var,
      stationary   = used_stationary,
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
       stationary = used_stationary,
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
    stationary     = stationary,
    max_iter       = max_iter
  )
}

ri_results_summary <- bind_rows(lapply(all_results, `[[`, "summary"))

## --------------------------- 5. Output Summary ------------------------

print(ri_results_summary)

# Quick readable interpretation lines (matching Yamada's presentation)
interpret <- ri_results_summary %>%
  mutate(
    pain_to_sleep_sig  = ifelse(p_b < .05, "YES", "no"),
    sleep_to_pain_sig  = ifelse(p_d < .05, "YES", "no")
  ) %>%
  select(sleep_metric,
         b_est = est_b, b_p = p_b, pain_to_sleep_sig,
         d_est = est_d, d_p = p_d, sleep_to_pain_sig,
         a_est = est_a, c_est = est_c,
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
  relocate(sleep_metric, stationary,
           N,
           chisq, df, pvalue, cfi, tli, rmsea, srmr, aic, bic,
           est_a, se_a, p_a, est_c, se_c, p_c,
           est_b, se_b, p_b, est_d, se_d, p_d,
           ri_cov_est, ri_cov_se, ri_cov_p, ri_cov_std, icc_sleep)

cat("\n=== MODEL FIT INDICES (3-wave RI-CLPM per sleep metric) ===\n")
print(fit_indices_summary)

cat("\n=== FULL SUMMARY (fit + dynamic paths) ===\n")
print(full_summary)

# Check which models had issues
cat("\n=== MODEL CONVERGENCE CHECK ===\n")
for (sv in sleep_metrics) {
  converged <- lavInspect(all_results[[sv]]$fit, "converged")
  cat(sv, "- Converged:", converged, "- Used stationary:", all_results[[sv]]$stationary, "\n")
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
