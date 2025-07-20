
# README – Exploratory Data Analysis and CLPM Suitability

## What is a cross-lagged panel model?

A **cross-lagged panel model (CLPM)** tests possible directional influence between two repeatedly measured constructs by combining **autoregressive stability** (how well a variable predicts its own future) and **cross-lagged effects** (how well one variable predicts the future of another).

A standard two-variable CLPM can be written as:
```

Sleep\_{t+1} = α\_S + a \* Sleep\_t + b \* Pain\_t  + e\_S{t+1}
Pain\_{t+1}  = α\_P + c \* Pain\_t  + d \* Sleep\_t + e\_P{t+1}

```
* `a` and `c` are **autoregressive coefficients** capturing stability.
* `b` and `d` are **cross-lagged coefficients** testing directional influence.
* `e_S{t}` and `e_P{t}` are wave-specific residuals.

However, when variables exhibit systematic trends over time (i.e., they are non-stationary), a standard CLPM can produce biased results. A **Random-Intercept CLPM (RI-CLPM)** is a more robust alternative that separates stable, between-person differences (the "random intercepts") from dynamic, within-person fluctuations. This allows for a purer estimation of the cross-lagged effects.

---

## Dataset Overview

**File:** `gt9x_sleep_dsis_koos_pgaoa_weekly.csv`
**Unique participants:** 60
**Time points (waves):** 26 (indexed 0 to 25)

Each row aggregates nightly actigraphy and daily questionnaire entries into **weekly scores**. Columns fall into three families:

### Sleep Aggregates (Objective, from GT9X)
For each primary metric, the file stores the weekly **mean**, **median**, **min**, **max**, and **standard deviation**.
* `efficiency_*` – Percentage of time in bed spent asleep.
* `tst_*` – Total sleep time (minutes).
* `waso_*` – Wake after sleep onset (minutes).
* `number_of_awakenings_*` – Counts of brief awakenings per night.
* `sleep_fragmentation_index_*` – Composite index of restlessness.

> **Note:** All `_mode` columns were >80% missing and were discarded.

### Subjective Sleep Impairment
* `dsis` plus weekly `dsis_mean`, `dsis_sd`, `dsis_median` – Higher values = worse perceived sleep.

### Pain and Function Outcomes (Patient-Reported)
* `koos_pain`, `koos_adl` – 0–100, higher = better.
* `kd_womac_pain`, `kd_womac_stiff`, `kd_womac_func` – Higher = worse.
* `pgaoa` – 0–100 global OA severity, higher = worse.

Typical missingness is 18% for objective sleep means and 22-25% for symptom scales. This is best handled with **Full Information Maximum Likelihood (FIML)** estimation.

---

## Model Suitability Analysis

The exploratory analysis assessed whether the data meets the requirements for longitudinal panel modeling.

**1. Equally Spaced, Multi-Wave Data:** 
Scores are weekly aggregates, providing uniform seven-day spacing over 26 potential waves.

**2. Moderate Stability:** 
A panel model requires constructs to be stable but not static. The lag-1 autoregressive correlations show this is the case for most variables. Coefficients between 0.6 and 0.9 indicate moderate-to-high stability with sufficient within-person variance for cross-lagged effects to be estimated.

| Variable                       | Lag-1 Correlation (r) |
| :----------------------------- | :-------------------: |
| `dsis`                           |         **0.79** |
| `dsis_mean`                      |         **0.94** |
| `efficiency_mean`                |         **0.77** |
| `tst_mean`                       |         **0.71** |
| `waso_mean`                      |         **0.68** |
| `number_of_awakenings_mean`      |         **0.83** |
| `sleep_fragmentation_index_mean` |         **0.33** |
| `koos_pain`                      |         **0.87** |
| `koos_adl`                       |         **0.90** |
| `kd_womac_pain`                  |         **0.83** |
| `kd_womac_stiff`                 |         **0.79** |
| `kd_womac_func`                  |         **0.90** |
| `pgaoa`                          |         **0.75** |

**3. Stationarity and Systematic Trends:** 
A standard CLPM assumes stationarity (no systematic trends over time). The analysis of the **full 26-week dataset shows this assumption is violated.** Correlating each variable's weekly grand mean with the time variable (`redcap_event_name`) reveals significant trends.

| Variable                         | Correlation with Time (r) | p-value                 |
| :------------------------------- | :-----------------------: | :---------------------- |
| `efficiency_mean`                |           **0.83** | **< .001** |
| `pgaoa`                          |           **0.90** | **< .001** |
| `koos_pain`                      |           **0.81** | **< .001** |
| `koos_adl`                       |           **0.88** | **< .001** |
| `number_of_awakenings_mean`      |          **-0.85** | **< .001** |
| `waso_mean`                      |          **-0.82** | **< .001** |
| `kd_womac_pain`                  |          **-0.84** | **< .001** |
| `kd_womac_func`                  |          **-0.88** | **< .001** |
| `kd_womac_stiff`                 |          **-0.70** | **< .001** |
| `tst_mean`                       |           **0.52** | **0.011** |
| `dsis`                           |           -0.33           | 0.122                   |
| `dsis_mean`                      |           -0.15           | 0.475                   |
| `sleep_fragmentation_index_mean` |           -0.03           | 0.905                   |

**Conclusion:** The presence of strong, systematic trends makes a standard CLPM inappropriate. These trends must be explicitly modeled.



## Final Recommendation: Use an RI-CLPM

Based on this analysis, the recommended approach is a **Random-Intercept Cross-Lagged Panel Model (RI-CLPM)**.

