# CLPM Analysis: Sleep and Pain
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

## Table X. Trend Analysis Comparison: All Waves vs First 6 Waves

| Variable | All Waves (0-25) |  | First 6 Waves (0-5) |  | Trend Reduction |
|----------|------------------|--|---------------------|--|-----------------|
|          | r | p-value | r | p-value | % Reduction |
| **Pain/Function Variables** |  |  |  |  |  |
| PGAOA | 0.897*** | <0.001 | 0.062 | 0.907 | 93.1% |
| KOOS ADL | 0.878*** | <0.001 | 0.801 | 0.055 | 8.7% |
| KOOS Pain | 0.807*** | <0.001 | 0.873* | 0.023 | -8.2%ᵃ |
| WOMAC Pain | -0.839*** | <0.001 | -0.818* | 0.047 | 2.5% |
| WOMAC Stiffness | -0.701*** | <0.001 | -0.842* | 0.036 | -20.1%ᵃ |
| WOMAC Function | -0.878*** | <0.001 | -0.801 | 0.055 | 8.7% |
| **Sleep Variables** |  |  |  |  |  |
| Sleep Efficiency | 0.833*** | <0.001 | 0.633 | 0.177 | 23.9% |
| Total Sleep Time | 0.522* | 0.011 | 0.474 | 0.342 | 9.2% |
| WASO | -0.815*** | <0.001 | -0.650 | 0.163 | 20.3% |
| Awakenings | -0.848*** | <0.001 | -0.688 | 0.131 | 19.0% |
| Sleep Fragmentation | -0.026 | 0.905 | 0.071 | 0.894 | -169.2%ᵃ |
| **Symptom Variables** |  |  |  |  |  |
| DSIS | -0.325 | 0.122 | -0.471 | 0.346 | -44.9%ᵃ |
| DSIS Mean | -0.153 | 0.475 | -0.719 | 0.171 | -370.0%ᵃ |

ICC measures the proportion of total variance that comes from stable between-person differences versus within-person changes over time.
Formula: ICC = Between-Person Variance / Total Variance

Interpretation:

ICC ≥ 0.75: Large between-person differences
ICC ≥ 0.50: Moderate between-person differences
ICC ≥ 0.25: Small between-person differences
ICC < 0.25: Minimal between-person differences

| Variable | ICC | Interpretation |
|----------|-----|----------------|
| KOOS ADL | 0.836 | Large between-person differences |
| WOMAC Function | 0.836 | Large between-person differences |
| KOOS Pain | 0.792 | Large between-person differences |
| DSIS Mean | 0.783 | Large between-person differences |
| Awakenings | 0.750 | Large between-person differences |
| WOMAC Pain | 0.741 | Moderate between-person differences |
| Total Sleep Time | 0.732 | Moderate between-person differences |
| Sleep Efficiency | 0.704 | Moderate between-person differences |
| WOMAC Stiffness | 0.697 | Moderate between-person differences |
| DSIS | 0.666 | Moderate between-person differences |
| WASO | 0.637 | Moderate between-person differences |
| Sleep Fragmentation | 0.577 | Moderate between-person differences |
| PGAOA | 0.565 | Moderate between-person differences |

## Method

Examined bidirectional relationships between objective sleep measures and pain severity using random intercept cross-lagged panel models (CLPMs). The analysis included 39 participants measured weekly during weeks 18–23, providing six consecutive assessments. We tested five actigraphy-derived sleep variables against the Patient Global Assessment of Osteoarthritis (PGAoA) pain scale. Following Selig & Little (2012), our models included autoregressive paths (week-to-week stability), cross-lagged paths (directional effects), and within-wave residual covariances. All parameters were constrained equal across waves to assume stationarity, and we used Full Information Maximum Likelihood (FIML) to handle missing data.

## Random-Intercept Cross-Lagged Panel Model (RI-CLPM)

However, when variables exhibit systematic trends over time (i.e., they are non-stationary), a standard CLPM can produce biased results. A **Random-Intercept CLPM (RI-CLPM)** is a more robust alternative that separates stable, between-person differences (the "random intercepts") from dynamic, within-person fluctuations. This allows for a purer estimation of the cross-lagged effects.

### CLPM

```
Sleep_t = μ_S,t + a·Sleep_t-1 + b·Pain_t-1 + ε_S,t
Pain_t = μ_P,t + c·Pain_t-1 + d·Sleep_t-1 + ε_P,t
```

Here, `a` and `c` represent autoregressive effects (week-to-week stability), while `b` and `d` are the cross-lagged effects of interest—capturing whether sleep predicts later pain or pain predicts later sleep. The μ terms are intercepts, and the ε terms represent unexplained variance at each time point.


###  RI-CLPM

For each individual i at time t:

```
Sleep_{i,t} = μ_S + RI_S{i} + w_S{i,t}
Pain_{i,t} = μ_P + RI_P{i} + w_P{i,t}
```

Where:
- `μ_S` and `μ_P` are grand means across all individuals and time points
- `RI_S{i}` and `RI_P{i}` are person-specific random intercepts capturing stable between-person differences
- `w_S{i,t}` and `w_P{i,t}` are within-person deviations from one's typical level

The random intercepts are allowed to correlate:
```
Cov(RI_S, RI_P) = σ_{RI_S,RI_P}
```

This correlation captures the between-person association: do people who generally sleep better also tend to have less pain overall?

### Within-Person Dynamics in RI-CLPM

The within-person components follow the same autoregressive and cross-lagged structure as a standard CLPM:

```
w_S{i,t} = a·w_S{i,t-1} + b·w_P{i,t-1} + ε_S{i,t}
w_P{i,t} = c·w_P{i,t-1} + d·w_S{i,t-1} + ε_P{i,t}
```

Critically, these parameters now have a different interpretation:
- `a` and `c` capture within-person stability: when someone deviates from their typical sleep/pain level, how much does that deviation persist?
- `b` and `d` capture within-person cross-lagged effects: when someone's pain is higher than their usual level, does their sleep deviate from baseline in the following week?

## Method

We examined bidirectional relationships between objective sleep measures and KOOS pain scores using Random-Intercept Cross-Lagged Panel Models (RI-CLPMs). The analysis included 60 participants measured across the first 6 weeks (waves 0-5), providing consecutive weekly assessments. We tested five actigraphy-derived sleep variables against the KOOS pain scale (0-100, higher = better function/less pain). Following Hamaker et al. (2015), our models decomposed observed scores into stable between-person differences (random intercepts) and time-varying within-person fluctuations. All within-person autoregressive and cross-lagged paths were constrained equal across waves to assume stationarity, and we used Full Information Maximum Likelihood (FIML) with robust standard errors (MLR estimator) to handle missing data.

## Table 1: Model Fit Statistics

| Sleep Measure | N | χ² (df=53) | p-value | CFI | TLI | RMSEA | SRMR | 
|---------------|---|------------|---------|-----|-----|-------|------|
| **Optimal Thresholds** | — | — | **> 0.05** | **> 0.95** | **> 0.95** | **< 0.06** | **< 0.08** |
| Efficiency Mean | 60 | 150.0 | <0.001 | 0.840 | 0.801 | 0.175 | 0.126 | 
| TST Mean | 60 | 120.0 | <0.001 | 0.886 | 0.858 | 0.145 | 0.152 |
| WASO Mean | 60 | 147.0 | <0.001 | 0.828 | 0.785 | 0.172 | 0.130 | 
| Number of Awakenings | 60 | 156.0 | <0.001 | 0.845 | 0.808 | 0.180 | 0.123 | 
| Sleep Fragmentation | 60 | 142.0 | <0.001 | 0.813 | 0.767 | 0.167 | 0.144 |

### Fit Metric Definitions:

- **CFI (Comparative Fit Index)**: Compares model fit to a null model where all variables are uncorrelated. Higher is better; values above 0.95 are considered excellent.
- **TLI (Tucker-Lewis Index)**: Penalizes model complexity. Like CFI, it compares fit to a null model. Higher is better.
- **RMSEA (Root Mean Square Error of Approximation)**: Estimates how badly the model would fit the population covariance structure, per degree of freedom. Lower is better; under 0.06 is ideal.
- **SRMR (Standardized Root Mean Square Residual)**: Measures average discrepancy between observed and model-predicted covariances. Lower is better; values below 0.08 are ideal.
- **χ² (Chi-square)**: Tests whether the model-implied covariance matrix differs significantly from the observed matrix. Lower values (and p > .05) indicate good fit.

## Table 2: Within-Person Cross-Lagged Effects (Standardized Coefficients)

| Sleep Measure | Pain → Sleep (b) | p-value | Sleep → Pain (d) | p-value |
|---------------|------------------|---------|------------------|---------|
| Efficiency | −0.168 | 0.184 | −0.148 | 0.275 |
| Total Sleep Time | −0.180 | 0.071 | −0.288 | 0.243 |
| WASO | 0.139 | 0.118 | 0.144 | 0.145 |
| Awakenings* | **0.172** | **0.035** | 0.094 | 0.276 |
| Fragmentation | −0.099 | 0.573 | −0.073 | 0.499 |

*Significant at p < 0.05

## Table 3: Within-Person Autoregressive Effects and Random Intercept Correlations

| Sleep Measure | Sleep AR (a) | p-value | Pain AR (c) | p-value | RI Correlation | p-value |
|---------------|--------------|---------|-------------|---------|----------------|---------|
| Efficiency | 0.226 | 0.069 | 0.289 | 0.526 | −0.008 | 0.958 |
| Total Sleep Time | 0.041 | 0.747 | 0.308 | 0.282 | 0.026 | 0.893 |
| WASO | 0.200* | 0.013 | 0.295 | 0.535 | −0.010 | 0.945 |
| Awakenings | 0.140 | 0.151 | 0.339 | 0.543 | 0.013 | 0.928 |
| Fragmentation | 0.114 | 0.424 | 0.315 | 0.483 | 0.482*** | <0.001 |
