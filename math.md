# README – Exploratory Data Analysis and CLPM Suitability

## What is a cross-lagged panel model?

A **cross-lagged panel model (CLPM)** tests possible directional influence between two repeatedly measured constructs by combining **autoregressive stability** (how well a variable predicts its own future) and **cross-lagged effects** (how well one variable predicts the future of another).

A standard two-variable CLPM can be written as:
```
Sleep_t = μ_S,t + a·Sleep_t-1 + b·Pain_t-1 + ε_S,t
Pain_t = μ_P,t + c·Pain_t-1 + d·Sleep_t-1 + ε_P,t
```

Here, `a` and `c` represent autoregressive effects (week-to-week stability), while `b` and `d` are the cross-lagged effects of interest—capturing whether sleep predicts later pain or pain predicts later sleep. The μ terms are intercepts, and the ε terms represent unexplained variance at each time point.

Although these regression equations appear simple, estimating them requires a lot more than solving a standard linear model. That's because in SEM, we're estimating an entire system of relationships. That system includes not only the lagged regression paths, but also the variances and covariances needed to reproduce the observed relationships among all 12 variables (Sleep and Pain across 6 weeks).

## Parameters Estimated

Some parameters are estimated directly. These include:

- The initial variances and covariance at wave 1: `Var(Sleep_1)`, `Var(Pain_1)`, `Cov(Sleep_1, Pain_1)`
- The residual variances at each wave (t = 2 to 6): `Var(ε_S,t)`, `Var(ε_P,t)`
- The residual covariances at each wave (t = 2 to 6): `Cov(ε_S,t, ε_P,t)`

These are free parameters because there are no prior values that could define them, or they capture noise that's not explained by prior predictors.

Other variances and covariances are not directly estimated, but are computed from the structural paths and estimated variance parameters. These are called model-implied variances and covariances. For example:

```
Var(Sleep_2) = a²·Var(Sleep_1) + b²·Var(Pain_1) + 2ab·Cov(Sleep_1, Pain_1) + Var(ε_S,2)
Cov(Sleep_1, Pain_2) = d·Var(Sleep_1) + c·Cov(Sleep_1, Pain_1)
```

These values are computed from the model equations and parameter estimates. The complete set of model-implied variances and covariances make up the model-implied covariance matrix Σ(θ).

## Maximum Likelihood Estimation

We use Maximum Likelihood Estimation (MLE) to estimate all of the model parameters. The likelihood function describes the probability of observing our data given a set of parameter values θ. For multivariate normal data, the log-likelihood function is:

```
ℓ(θ) = -N/2 [p·log(2π) + log|Σ(θ)| + tr(S·Σ(θ)⁻¹) + (ȳ - μ(θ))'·Σ(θ)⁻¹·(ȳ - μ(θ))]
```

Where:
- N is the sample size (60 participants for RI-CLPM)
- p is the number of observed variables (12)
- S is the sample covariance matrix
- Σ(θ) is the model-implied covariance matrix
- ȳ is the vector of sample means
- μ(θ) is the vector of model-implied means

We maximize this log-likelihood by updating all parameters until the model-implied covariance matrix Σ(θ) is as close as possible to the observed sample covariance matrix S. Some parameters are estimated because they are structural paths (a, b, c, d), others because they are starting values (like Var(Sleep_1)), and others because they are unexplained error components.

Residual variances (e.g., Var(ε_S,t)) capture how much variation in Sleep_t is left over after accounting for Sleep_t-1 and Pain_t-1. Residual covariances (e.g., Cov(ε_S,t, ε_P,t)) tell us how Sleep and Pain still co-vary within the same week, even after lagged effects have been controlled. Estimating these is essential: without them, the model would falsely attribute any remaining correlation to the lagged paths.

In short, we estimate a full system of parameters so that the resulting Σ(θ) best reproduces the observed data. The observed variances and covariances are treated as imperfect measurements (sample estimates), and we use MLE to find the most likely parameter values that could have generated them. That's why even though variances and covariances can be calculated from the data as sample statistics, they must still be estimated from the model — because our sample is only one realization of the truth, and we want the model to find the true population parameters that best explain that sample.

## Stationarity Assumption

We also assumed that the four structural paths a, b, c, d were constant (equal) across all five lags, meaning the effect of Sleep_t-1 on Sleep_t is assumed to be the same from week to week, rather than varying each time. This stationarity assumption drastically reduces the number of parameters to estimate—from potentially 20+ paths to just four—which improves statistical power, increases model stability, and makes it easier to detect general trends. However, it also imposes a strong constraint: if effects do change over time, the model won't be able to capture that. Relaxing this constraint would allow each week-to-week transition to have its own unique parameters but would require a much larger sample size and could result in overfitting.

More importantly, stationarity also simplifies the structure of the model-implied covariance matrix Σ(θ). When parameters like a and b are held constant, the entire matrix can be recursively constructed using fewer building blocks—allowing more accurate estimation of how the system evolves over time. If the paths were allowed to vary across time, Σ(θ) would require different propagation equations for each wave, vastly increasing complexity and reducing identifiability. Holding the paths equal allows MLE to more efficiently estimate the entire set of covariances in a way that makes the model matrix closely match the observed matrix S under fewer assumptions.

## Full Information Maximum Likelihood (FIML)

Full Information Maximum Likelihood (FIML) was used to handle missing data. FIML is not a separate estimation method from MLE, but rather an extension of it that allows individuals with partially missing data to contribute information to the likelihood function. Instead of dropping cases with any missing values, FIML computes the likelihood for each individual based on the variables they do have, and combines those partial likelihoods into one full sample likelihood. This increases power, reduces bias, and allows us to retain participants with incomplete assessments. Because CLPM uses multiple variables measured across time, this is especially important — FIML ensures we don't lose cases due to missed weekly recordings and still get efficient and consistent estimates.

## Example: What the Covariance Matrix Looks Like

The model-implied covariance matrix Σ(θ) is a 12 × 12 matrix representing all pairwise covariances among Sleep and Pain variables across 6 time points. Each cell is filled in based on the estimated parameters and model equations.

```
Σ(θ) = [Var(S₁)      Cov(S₁,P₁)    ...  Cov(S₁,S₆)    Cov(S₁,P₆)   ]
       [Cov(P₁,S₁)   Var(P₁)       ...  Cov(P₁,S₆)    Cov(P₁,P₆)   ]
       [⋮            ⋮             ⋱    ⋮             ⋮            ]
       [Cov(S₆,S₁)   Cov(S₆,P₁)    ...  Var(S₆)       Cov(S₆,P₆)   ]
       [Cov(P₆,S₁)   Cov(P₆,P₁)    ...  Cov(P₆,S₆)    Var(P₆)      ]
```

## Random-Intercept Cross-Lagged Panel Model (RI-CLPM)

However, when variables exhibit systematic trends over time (i.e., they are non-stationary), a standard CLPM can produce biased results. A **Random-Intercept CLPM (RI-CLPM)** is a more robust alternative that separates stable, between-person differences (the "random intercepts") from dynamic, within-person fluctuations. This allows for a purer estimation of the cross-lagged effects.

### Mathematical Specification of RI-CLPM

The key innovation of the RI-CLPM is the decomposition of observed scores into stable trait-like components and time-varying state-like components. For each individual i at time t:

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

### Parameters Estimated in RI-CLPM

The RI-CLPM estimates additional parameters compared to the standard CLPM:

**Random Intercept Parameters:**
- `Var(RI_S)`: Between-person variance in sleep
- `Var(RI_P)`: Between-person variance in pain  
- `Cov(RI_S, RI_P)`: Between-person covariance

**Within-Person Initial Conditions:**
- `Var(w_S{1})`: Within-person variance at wave 1 for sleep
- `Var(w_P{1})`: Within-person variance at wave 1 for pain
- `Cov(w_S{1}, w_P{1})`: Within-person covariance at wave 1

**Within-Person Dynamics:**
- Autoregressive paths: `a`, `c`
- Cross-lagged paths: `b`, `d`
- Residual variances: `Var(ε_S{t})`, `Var(ε_P{t})` for t = 2 to 6
- Residual covariances: `Cov(ε_S{t}, ε_P{t})` for t = 2 to 6

### Model-Implied Covariance Structure in RI-CLPM

The model-implied variances and covariances become more complex due to the random intercept structure. For example:

```
Var(Sleep_t) = Var(RI_S) + Var(w_S{t})
Cov(Sleep_t, Pain_t) = Cov(RI_S, RI_P) + Cov(w_S{t}, w_P{t})
Cov(Sleep_t, Sleep_{t+k}) = Var(RI_S) + Cov(w_S{t}, w_S{t+k})
```

The within-person variances and covariances evolve according to:
```
Var(w_S{t}) = a²·Var(w_S{t-1}) + b²·Var(w_P{t-1}) + 2ab·Cov(w_S{t-1}, w_P{t-1}) + Var(ε_S{t})
```

This recursive structure means that the model-implied covariance matrix Σ(θ) now depends on both the between-person random intercept parameters and the within-person dynamic parameters.

### Identification Constraints in RI-CLPM

To identify the model, we impose several constraints:
1. Random intercepts have unit loadings on all observed variables
2. Random intercepts are uncorrelated with within-person components at wave 1
3. Observed variable residuals are fixed to zero (all variance is captured by RIs and within-person components)
4. Within-person component means are fixed to zero

These identification constraints are mathematical requirements that make the RI-CLPM estimable and ensure a clean separation between stable individual differences and time-varying changes. The unit loadings constraint (constraint 1) means that a person's random intercept affects all their measurements equally—if someone is one unit above average on their sleep intercept, they're one unit above average at every time point. This creates the "stable trait" interpretation. The orthogonality constraint (constraint 2) prevents the model from arbitrarily shifting variance between the random intercepts and initial within-person components, which would make the decomposition non-unique. The zero residual constraint (constraint 3) is perhaps the most conceptually important: it forces all observed variance to be explained by either the random intercept or the within-person component, with no additional measurement error. This means any correlation between sleep and pain must work through one of these two pathways—either through stable between-person associations (captured by the correlation between random intercepts) or through within-person dynamics (captured by the cross-lagged paths). Finally, fixing within-person component means to zero (constraint 4) ensures that the random intercepts capture all mean differences between individuals, while the within-person components only capture deviations from one's own typical level. Together, these constraints create a model where the question "does sleep affect pain?" is cleanly separated into "do people who sleep better generally have less pain?" (between-person) versus "when someone sleeps better than their usual, does their pain improve?" (within-person).

OVERVIEW SUMMARY: 
The fundamental difference between a standard CLPM and an RI-CLPM lies in how they conceptualize and model individual differences. A standard CLPM treats all variance as time-varying, meaning it assumes that when Person A consistently sleeps better than Person B across all weeks, this stable difference is part of the same process that explains why Person A might sleep better in Week 3 than Week 2. This conflation becomes problematic because the model cannot distinguish between "Person A always sleeps better than others" (a between-person phenomenon) and "Person A slept better than usual this week" (a within-person phenomenon). As a result, the cross-lagged paths in a standard CLPM capture a mixture of between-person associations (people who generally sleep better tend to have less pain) and within-person dynamics (when someone sleeps better than their usual, their pain improves), making it impossible to determine whether sleep actually influences pain within individuals over time.
The RI-CLPM solves this problem by explicitly decomposing each person's observed score into two components: a time-invariant random intercept that captures their stable position relative to others, and time-varying deviations from their own typical level. This decomposition is crucial for handling non-stationary data because systematic trends often arise from between-person differences—for instance, some individuals might show steady improvement while others remain stable or worsen. In a standard CLPM, these person-specific trajectories would bias the estimation of cross-lagged effects because the model would interpret these stable individual trends as evidence of variables influencing each other over time. The RI-CLPM absorbs these stable differences (including person-specific linear trends) into the random intercepts, leaving only the within-person fluctuations to inform the cross-lagged parameters. This means the model can handle data where the group-level means change over time, as long as these changes reflect aggregations of stable individual differences rather than synchronized within-person changes. The cross-lagged effects in an RI-CLPM thus answer a more precise question: when an individual deviates from their own typical level on one variable, does this predict their deviation on another variable at the next time point?


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



Even after restricting analysis to the first 6 waves to minimize trending effects, people still differ systematically in their stable average levels across variables, and these between-person differences can contaminate estimates of within-person causal processes. For example, Person A might consistently average 8 hours of sleep and 3/10 pain across all 6 waves, while Person B consistently averages 5 hours of sleep and 7/10 pain. When a standard CLPM analyzes this data, it sees that higher sleep values tend to co-occur with lower pain values and incorrectly concludes that "sleep improvements cause pain reductions." However, this correlation actually reflects stable between-person differences rather than true within-person causal dynamics—Person A is simply a different type of person (good sleeper, low pain) compared to Person B (poor sleeper, high pain). The RI-CLPM addresses this by estimating and removing each person's stable average level (their "random intercept") before examining cross-lagged relationships, ensuring that the resulting estimates reflect genuine within-person fluctuations around each individual's personal baseline rather than contamination from stable trait-like differences between people.

# RI-CLPM Analysis: Sleep and Pain

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

AR = Autoregressive effect; RI = Random Intercept  
*p < 0.05, ***p < 0.001

## Results Summary

### Key Findings

1. **Limited Cross-Lagged Effects**: Only one significant within-person cross-lagged effect emerged across all models. Higher KOOS pain scores (indicating better function/less pain) predicted more awakenings in the following week (β = 0.172, p = 0.035). This counterintuitive finding may reflect increased activity levels or reduced pain medication use when pain is lower, potentially leading to lighter, more disrupted sleep.

2. **Low Within-Person Stability**: After accounting for stable between-person differences, within-person autoregressive effects were surprisingly weak. Sleep variables showed minimal week-to-week stability (a ranging from 0.041 to 0.226), with only WASO showing significant autoregression (a = 0.200, p = 0.013). Pain showed more consistent but non-significant within-person stability across models (c ranging from 0.289 to 0.339).

3. **Strong Between-Person Association for Sleep Fragmentation**: The random intercept correlation between sleep fragmentation and pain was substantial (r = 0.482, p < 0.001), indicating that individuals who generally experience more fragmented sleep tend to report worse pain overall. Other sleep metrics showed negligible between-person associations with pain (|r| < 0.03).

4. **Poor Model Fit**: All models demonstrated inadequate fit to the data, with CFI values ranging from 0.813 to 0.886 (well below the 0.95 threshold), RMSEA values from 0.145 to 0.180 (above the 0.06 threshold), and significant χ² tests (all p < 0.001). This poor fit likely reflects the limited sample size (N = 60) relative to model complexity.

### Model Complexity and Sample Size Considerations

The poor model fit across all specifications is likely driven by insufficient statistical power. RI-CLPMs are parameter-intensive models that simultaneously estimate:
- 2 random intercept variances
- 1 random intercept covariance  
- 4 within-person structural paths (2 autoregressive, 2 cross-lagged)
- 6 residual variances (one per wave)
- 5 residual covariances (waves 2-6)
- Additional means and intercepts

With only 60 participants and 18-25% missing data, the model lacks sufficient information to precisely estimate all parameters. Simulation studies suggest that RI-CLPMs typically require samples of 100-200+ participants for adequate power and model fit, particularly when effects are small to moderate as observed here.

### Conclusions

The RI-CLPM analysis provides limited evidence for weekly directional effects between sleep and pain after accounting for stable individual differences. The single significant finding—that better pain predicts more awakenings—warrants replication in larger samples before drawing firm conclusions. The strong between-person association for sleep fragmentation suggests that chronic sleep disruption and pain may be linked through stable individual differences rather than week-to-week fluctuations.

The consistently poor model fit and weak within-person effects suggest that weekly measurement intervals may be too coarse to capture the dynamic interplay between sleep and pain. These processes may operate on shorter timescales (daily or within-day) or involve more complex, non-linear relationships than can be captured by time-invariant linear models. Future research should consider:
1. Larger sample sizes (N > 150) to improve model estimation
2. More frequent measurement (e.g., daily assessments)
3. Alternative modeling approaches that allow for time-varying effects or non-linear dynamics
4. Inclusion of potential moderators such as pain medication use, physical activity, or psychological factors

