# README: How Our Cross-Lagged Panel Model (CLPM) Works — Fully Explained

We're using a Cross-Lagged Panel Model (CLPM) to understand how sleep and pain influence each other over six weekly time points. Our goal is to estimate whether poor sleep leads to increased pain (or vice versa), while accounting for each variable's stability over time. The model structure assumes that each variable at time t is a linear function of both variables at time t−1. The structural equations look like this:

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
- N is the sample size (39 participants)
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

---

# CLPM Analysis: Sleep and Pain

## Method

We examined bidirectional relationships between objective sleep measures and pain severity using cross-lagged panel models (CLPMs). The analysis included 39 participants measured weekly during weeks 18–23, providing six consecutive assessments. We tested five actigraphy-derived sleep variables against the Patient Global Assessment of Osteoarthritis (PGAoA) pain scale. Following Selig & Little (2012), our models included autoregressive paths (week-to-week stability), cross-lagged paths (directional effects), and within-wave residual covariances. All parameters were constrained equal across waves to assume stationarity, and we used Full Information Maximum Likelihood (FIML) to handle missing data.

## Table 1: Model Fit Statistics

| Sleep Measure | CFI | TLI | RMSEA | SRMR | χ² (df=56) | p-value |
|---------------|-----|-----|--------|------|------------|---------|
| **Optimal Thresholds** | **> 0.95** | **> 0.95** | **< 0.06** | **< 0.08** | **—** | **> 0.05** |
| Number of Awakenings* | 0.859 | 0.834 | 0.163 | 0.126 | 114.0 | <0.001 |
| Efficiency Mean | 0.822 | 0.790 | 0.172 | 0.150 | 121.0 | <0.001 |
| TST Mean | 0.791 | 0.754 | 0.180 | 0.179 | 127.0 | <0.001 |
| WASO Mean | 0.774 | 0.734 | 0.178 | 0.158 | 126.0 | <0.001 |
| Sleep Fragmentation | 0.633 | 0.567 | 0.196 | 0.207 | 140.0 | <0.001 |

### Fit Metric Definitions:

- **CFI (Comparative Fit Index)**: Compares model fit to a null model where all variables are uncorrelated. Higher is better; values above 0.95 are considered excellent.
- **TLI (Tucker-Lewis Index)**: Penalizes model complexity. Like CFI, it compares fit to a null model. Higher is better.
- **RMSEA (Root Mean Square Error of Approximation)**: Estimates how badly the model would fit the population covariance structure, per degree of freedom. Lower is better; under 0.06 is ideal.
- **SRMR (Standardized Root Mean Square Residual)**: Measures average discrepancy between observed and model-predicted covariances. Lower is better; values below 0.08 are ideal.
- **χ² (Chi-square)**: Tests whether the model-implied covariance matrix differs significantly from the observed matrix. Lower values (and p > .05) indicate good fit.

## Table 2: Cross-Lagged Effects (Standardized Coefficients)

| Sleep Measure | Pain → Sleep | p-value | Sleep → Pain | p-value |
|---------------|--------------|---------|--------------|---------|
| Efficiency | 0.018 | 0.728 | −0.047 | 0.343 |
| Total Sleep Time | −0.095 | 0.086 | 0.018 | 0.705 |
| WASO | −0.036 | 0.555 | 0.056 | 0.251 |
| Awakenings | −0.024 | 0.575 | 0.074 | 0.149 |
| Fragmentation | 0.110 | 0.168 | 0.064 | 0.243 |

## Results Summary

No significant cross-lagged effects emerged in any model. All standardized coefficients were small in magnitude, ranging from −0.095 to 0.110, and all p-values exceeded 0.05. The only marginal trend appeared in the total sleep time model, where higher pain was weakly associated with lower sleep in the following week (β = −0.095, p = 0.086), but this did not reach statistical significance.

Model fit was poor across all models. Every model failed to meet commonly accepted fit criteria: CFI and TLI were well below 0.95, RMSEA was well above 0.06, SRMR exceeded 0.08, and χ² tests were significant (p < .001), indicating that the model-implied covariance structure differed substantially from the observed one.

Despite this, autoregressive paths were consistently strong across models, ranging from 0.70 to 0.90, indicating high stability of both sleep and pain across weeks. This confirms that individuals' relative rankings on these constructs remain stable over time, which is a necessary condition for applying CLPMs.

Taken together, the results show that in this sample, there is no evidence for directional week-to-week influence between sleep quality and pain severity. These findings could suggest that the relationship between sleep and pain operates on a shorter timescale than weekly measurement can detect (e.g., daily or nightly), or that their coupling is more complex and nonlinear than can be captured by a time-invariant CLPM. Future models that allow effects to vary over time or that include within-day resolution may provide more insight.

