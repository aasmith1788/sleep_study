# CLPM Analysis: Sleep and Pain

## Method

We examined bidirectional relationships between objective sleep measures and pain severity using cross-lagged panel models (CLPMs). The analysis included 39 participants measured weekly during weeks 18–23, providing six consecutive assessments. We tested five actigraphy-derived sleep variables against the Patient Global Assessment of Osteoarthritis (PGAoA) pain scale. Following Selig & Little (2012), our models included autoregressive paths (week-to-week stability), cross-lagged paths (directional effects), and within-wave residual covariances. All parameters were constrained equal across waves to assume stationarity, and we used Full Information Maximum Likelihood (FIML) to handle missing data.

```
Sleep_t = μ_S,t + a·Sleep_t-1 + b·Pain_t-1 + ε_S,t
Pain_t = μ_P,t + c·Pain_t-1 + d·Sleep_t-1 + ε_P,t
```
Here, `a` and `c` represent autoregressive effects (week-to-week stability), while `b` and `d` are the cross-lagged effects of interest—capturing whether sleep predicts later pain or pain predicts later sleep. The μ terms are intercepts, and the ε terms represent unexplained variance at each time point.

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

