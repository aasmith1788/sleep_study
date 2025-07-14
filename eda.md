Here's the updated README with your new results:

# README – Exploratory Data Analysis and CLPM Suitability

## What is a cross‑lagged panel model?

A **cross‑lagged panel model (CLPM)** tests possible directional influence between two repeatedly measured constructs by combining **autoregressive stability** (how well a variable predicts its own future) and **cross‑lagged effects** (how well one variable predicts the future of another).

Below is the two‑variable CLPM written in plain ASCII notation so it displays correctly even in editors that do not render LaTeX:

```
Sleep_{t+1} = α_S + a * Sleep_t + b * Pain_t  + e_S{t+1}
Pain_{t+1}  = α_P + c * Pain_t  + d * Sleep_t + e_P{t+1}
```

* `a` and `c` are **autoregressive coefficients** capturing rank‑order stability.
* `b` and `d` are **cross‑lagged coefficients** that test directional influence from one construct to the other while controlling for their respective stabilities.
* `e_S{t}` and `e_P{t}` are wave‑specific residuals.  Their same‑wave covariance absorbs any concurrent correlation not explained by the lagged paths.

Stacking both equations into vector form gives

```
Y_{t+1} = α + A * Y_t + e_{t+1},
```

where `Y_t = (Sleep_t, Pain_t)ᵀ` and the coefficient matrix

```
A = | a  b |
    | d  c |
```

Collecting data at equally spaced times lets the same set of parameters repeat across waves (a stationarity assumption).  Structural‑equation software fits the model in **wide format**—Sleep₁ … Sleep\_T and Pain₁ … Pain\_T as separate observed variables—using full‑information maximum likelihood (FIML) so participants with partial missing data still contribute information.

---

## Dataset overview

**File:** `gt9x_sleep_dsis_koos_pgaoa_weekly.csv`
**Rows (person‑weeks):** 1 424
**Unique participants:** 60
**Consecutive weeks per participant:** 11 – 29 (median ≈ 24)
**Calendar weeks present:** 1 – 52

Each row aggregates nightly actigraphy and daily questionnaire entries into **weekly scores**.  Columns fall into three families:

### Sleep aggregates (objective, from GT9X)

For each primary metric the file stores weekly **mean**, **median**, **min**, **max**, **standard deviation**, **valid‑night count** (`_n`) and **missing‑night count** (`_n_miss`).

* `efficiency_*` – percentage of time in bed spent asleep.
* `tst_*` – total sleep time (minutes).
* `waso_*` – wake after sleep onset (minutes).
* `number_of_awakenings_*` – counts of brief awakenings per night.
* `sleep_fragmentation_index_*` – composite index of restlessness.

> **Note:** All *\_mode* columns are >80 % missing and were discarded.

### Subjective sleep impairment

* `dsis` plus weekly `dsis_mean`, `dsis_sd`, `dsis_median`, `dsis_missing_n` – higher values = worse perceived sleep.

### Pain and function outcomes (patient‑reported)

* `koos_pain`, `koos_adl` – 0–100, higher = better.
* `kd_womac_pain`, `kd_womac_stiff`, `kd_womac_func` – 0–20 / 0–8 / 0–68, higher = worse.
* `pgaoa` – 0–100 global OA severity, higher = worse.

All questionnaire columns were converted from text to numeric.  Typical missingness: 17 % for sleep means/medians; 23–26 % for symptom scales.

---



Lag‑one (t → t+1) Pearson correlations estimate autoregressive stability:

| Variable | Lag‑one correlation r |
| -------- | :-------------------: |
| dsis | **0.82** |
| dsis_mean | **0.94** |
| efficiency_mean | **0.76** |
| tst_mean | **0.71** |
| waso_mean | **0.67** |
| number_of_awakenings_mean | **0.81** |
| sleep_fragmentation_index_mean | **0.35** |
| koos_pain | **0.86** |
| koos_adl | **0.89** |
| kd_womac_pain | **0.82** |
| kd_womac_stiff | **0.78** |
| kd_womac_func | **0.89** |
| pgaoa | **0.76** |

Coefficients between 0.6 and 0.9 indicate **moderate stability with room for change**—exactly the pattern CLPM needs. Most variables fall within this ideal range, with `dsis_mean` showing very high stability (0.94) and `sleep_fragmentation_index_mean` showing weaker stability (0.35), flagging it as noisy but not unusable.

---

## CLMP requirements and how the dataset meets them

A two‑variable CLPM is justifiable only when four empirical conditions are satisfied.  The exploratory analysis—descriptive statistics, lag‑one correlations, and grand‑mean plots—shows that all four hold for this dataset.

**1  Equally spaced, multi‑wave data.**  Scores are weekly aggregates, giving uniform seven‑day spacing.  Every participant supplies at least eleven consecutive person‑indexed waves.  To maximise overlap, the calendar window used for modelling will be the six densest consecutive weeks (18–23), yielding roughly forty analysable cases once FIML handles partial missingness.

**2  Genuine within‑person change with moderate stability.**  A CLPM works only when each construct is neither frozen (r ≈ 1.0) nor random noise (r ≈ 0).  The week‑ahead Pearson correlations above quantify that balance.  Values around 0.7–0.9 show that participants generally keep their rank order from one week to the next (so autoregressive paths are meaningful) while still leaving 30–50 % of the week‑to‑week variance unexplained (so cross‑lagged effects have room to operate).

**3  No systematic trends within the analysis window.**  To check that no variable drifts systematically inside the six‑week block, each variable was collapsed to weekly grand means (averaging all participants present in that week), then correlated with week numbers 18 through 23.  With only six points a correlation must exceed |0.81| to be significant at α = .05.  The table below shows that all variables produced |r| ≤ 0.68 with all p > .05, indicating no detectable systematic trends.

| Variable | r | p_val |
|----------|---|-------|
| dsis | 0.371 | 0.469 |
| dsis_mean | 0.681 | 0.136 |
| efficiency_mean | -0.571 | 0.236 |
| kd_womac_func | -0.190 | 0.718 |
| kd_womac_pain | -0.187 | 0.723 |
| kd_womac_stiff | -0.261 | 0.617 |
| koos_adl | 0.190 | 0.718 |
| koos_pain | 0.180 | 0.733 |
| number_of_awakenings_mean | 0.377 | 0.461 |
| pgaoa | 0.224 | 0.670 |
| sleep_fragmentation_index_mean | -0.077 | 0.886 |
| tst_mean | -0.488 | 0.326 |
| waso_mean | 0.431 | 0.393 |

<img width="802" height="558" alt="image" src="https://github.com/user-attachments/assets/1f6bf48a-1cf0-41d0-8857-d4459fba7a45" />

<img width="802" height="558" alt="image" src="https://github.com/user-attachments/assets/d41b6393-fe3d-4d63-873e-c6b9446f1a98" />



**4  We have enough data to run our statistical model reliably.** Our cross-lagged panel model needs to estimate about 8-9 key numbers (like how much sleep predicts future pain, how much pain predicts future sleep, etc.). With 40 participants measured across 6 time points, we get about 200 data transitions to work with. That gives us roughly a 22-to-1 ratio of data points to parameters we're estimating, which is well above the recommended 10-to-1 minimum that statisticians like to see.

Together these points satisfy the standards laid out by Selig & Little (2012): evenly spaced waves, observable yet controlled change, approximate stationarity, and an estimable likelihood surface.


### Alignment with Selig & Little (2012)

The paper outlines four modelling principles: (1) include autoregressive paths to capture rank‑order stability; (2) estimate cross‑lagged paths in both directions to test potential causal ordering; (3) allow within‑wave residual covariances so lagged effects are not inflated by concurrent correlations; and (4) impose equality constraints across waves to enforce stationarity unless theory dictates otherwise.  All four principles are incorporated here: autoregressive stability was confirmed empirically (AR ≈ 0.6–0.9), bidirectional cross‑lagged paths will be estimated, residual covariances are retained in every proposed specification, and equality constraints are recommended for six‑wave and eleven‑wave models.  Hence the analysis plan and terminology align directly with Selig & Little's guidance.
