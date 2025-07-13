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
**Rows (person‑weeks):** 1 424
**Unique participants:** 60
**Consecutive weeks per participant:** 11 – 29 (median ≈ 24)
**Calendar weeks present:** 1 – 52

Each row aggregates nightly actigraphy and daily questionnaire entries into **weekly scores**.  Columns fall into three families:

### Sleep aggregates (objective, from GT9X)

For each primary metric the file stores weekly **mean**, **median**, **min**, **max**, **standard deviation**, **valid‑night count** (`_n`) and **missing‑night count** (`_n_miss`).

* `efficiency_*` – percentage of time in bed spent asleep.
* `tst_*` – total sleep time (minutes).
* `waso_*` – wake after sleep onset (minutes).
* `number_of_awakenings_*` – counts of brief awakenings per night.
* `sleep_fragmentation_index_*` – composite index of restlessness.

> **Note:** All *\_mode* columns are >80 % missing and were discarded.

### Subjective sleep impairment

* `dsis` plus weekly `dsis_mean`, `dsis_sd`, `dsis_median`, `dsis_missing_n` – higher values = worse perceived sleep.

### Pain and function outcomes (patient‑reported)

* `koos_pain`, `koos_adl` – 0–100, higher = better.
* `kd_womac_pain`, `kd_womac_stiff`, `kd_womac_func` – 0–20 / 0–8 / 0–68, higher = worse.
* `pgaoa` – 0–100 global OA severity, higher = worse.

All questionnaire columns were converted from text to numeric.  Typical missingness: 17 % for sleep means/medians; 23–26 % for symptom scales.

---

## Do the variables actually change week‑to‑week?

Lag‑one (t → t+1) Pearson correlations estimate autoregressive stability:

* **Sleep efficiency mean:** 0.76 • **TST mean:** 0.71 • **WASO mean:** 0.67 • **Awakenings mean:** 0.81
* **KOOS Pain:** 0.86 • **KOOS ADL:** 0.89 • **WOMAC Pain:** 0.82 • **WOMAC Function:** 0.89

Coefficients between 0.6 and 0.9 indicate **moderate stability with room for change**—exactly the pattern CLPM needs.  Fragmentation‑index means show weaker stability (≈ 0.35), flagging them as noisy but not unusable.

Visual series plots confirm gentle drifts (e.g., average efficiency rises \~2 percentage points midsummer) rather than flat lines, so neither construct is static.

---

## CLPM requirements and how the dataset meets them

A two‑variable CLPM is justifiable only when four empirical conditions are satisfied.  The exploratory analysis—descriptive statistics, lag‑one correlations, and grand‑mean plots—shows that all four hold for this dataset.

**1  Equally spaced, multi‑wave data.**  Scores are weekly aggregates, giving uniform seven‑day spacing.  Every participant supplies at least eleven consecutive person‑indexed waves.  To maximise overlap, the calendar window used for modelling will be the six densest consecutive weeks (18–23), yielding roughly forty analysable cases once FIML handles partial missingness.

**2  Genuine within‑person change with moderate stability.**  A CLPM works only when each construct is neither frozen (r ≈ 1.0) nor random noise (r ≈ 0).  The week‑ahead Pearson correlations below quantify that balance.  Values around 0.7–0.9 show that participants generally keep their rank order from one week to the next (so autoregressive paths are meaningful) while still leaving 30–50 % of the week‑to‑week variance unexplained (so cross‑lagged effects have room to operate).

| Variable              | Lag‑one correlation r |
| --------------------- | :-------------------: |
| Sleep efficiency mean |        **0.76**       |
| Total sleep time mean |        **0.71**       |
| WASO mean             |        **0.67**       |
| Awakenings mean       |        **0.81**       |
| KOOS Pain             |        **0.86**       |
| WOMAC Pain            |        **0.82**       |
| WOMAC Function        |        **0.89**       |
| PGA‑OA                |        **0.76**       |

A correlation of 0.76, for example, means that 58 % of each week’s variance (0.76²) is predictable from the prior week—evidence of continuity—while 42 % is new, week‑specific fluctuation that a cross‑lagged predictor could potentially explain.  Fragmentation‑index means (r ≈ 0.35) fall outside this ideal zone and are excluded from primary models.

**3  Local stationarity over the analysis window.**  Grand‑mean plots for weeks 18‑23 show …  Local stationarity over the analysis window.\*\*  Grand‑mean plots for weeks 18‑23 show the sample average oscillates but does not trend up or down: ± 2 percentage points for sleep efficiency and ± 3 points for KOOS Pain, both < 0.2 SD.  This visual evidence supports the equality constraints that will be imposed on autoregressive and cross‑lagged paths.  Fit indices (χ², CFI, RMSEA) will formally test that assumption.


<img width="816" height="558" alt="image" src="https://github.com/user-attachments/assets/dd3075c5-518d-4961-bc67-954db9881ac0" />

<img width="816" height="558" alt="image" src="https://github.com/user-attachments/assets/8feae82a-fc08-43dd-b047-5d7057f90270" />


**4  Sufficient information for reliable maximum‑likelihood estimation.**  With equality constraints the six‑wave CLPM requires fewer than ten free parameters.  Forty participants × six waves provides an observations‑to‑parameters ratio comfortably exceeding the conventional 10:1 rule of thumb.  Mode columns (>80 % missing) are excluded; remaining missingness is intermittent and handled by FIML.

Together these points satisfy the standards laid out by Selig & Little (2012): evenly spaced waves, observable yet controlled change, approximate stationarity, and an estimable likelihood surface.

## Recommended modelling strategy

* **Primary analysis:** Equality‑constrained six‑wave CLPM (weeks 18‑23) pairing one sleep metric (`efficiency_mean`, `tst_mean`, `waso_mean`, or `number_of_awakenings_mean`) with one pain metric (`koos_pain`, `kd_womac_pain`, or `kd_womac_func`).  This window maximises participant overlap.
* **Robustness check:** Repeat the same specification on a four‑wave block (weeks 19‑22) and compare standardised cross‑lagged coefficients.
* **Extended window:** Fit an eleven‑wave *random‑intercept* CLPM on person‑indexed waves 1–11 to exploit each participant’s full run, acknowledging larger standard errors.
* **Supplementary analysis:** Mixed‑effects distributed‑lag regression (long format) to test 2‑ or 3‑week lag effects without wide reshaping.

These steps balance statistical power, interpretability and efficient use of the data’s longer individual series.

---

### Alignment with Selig & Little (2012)

The paper outlines four modelling principles: (1) include autoregressive paths to capture rank‑order stability; (2) estimate cross‑lagged paths in both directions to test potential causal ordering; (3) allow within‑wave residual covariances so lagged effects are not inflated by concurrent correlations; and (4) impose equality constraints across waves to enforce stationarity unless theory dictates otherwise.  All four principles are incorporated here: autoregressive stability was confirmed empirically (AR ≈ 0.6–0.9), bidirectional cross‑lagged paths will be estimated, residual covariances are retained in every proposed specification, and equality constraints are recommended for six‑wave and eleven‑wave models.  Hence the analysis plan and terminology align directly with Selig & Little’s guidance.
