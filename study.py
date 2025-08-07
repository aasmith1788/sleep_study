# -------------------------------------------------------------------
# Bidirectional weekly dynamics of sleep efficiency and knee pain
# This cell installs packages, ingests the CSV, prepares the panel,
# and estimates frequentist and Bayesian cross-lagged models.
# -------------------------------------------------------------------

# 0. install required libraries (run only once per environment)
import sys, subprocess, importlib
def install(pkg):
    if importlib.util.find_spec(pkg) is None:
        subprocess.check_call([sys.executable, "-m", "pip", "install", "-q", pkg])

for p in ["pandas", "numpy", "statsmodels", "bambi", "pymc", "arviz", "matplotlib"]:
    install(p)

# 1. imports
import warnings, pandas as pd, numpy as np, statsmodels.formula.api as smf
import bambi as bmb, arviz as az, matplotlib.pyplot as plt
warnings.filterwarnings("ignore", category=FutureWarning)

# 2. file location (adjust if you renamed or moved it)
DATA_PATH = "C:\\Users\\aasmi\\Downloads\\gt9x_sleep_dsis_koos_pgaoa_weekly (1).csv"

# 3. read and inspect headers
df_raw = pd.read_csv(DATA_PATH)
print("Columns found:", list(df_raw.columns))

# 4. standardise expected column names
rename_map = {}
if "redcap_event_name" in df_raw.columns and "redcap_event" not in df_raw.columns:
    rename_map["redcap_event_name"] = "redcap_event"
if rename_map:
    df_raw = df_raw.rename(columns=rename_map)

needed = ["record_id", "redcap_event", "Efficiency_Mean", "koos_pain"]
df = df_raw.loc[:, needed].copy()

# 5. de-duplicate accidental repeats by averaging numeric values
df = (df
      .groupby(["record_id", "redcap_event"], as_index=False)
      .agg({"Efficiency_Mean": "mean", "koos_pain": "mean"}))

# 6. sort and create centred variables and one-week lags
df = df.sort_values(["record_id", "redcap_event"]).reset_index(drop=True)
df["sleep_c"] = df.groupby("record_id")["Efficiency_Mean"].transform(lambda x: x - x.mean())
df["pain_c"]  = df.groupby("record_id")["koos_pain"].transform(lambda x: x - x.mean())
df["sleep_c_lag"] = df.groupby("record_id")["sleep_c"].shift(1)
df["pain_c_lag"]  = df.groupby("record_id")["pain_c"].shift(1)
d = df.dropna(subset=["sleep_c", "pain_c", "sleep_c_lag", "pain_c_lag"]).copy()

print(f"\nAfter cleaning: {d.shape[0]} person-weeks from {d['record_id'].nunique()} participants")

# 7. frequentist mixed-effects cross-lag
sleep_eq = "sleep_c ~ sleep_c_lag + pain_c_lag"
pain_eq  = "pain_c  ~ pain_c_lag + sleep_c_lag"
sleep_ml = smf.mixedlm(sleep_eq, d, groups=d["record_id"]).fit(reml=False)
pain_ml  = smf.mixedlm(pain_eq,  d, groups=d["record_id"]).fit(reml=False)

print("\n=== Maximum-likelihood mixed-effects results ===\n")
print(sleep_ml.summary())
print("\n")
print(pain_ml.summary())

# 8. Bayesian Dynamic SEM via Bambi / PyMC
priors = {
    "Intercept"   : bmb.Prior("Normal", mu=0, sigma=1),
    "sleep_c_lag" : bmb.Prior("Normal", mu=0, sigma=0.5),
    "pain_c_lag"  : bmb.Prior("Normal", mu=0, sigma=0.5),
    "Group SD"    : bmb.Prior("HalfCauchy", beta=1),
    "Sigma"       : bmb.Prior("HalfCauchy", beta=1),
}

print("\nRunning Bayesian models; this will take a couple of minutes…")
mod_sleep = bmb.Model(sleep_eq + " + (1|record_id)", d, priors=priors)
idata_slp = mod_sleep.fit(draws=2000, tune=2000, chains=4, target_accept=0.9, progressbar=False)

mod_pain  = bmb.Model(pain_eq  + " + (1|record_id)", d, priors=priors)
idata_pai = mod_pain.fit(draws=2000, tune=2000, chains=4, target_accept=0.9, progressbar=False)

print("\n=== Bayesian posterior summaries (means and 95% HDIs) ===\n")
print("Sleep equation")
print(az.summary(idata_slp, var_names=["sleep_c_lag", "pain_c_lag"], hdi_prob=0.95))
print("\nPain equation")
print(az.summary(idata_pai, var_names=["pain_c_lag", "sleep_c_lag"], hdi_prob=0.95))

# 9. quick posterior-predictive checks
fig, axes = plt.subplots(1, 2, figsize=(12, 4))
az.plot_ppc(idata_slp, var_names=["sleep_c"], num_pp_samples=500, ax=axes[0])
axes[0].set_title("PPC: Sleep equation")
az.plot_ppc(idata_pai, var_names=["pain_c"],  num_pp_samples=500, ax=axes[1])
axes[1].set_title("PPC: Pain equation")
plt.tight_layout(); plt.show()
