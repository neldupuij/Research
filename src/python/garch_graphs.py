import glob, os, numpy as np, pandas as pd, matplotlib.pyplot as plt
os.makedirs("figures_garch", exist_ok=True)
files = sorted(glob.glob("reports/exports_garch/dcc_rolling_corr_*.csv"))
rows=[]
for f in files:
    base=os.path.basename(f)[:-4]; d0,d1=base.split("_")[-2:]
    end=pd.to_datetime(d1,format="%Y%m%d")
    C=pd.read_csv(f)
    if "rowname" in C.columns: C=C.set_index("rowname")
    v=np.abs(C.values.astype(float)); np.fill_diagonal(v, np.nan)
    rows.append({"end":end,"mean_abs_corr":np.nanmean(v)})
df=pd.DataFrame(rows).sort_values("end")
fig = plt.figure(figsize=(9,4))
plt.plot(df["end"], df["mean_abs_corr"], label="DCC mean |corr|")
plt.legend(); plt.title("DCC rolling mean |corr|"); plt.tight_layout()
fig.savefig("figures_garch/meancorr_over_time.png", dpi=200); plt.close(fig)
print("[OK] figures_garch/meancorr_over_time.png")
