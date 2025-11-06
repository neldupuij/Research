import os, pandas as pd, numpy as np, matplotlib.pyplot as plt
os.makedirs("reports/exports_var", exist_ok=True)
os.makedirs("figures_var", exist_ok=True)

X = pd.read_csv("data/processed/returns_panel.csv", parse_dates=["date"])
W = (X.pivot(index="date", columns="symbol", values="return")
       .sort_index().dropna(axis=1, how="any"))
C = W.corr()
C.to_csv("reports/exports_var/corr_full.csv", index=True)

fig = plt.figure(figsize=(7,6))
plt.imshow(C.values, aspect="auto")
plt.title("Full-sample correlation"); plt.tight_layout()
fig.savefig("figures_var/corr_full.png", dpi=160); plt.close(fig)
print("[OK] corr_full -> CSV+PNG")
