import pandas as pd, matplotlib.pyplot as plt, os

p1 = "reports/exports/tci_rolling.csv"         # sortie step_model()
p2 = "reports/exports_var/var_rolling_tci.csv" # fallback ancien modÃ¨le

csv = p1 if os.path.exists(p1) else p2
if not os.path.exists(csv):
    raise FileNotFoundError(f"Fichier TCI introuvable : {csv}")

df = pd.read_csv(csv, parse_dates=["date_end"], dayfirst=False)
df = df.sort_values("date_end")

plt.figure(figsize=(11,4))
plt.plot(df["date_end"], df["TCI"], label="TCI (VAR-FEVD)")
plt.title("VAR Total Connectedness Index")
plt.legend(); plt.tight_layout()

os.makedirs("figures_var", exist_ok=True)
plt.savefig("figures_var/rolling_tci.png", dpi=130)
print(f"[OK] {out}")
