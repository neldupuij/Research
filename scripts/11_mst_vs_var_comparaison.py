# ==============================================================================
# 11_mst_vs_var_comparison.py
# Comparaison formelle entre centralité MST et net spillover VAR-DY.
# Répond au reviewer 2 : "empirical comparison between centrality (MST)
# and net spillovers (VAR) could be useful".
#
# Entrées :
#   outputs/mst_global/mat/mst_node_degree_global.csv  (degré MST par ticker)
#   outputs/var_dy/mat/DY_net.csv                      (net spillover rolling)
#   outputs/dcc_garch/mat/dcc_eigen_centrality.csv     (centralité DCC)
#
# Sorties (outputs/comparison_var_dcc/) :
#   mat/mst_vs_var_net_merged.csv
#   fig/mst_degree_vs_var_net_scatter.png
#   fig/dcc_eigen_vs_var_net_scatter.png
#   summary/spearman_correlations.csv
# ==============================================================================

import os
import warnings
warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from scipy import stats
import yaml

# ==============================================================================
# CONFIG
# ==============================================================================
with open("config/project.yml", "r") as f:
    cfg = yaml.safe_load(f)

paths = cfg.get("paths", {})

MST_DEG_PATH  = os.path.join(paths.get("out_mst",     "outputs/mst_global"),
                              "mat", "mst_node_degree_global.csv")
VAR_NET_PATH  = os.path.join(paths.get("out_var_dy",  "outputs/var_dy"),
                              "mat", "DY_net.csv")
DCC_EIG_PATH  = os.path.join(paths.get("out_dcc",     "outputs/dcc_garch"),
                              "mat", "dcc_eigen_centrality.csv")

OUT_DIR = paths.get("out_compare", "outputs/comparison_var_dcc")
FIG_DIR = os.path.join(OUT_DIR, "fig")
MAT_DIR = os.path.join(OUT_DIR, "mat")
SUM_DIR = os.path.join(OUT_DIR, "summary")
for d in [FIG_DIR, MAT_DIR, SUM_DIR]:
    os.makedirs(d, exist_ok=True)

# ==============================================================================
# CORE — chargement et préparation des données
# ==============================================================================
if not os.path.exists(MST_DEG_PATH):
    raise FileNotFoundError(f"Manque : {MST_DEG_PATH} (lancer 08_mst_network.py)")
if not os.path.exists(VAR_NET_PATH):
    raise FileNotFoundError(f"Manque : {VAR_NET_PATH} (lancer 04_var_dy_connectedness.R)")

# Degré MST global (une valeur par ticker, full-sample)
mst_deg = pd.read_csv(MST_DEG_PATH)
mst_deg.columns = mst_deg.columns.str.lower()
mst_deg = mst_deg.rename(columns={"ticker": "ticker", "degree": "mst_degree"})
mst_deg["ticker"] = mst_deg["ticker"].astype(str)

# Net spillover VAR-DY : moyenne temporelle par institution
var_net = pd.read_csv(VAR_NET_PATH)

# Détecter colonne date
date_col = next((c for c in var_net.columns if c.lower() in ["date", "end"]), None)
if date_col:
    var_net = var_net.drop(columns=[date_col])

# Moyenne temporelle de chaque institution (colonnes = tickers)
var_net_mean = var_net.mean(axis=0).reset_index()
var_net_mean.columns = ["ticker", "var_net_mean"]
var_net_mean["ticker"] = var_net_mean["ticker"].astype(str)

# ==============================================================================
# CORE — fusion MST degree + VAR net
# ==============================================================================
merged = pd.merge(mst_deg[["ticker", "mst_degree"]],
                  var_net_mean,
                  on="ticker", how="inner")

print(f"  MST degree    : {len(mst_deg)} tickers")
print(f"  VAR net (moy) : {len(var_net_mean)} tickers")
print(f"  Intersection  : {len(merged)} tickers")

merged.to_csv(os.path.join(MAT_DIR, "mst_vs_var_net_merged.csv"), index=False)

# ==============================================================================
# CORE — corrélation de rang Spearman MST degree ↔ VAR net
# ==============================================================================
results = []

if len(merged) >= 5:
    rho1, pval1 = stats.spearmanr(merged["mst_degree"], merged["var_net_mean"])
    results.append({
        "comparison":    "MST degree vs VAR net spillover",
        "n":             len(merged),
        "spearman_rho":  round(float(rho1), 4),
        "p_value":       round(float(pval1), 4),
        "significant_5pct": pval1 < 0.05
    })
    print(f"  Spearman MST degree ↔ VAR net : rho={rho1:.3f} | p={pval1:.4f}")

# ==============================================================================
# PLOTS — scatter MST degree vs VAR net spillover
# ==============================================================================
fig, ax = plt.subplots(figsize=(8, 6))
ax.scatter(merged["mst_degree"], merged["var_net_mean"],
           s=60, alpha=0.7, edgecolors="k", linewidths=0.4)

for _, row in merged.iterrows():
    ax.annotate(row["ticker"],
                (row["mst_degree"], row["var_net_mean"]),
                fontsize=6, alpha=0.7,
                xytext=(3, 3), textcoords="offset points")

if len(merged) >= 5:
    z = np.polyfit(merged["mst_degree"], merged["var_net_mean"], 1)
    xr = np.linspace(merged["mst_degree"].min(), merged["mst_degree"].max(), 100)
    ax.plot(xr, np.polyval(z, xr), "r--", lw=1.5,
            label=f"OLS (rho Spearman={rho1:.2f}, p={pval1:.3f})")
    ax.legend(fontsize=9)

ax.set_xlabel("MST degree (full-sample)", fontsize=11)
ax.set_ylabel("Average net spillover VAR-DY (TO − FROM)", fontsize=11)
ax.set_title("MST centrality vs VAR-DY net spillover\nCross-sectional comparison",
             fontsize=12)
ax.grid(alpha=0.3)
plt.tight_layout()
plt.savefig(os.path.join(FIG_DIR, "mst_degree_vs_var_net_scatter.png"), dpi=250)
plt.close()
print("  [OK] mst_degree_vs_var_net_scatter.png")

# ==============================================================================
# CORE + PLOTS — centralité eigenvector DCC vs VAR net (si disponible)
# ==============================================================================
if os.path.exists(DCC_EIG_PATH):
    dcc_eig = pd.read_csv(DCC_EIG_PATH)
    if {"ticker", "eigen"}.issubset(dcc_eig.columns):
        dcc_mean = (dcc_eig.groupby("ticker", as_index=False)["eigen"]
                    .mean()
                    .rename(columns={"eigen": "dcc_eigen_mean"}))
        dcc_mean["ticker"] = dcc_mean["ticker"].astype(str)

        merged2 = pd.merge(dcc_mean, var_net_mean, on="ticker", how="inner")
        print(f"  DCC eigen ↔ VAR net : {len(merged2)} tickers communs")

        if len(merged2) >= 5:
            rho2, pval2 = stats.spearmanr(merged2["dcc_eigen_mean"],
                                           merged2["var_net_mean"])
            results.append({
                "comparison":    "DCC eigen centrality vs VAR net spillover",
                "n":             len(merged2),
                "spearman_rho":  round(float(rho2), 4),
                "p_value":       round(float(pval2), 4),
                "significant_5pct": pval2 < 0.05
            })
            print(f"  Spearman DCC eigen ↔ VAR net : rho={rho2:.3f} | p={pval2:.4f}")

            fig2, ax2 = plt.subplots(figsize=(8, 6))
            ax2.scatter(merged2["dcc_eigen_mean"], merged2["var_net_mean"],
                        s=60, alpha=0.7, edgecolors="k", linewidths=0.4)
            for _, row in merged2.iterrows():
                ax2.annotate(row["ticker"],
                             (row["dcc_eigen_mean"], row["var_net_mean"]),
                             fontsize=6, alpha=0.7,
                             xytext=(3, 3), textcoords="offset points")
            z2 = np.polyfit(merged2["dcc_eigen_mean"], merged2["var_net_mean"], 1)
            xr2 = np.linspace(merged2["dcc_eigen_mean"].min(),
                               merged2["dcc_eigen_mean"].max(), 100)
            ax2.plot(xr2, np.polyval(z2, xr2), "r--", lw=1.5,
                     label=f"OLS (rho Spearman={rho2:.2f}, p={pval2:.3f})")
            ax2.legend(fontsize=9)
            ax2.set_xlabel("DCC eigenvector centrality (time average)", fontsize=11)
            ax2.set_ylabel("Average net spillover VAR-DY (TO − FROM)", fontsize=11)
            ax2.set_title("DCC centrality vs VAR-DY net spillover\nCross-sectional comparison",
                          fontsize=12)
            ax2.grid(alpha=0.3)
            plt.tight_layout()
            plt.savefig(os.path.join(FIG_DIR, "dcc_eigen_vs_var_net_scatter.png"), dpi=250)
            plt.close()
            print("  [OK] dcc_eigen_vs_var_net_scatter.png")

# ==============================================================================
# EXPORT — tableau Spearman
# ==============================================================================
if results:
    df_res = pd.DataFrame(results)
    df_res.to_csv(os.path.join(SUM_DIR, "spearman_correlations.csv"), index=False)
    print("  [OK] spearman_correlations.csv")

print("11_mst_vs_var_comparison.py terminé.")
