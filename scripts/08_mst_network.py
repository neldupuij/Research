# =============================================================================
# 08_mst_network.py
# Construit les réseaux MST (global + rolling) à partir des log-returns
# hebdomadaires. Exporte les matrices de corrélation et covariance filtrées
# prêtes pour la construction de portefeuille.
#
# Produit :
#   outputs/mst_global/mat/corr_weekly_global.csv
#   outputs/mst_global/mat/dist_weekly_global.csv
#   outputs/mst_global/mat/mst_edges_global.csv
#   outputs/mst_global/mat/mst_node_degree_global.csv
#   outputs/mst_global/fig/corr_heatmap_global.png
#   outputs/mst_global/fig/mst_network_global.png
#   outputs/mst_rolling/mat/mst_rolling_metrics.csv
#   outputs/mst_rolling/mat/mst_edges_rolling_long.csv
#   outputs/mst_rolling/mat/sample_corr_rolling_long.csv
#   outputs/mst_rolling/mat/sample_cov_rolling_long.csv
#   outputs/mst_portfolio/mat/mst_corr_rolling_long.csv
#   outputs/mst_portfolio/mat/mst_cov_rolling_long.csv
#   outputs/mst_rolling/mat/compare_mst_dy_dcc.csv  (si DY + DCC disponibles)
# =============================================================================

import os
import warnings
warnings.filterwarnings("ignore")

import yaml
import numpy as np
import pandas as pd
import networkx as nx
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates

# =============================================================================
# CONFIG
# =============================================================================
with open(os.path.join("config", "project.yml"), encoding="utf-8") as f:
    cfg = yaml.safe_load(f)

paths   = cfg.get("paths", {})
var_dy  = cfg.get("var_dy", {})

RET_PATH  = paths.get("returns_weekly", "data/processed/returns_weekly.csv")
DY_TCI    = os.path.join(paths.get("out_var_dy",  "outputs/var_dy"),   "mat", "DY_tci.csv")
DCC_PATH  = os.path.join(paths.get("out_dcc",     "outputs/dcc_garch"), "mat", "dcc_meancorr_rolling.csv")

OUT_GLOBAL  = paths.get("out_mst_global",  "outputs/mst_global")
OUT_ROLLING = paths.get("out_mst_rolling", "outputs/mst_rolling")
OUT_PORT    = paths.get("out_mst_port",    "outputs/mst_portfolio")

for d in [OUT_GLOBAL, OUT_ROLLING, OUT_PORT]:
    os.makedirs(os.path.join(d, "mat"), exist_ok=True)
    os.makedirs(os.path.join(d, "fig"), exist_ok=True)

W_SIZE      = int(var_dy.get("rolling_window", 156))
W_STEP      = int(var_dy.get("rolling_step",   4))
MIN_PERIODS = 30

# =============================================================================
# HELPERS
# =============================================================================
def corr_to_dist(corr: pd.DataFrame) -> pd.DataFrame:
    c    = corr.clip(-1, 1)
    dist = np.sqrt(2.0 * (1.0 - c))
    np.fill_diagonal(dist.values, 0.0)
    return dist

def build_mst(corr: pd.DataFrame):
    tickers = list(corr.columns)
    dist    = corr_to_dist(corr)
    G       = nx.Graph()
    G.add_nodes_from(tickers)
    vals, cvals = dist.values, corr.values
    n = len(tickers)
    for i in range(n):
        for j in range(i + 1, n):
            d, rho = vals[i, j], cvals[i, j]
            if np.isfinite(d) and np.isfinite(rho):
                G.add_edge(tickers[i], tickers[j], weight=float(d), corr=float(rho))
    MST = nx.minimum_spanning_tree(G, weight="weight")
    edges = [{"u": u, "v": v,
               "distance": float(data["weight"]),
               "corr":     float(data["corr"])}
             for u, v, data in MST.edges(data=True)]
    return MST, pd.DataFrame(edges)

def ensure_psd(mat: np.ndarray, eps: float = 1e-8) -> np.ndarray:
    A    = 0.5 * (mat + mat.T)
    w, V = np.linalg.eigh(A)
    w    = np.maximum(w, eps)
    return (V * w) @ V.T

def longify(date, tickers, M, name="value"):
    dfm = pd.DataFrame(M, index=tickers, columns=tickers)
    out = dfm.stack().reset_index()
    out.columns = ["i", "j", name]
    out.insert(0, "date", date)
    return out

# =============================================================================
# CHARGEMENT DES RETURNS
# =============================================================================
if not os.path.exists(RET_PATH):
    raise FileNotFoundError(f"Introuvable : {RET_PATH}")

df = pd.read_csv(RET_PATH)
df["date"] = pd.to_datetime(df["date"], errors="coerce")
df = df.dropna(subset=["date"]).sort_values("date").reset_index(drop=True)

tickers_all = [c for c in df.columns if c != "date"]
if len(tickers_all) < 3:
    raise ValueError("Pas assez de colonnes tickers (>= 3 requis).")

R_all = df[tickers_all].apply(pd.to_numeric, errors="coerce")
print(f"Returns chargés : N={len(df)} | K={len(tickers_all)}")

# =============================================================================
# (A) MST GLOBAL (plein échantillon)
# =============================================================================
corr_global = R_all.corr(method="pearson", min_periods=MIN_PERIODS)
dist_global = corr_to_dist(corr_global)

gmat_dir = os.path.join(OUT_GLOBAL, "mat")
corr_global.to_csv(os.path.join(gmat_dir, "corr_weekly_global.csv"))
dist_global.to_csv(os.path.join(gmat_dir, "dist_weekly_global.csv"))

MST_global, edges_global = build_mst(corr_global)
edges_global.sort_values(["distance", "u", "v"]).to_csv(
    os.path.join(gmat_dir, "mst_edges_global.csv"), index=False)

deg     = dict(MST_global.degree())
deg_df  = (pd.DataFrame({"ticker": list(deg.keys()), "degree": list(deg.values())})
           .sort_values("degree", ascending=False))
deg_df.to_csv(os.path.join(gmat_dir, "mst_node_degree_global.csv"), index=False)

print(f"MST global : {len(edges_global)} edges | hub = {deg_df.iloc[0]['ticker']}")

# --- Figure : heatmap corrélation globale
gfig_dir = os.path.join(OUT_GLOBAL, "fig")
fig, ax = plt.subplots(figsize=(10, 8))
im = ax.imshow(corr_global.values, aspect="auto", vmin=-1, vmax=1)
ax.set_title("Pearson correlation matrix, weekly returns (global)")
plt.colorbar(im, ax=ax, label="corr")
ax.set_xticks(range(len(corr_global.columns)))
ax.set_xticklabels(corr_global.columns, rotation=90, fontsize=6)
ax.set_yticks(range(len(corr_global.columns)))
ax.set_yticklabels(corr_global.columns, fontsize=6)
plt.tight_layout()
plt.savefig(os.path.join(gfig_dir, "corr_heatmap_global.png"), dpi=250)
plt.close()

# --- Figure : réseau MST global
fig, ax = plt.subplots(figsize=(12, 9))
pos = nx.spring_layout(MST_global, seed=42,
                       k=1.0 / np.sqrt(max(1, len(MST_global.nodes()))))
node_sizes = [80 + 60 * deg[n] for n in MST_global.nodes()]
nx.draw_networkx_edges(MST_global, pos, ax=ax, width=1.2, alpha=0.7)
nx.draw_networkx_nodes(MST_global, pos, ax=ax, node_size=node_sizes, alpha=0.9)
nx.draw_networkx_labels(MST_global, pos, ax=ax, font_size=7)
ax.set_title("Global MST: distance = sqrt(2*(1-corr)), weekly returns")
ax.axis("off")
plt.tight_layout()
plt.savefig(os.path.join(gfig_dir, "mst_network_global.png"), dpi=250)
plt.close()

# =============================================================================
# (B) MST ROLLING
# =============================================================================
N        = len(df)
ends_idx = list(range(W_SIZE - 1, N, W_STEP))

rows_metrics  = []
all_edges     = []
all_corr      = []
all_cov       = []
all_mst_corr  = []
all_mst_cov   = []

for e in ends_idx:
    s        = e - W_SIZE + 1
    date_end = df.loc[e, "date"]
    sub      = R_all.iloc[s:e + 1].copy()

    good = sub.notna().sum(axis=0) >= MIN_PERIODS
    sub  = sub.loc[:, good.values]
    if sub.shape[1] < 3:
        continue

    tickers  = list(sub.columns)
    corr_w   = sub.corr(method="pearson", min_periods=MIN_PERIODS)
    cov_w    = sub.cov(min_periods=MIN_PERIODS)

    if corr_w.shape[0] < 3:
        continue

    MST_w, edges_w = build_mst(corr_w)

    mst_corr = pd.DataFrame(0.0, index=tickers, columns=tickers)
    np.fill_diagonal(mst_corr.values, 1.0)
    for _, row in edges_w.iterrows():
        u, v, rho = row["u"], row["v"], row["corr"]
        if np.isfinite(rho):
            mst_corr.loc[u, v] = rho
            mst_corr.loc[v, u] = rho

    vol     = np.sqrt(np.maximum(np.diag(cov_w.loc[tickers, tickers].values), 0.0))
    D       = np.diag(vol)
    mst_cov = ensure_psd(D @ mst_corr.values @ D)

    deg_w = dict(MST_w.degree())
    hub   = max(deg_w, key=deg_w.get) if deg_w else None

    rows_metrics.append({
        "date":          date_end,
        "k_used":        len(tickers),
        "mst_mean_corr": float(edges_w["corr"].mean())     if len(edges_w) else np.nan,
        "mst_mean_dist": float(edges_w["distance"].mean()) if len(edges_w) else np.nan,
        "mst_total_len": float(edges_w["distance"].sum())  if len(edges_w) else np.nan,
        "mst_hub":       hub,
    })

    tmp = edges_w.copy(); tmp.insert(0, "date", date_end)
    all_edges.append(tmp)
    all_corr.append(longify(date_end, tickers,
                            corr_w.loc[tickers, tickers].values, "corr"))
    all_cov.append(longify(date_end, tickers,
                           cov_w.loc[tickers, tickers].values, "cov"))
    all_mst_corr.append(longify(date_end, tickers, mst_corr.values, "mst_corr"))
    all_mst_cov.append(longify(date_end, tickers, mst_cov, "mst_cov"))

roll = (pd.DataFrame(rows_metrics)
        .dropna(subset=["date"]).sort_values("date").reset_index(drop=True))

rmat = os.path.join(OUT_ROLLING, "mat")
pmat = os.path.join(OUT_PORT,    "mat")

roll.to_csv(os.path.join(rmat, "mst_rolling_metrics.csv"), index=False)
if all_edges:
    pd.concat(all_edges, ignore_index=True).to_csv(
        os.path.join(rmat, "mst_edges_rolling_long.csv"), index=False)
if all_corr:
    pd.concat(all_corr, ignore_index=True).to_csv(
        os.path.join(rmat, "sample_corr_rolling_long.csv"), index=False)
if all_cov:
    pd.concat(all_cov, ignore_index=True).to_csv(
        os.path.join(rmat, "sample_cov_rolling_long.csv"), index=False)
if all_mst_corr:
    pd.concat(all_mst_corr, ignore_index=True).to_csv(
        os.path.join(pmat, "mst_corr_rolling_long.csv"), index=False)
if all_mst_cov:
    pd.concat(all_mst_cov, ignore_index=True).to_csv(
        os.path.join(pmat, "mst_cov_rolling_long.csv"), index=False)

print(f"Rolling MST : {len(roll)} fenêtres exportées.")

# =============================================================================
# (C) COMPARAISON OPTIONNELLE MST / DY / DCC
# =============================================================================
if os.path.exists(DY_TCI) and os.path.exists(DCC_PATH) and len(roll):
    dy  = pd.read_csv(DY_TCI)
    if "TCI" in dy.columns and "tci" not in dy.columns:
        dy = dy.rename(columns={"TCI": "tci"})
    dy["date"] = pd.to_datetime(dy["date"], errors="coerce")
    dy["tci"]  = pd.to_numeric(dy["tci"],  errors="coerce")
    dy = dy.dropna(subset=["date", "tci"]).sort_values("date").reset_index(drop=True)

    dcc = pd.read_csv(DCC_PATH).rename(columns={"end": "date"})
    dcc["date"]        = pd.to_datetime(dcc["date"],        errors="coerce")
    dcc["dcc_meancorr"] = pd.to_numeric(dcc["dcc_meancorr"], errors="coerce")
    dcc = dcc.dropna(subset=["date", "dcc_meancorr"]).sort_values("date").reset_index(drop=True)
    dcc["dcc_pct"] = 100.0 * dcc["dcc_meancorr"]

    roll2 = roll.copy()
    roll2["mst_pct"] = 100.0 * pd.to_numeric(roll2["mst_mean_corr"], errors="coerce")

    m1 = pd.merge_asof(roll2.sort_values("date"), dy[["date", "tci"]],
                       on="date", direction="nearest",
                       tolerance=pd.Timedelta(days=10))
    m2 = pd.merge_asof(m1, dcc[["date", "dcc_pct"]],
                       on="date", direction="nearest",
                       tolerance=pd.Timedelta(days=10))
    compare = m2.dropna(subset=["mst_pct", "tci", "dcc_pct"]).reset_index(drop=True)
    compare.to_csv(os.path.join(rmat, "compare_mst_dy_dcc.csv"), index=False)
    print(f"Comparaison MST/DY/DCC exportée : {len(compare)} points.")

print("Done.")
