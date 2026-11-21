# =============================================================================
# 10_plot_country_network.py
# Visualisation du réseau de corrélation entre TCI pays.
# Lit cluster_tci_rolling.csv (produit par 07_cluster_tci_by_country.R).
# Produit :
#   outputs/cluster_regimes/fig/tci_clusters_grid.png
#   outputs/cluster_regimes/fig/tci_country_correlation_heatmap.png
#   outputs/cluster_regimes/fig/tci_country_corr_network.png
#   outputs/cluster_regimes/fig/tci_country_correlation.csv
#   outputs/cluster_regimes/fig/tci_country_network_edges.csv
# =============================================================================

import os
import warnings
warnings.filterwarnings("ignore")

import yaml
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib as mpl
from matplotlib.patches import FancyArrowPatch

try:
    import networkx as nx
except ImportError:
    raise ImportError("pip install networkx")

# =============================================================================
# CONFIG
# =============================================================================
with open(os.path.join("config", "project.yml"), encoding="utf-8") as f:
    cfg = yaml.safe_load(f)

paths    = cfg.get("paths", {})
out_base = paths.get("out_cluster", "outputs/cluster_regimes")

INPUT_PATH = os.path.join(out_base, "mat", "cluster_tci_rolling.csv")
out_dir    = os.path.join(out_base, "fig")
os.makedirs(out_dir, exist_ok=True)

if not os.path.exists(INPUT_PATH):
    raise FileNotFoundError(
        f"Introuvable : {INPUT_PATH} — lance 07_cluster_tci_by_country.R d'abord.")

# =============================================================================
# CHARGEMENT + PIVOT
# =============================================================================
df = pd.read_csv(INPUT_PATH)
df["date"] = pd.to_datetime(df["date"])
df = df.sort_values(["cluster", "date"])

pivot    = df.pivot(index="date", columns="cluster", values="TCI").sort_index()
pivot    = pivot.sort_index(axis=1)
countries = list(pivot.columns)
n         = len(countries)

# =============================================================================
# FIGURE 1 : GRILLE TCI PAR PAYS
# =============================================================================
ncols = 4
nrows = int(np.ceil(n / ncols))
fig, axes = plt.subplots(nrows=nrows, ncols=ncols,
                          figsize=(22, 16), sharex=True)
axes = axes.flatten()
yr_loc = mdates.YearLocator(base=2)
yr_fmt = mdates.DateFormatter("%Y")

for i, c in enumerate(countries):
    ax = axes[i]
    ax.plot(pivot.index, pivot[c], linewidth=1.2)
    ax.set_title(c, fontsize=10)
    ax.grid(True, alpha=0.3)
    ax.xaxis.set_major_locator(yr_loc)
    ax.xaxis.set_major_formatter(yr_fmt)
    ax.tick_params(axis="y", labelsize=7)
    row = i // ncols
    if row < nrows - 1:
        ax.tick_params(axis="x", labelbottom=False)
    else:
        ax.tick_params(axis="x", labelsize=8, rotation=45)

for j in range(n, len(axes)):
    fig.delaxes(axes[j])

fig.suptitle("TCI(t) by country: cluster-level connectedness (Diebold-Yilmaz)", fontsize=16)
fig.tight_layout(rect=[0, 0.03, 1, 0.97])
plt.savefig(os.path.join(out_dir, "tci_clusters_grid.png"), dpi=250)
plt.close(fig)
print("[OK] tci_clusters_grid.png")

# =============================================================================
# FIGURE 2 : HEATMAP CORRÉLATION ENTRE TCI PAYS
# =============================================================================
corr = pivot.corr()
corr.to_csv(os.path.join(out_dir, "tci_country_correlation.csv"), float_format="%.6f")

fig2, ax2 = plt.subplots(figsize=(10, 8))
im = ax2.imshow(corr.values, vmin=-1, vmax=1, aspect="equal")
ax2.set_title("Correlation of country-level TCI(t)", fontsize=14)
ax2.set_xticks(np.arange(n)); ax2.set_yticks(np.arange(n))
ax2.set_xticklabels(countries, rotation=90, fontsize=8)
ax2.set_yticklabels(countries, fontsize=8)
ax2.set_xticks(np.arange(-.5, n, 1), minor=True)
ax2.set_yticks(np.arange(-.5, n, 1), minor=True)
ax2.grid(which="minor", color="white", linestyle="-", linewidth=0.7)
ax2.tick_params(which="minor", bottom=False, left=False)
fig2.colorbar(im, ax=ax2, fraction=0.046, pad=0.04).set_label("Correlation", fontsize=10)
fig2.tight_layout()
plt.savefig(os.path.join(out_dir, "tci_country_correlation_heatmap.png"), dpi=300)
plt.close(fig2)
print("[OK] tci_country_correlation_heatmap.png")

# =============================================================================
# FIGURE 3 : RÉSEAU FORCE-DIRECTED (spring layout)
# =============================================================================
SEED         = 7
TOP_K_ABS    = 55
MIN_ABS_CORR = 0.35
NEG_MIN_ABS  = 0.20
NEG_QUOTA    = 12
SPRING_K     = 0.95
SPRING_ITERS = 600
NODE_MIN, NODE_MAX   = 350, 1600
EDGE_LW_MIN, EDGE_LW_MAX     = 0.25, 6.5
EDGE_ALPHA_MIN, EDGE_ALPHA_MAX = 0.02, 0.78
CURVE_MIN, CURVE_MAX = 0.08, 0.32
COMMUNITY_CMAP = mpl.cm.Set2
EDGE_CMAP      = mpl.cm.RdBu_r
EDGE_NORM      = mpl.colors.TwoSlopeNorm(vmin=-1, vcenter=0, vmax=1)

def _scale(x, xmin, xmax, a, b):
    if xmax <= xmin: return 0.5 * (a + b)
    return a + (x - xmin) / (xmax - xmin) * (b - a)

def _emph(x, p=1.8):
    return float(np.clip(x, 0, 1) ** p)

corr_mat = corr.copy()
np.fill_diagonal(corr_mat.values, 0.0)
countries_str = [str(c) for c in countries]
corr_mat.index   = corr_mat.index.astype(str)
corr_mat.columns = corr_mat.columns.astype(str)

edges_df = (corr_mat.stack().rename_axis(["u", "v"]).reset_index(name="w"))
edges_df["w"] = pd.to_numeric(edges_df["w"], errors="coerce")
edges_df = edges_df.dropna(subset=["w"])
edges_df = edges_df[edges_df["u"] < edges_df["v"]].copy()
edges_df = edges_df[edges_df["w"].abs() >= MIN_ABS_CORR].copy()

neg_df = (corr_mat.stack().rename_axis(["u", "v"]).reset_index(name="w"))
neg_df["w"] = pd.to_numeric(neg_df["w"], errors="coerce")
neg_df = neg_df.dropna(subset=["w"])
neg_df = neg_df[neg_df["u"] < neg_df["v"]].copy()
neg_df = neg_df[(neg_df["w"] < 0) & (neg_df["w"].abs() >= NEG_MIN_ABS)].copy()
neg_df = neg_df.sort_values("w", key=lambda s: s.abs(), ascending=False).head(NEG_QUOTA)

core_df   = edges_df.sort_values("w", key=lambda s: s.abs(), ascending=False).head(TOP_K_ABS)
edges_use = pd.concat([core_df, neg_df]).drop_duplicates(subset=["u", "v"], keep="first")

G = nx.Graph()
G.add_nodes_from(countries_str)
for _, r in edges_use.iterrows():
    G.add_edge(r["u"], r["v"], weight=float(r["w"]), absw=abs(float(r["w"])))

node_strength = corr_mat.abs().sum(axis=1).reindex(countries_str).fillna(0.0)
smin, smax = float(node_strength.min()), float(node_strength.max())
node_sizes = {nd: _scale(float(node_strength.get(nd, 0.0)), smin, smax,
                         NODE_MIN, NODE_MAX) for nd in G.nodes()}

try:
    import community as community_louvain
except ImportError:
    raise ImportError(
        "python-louvain is required for community detection.\n"
        "Install with: pip install python-louvain\n"
        "See requirements.txt for the pinned version."
    )

# Louvain community detection with fixed seed for reproducibility.
# SEED=7 is set above; results are stable across random restarts.
Gw = nx.Graph()
Gw.add_nodes_from(G.nodes())
for u, v, d in G.edges(data=True):
    Gw.add_edge(u, v, weight=abs(d["weight"]))
communities = community_louvain.best_partition(Gw, weight="weight", random_state=SEED)

uniq = sorted(set(communities.values()))
remap = {old: i for i, old in enumerate(uniq)}
communities = {k: remap[v] for k, v in communities.items()}
K = len(set(communities.values()))

# ── Louvain stability: 100 random restarts ───────────────────────────────────
from collections import Counter as _Counter

def _entropy(labels):
    counts = np.array(list(_Counter(labels).values()), dtype=float)
    p = counts / counts.sum()
    return float(-np.sum(p * np.log(p + 1e-12)))

def _nmi(a, b):
    n = len(a)
    pj = np.array(list(_Counter(zip(a, b)).values()), dtype=float) / n
    h_joint = float(-np.sum(pj * np.log(pj + 1e-12)))
    ha, hb = _entropy(a), _entropy(b)
    denom = (ha * hb) ** 0.5
    return (ha + hb - h_joint) / denom if denom > 1e-12 else 1.0

_ref = [communities.get(nd, 0) for nd in countries_str]
_nmi_scores = []
for _rs in range(100):
    _p = community_louvain.best_partition(Gw, weight="weight", random_state=_rs)
    _rm = {o: i for i, o in enumerate(sorted(set(_p.values())))}
    _nmi_scores.append(_nmi(_ref, [_rm[_p.get(nd, 0)] for nd in countries_str]))

_nmi_arr = np.array(_nmi_scores)
pd.DataFrame({
    "mean_nmi":   [round(float(_nmi_arr.mean()), 4)],
    "min_nmi":    [round(float(_nmi_arr.min()),  4)],
    "std_nmi":    [round(float(_nmi_arr.std()),  4)],
    "n_restarts": [100],
    "reference_seed": [SEED]
}).to_csv(os.path.join(out_dir, "louvain_stability.csv"), index=False)
print(f"[OK] Louvain stability: mean NMI = {_nmi_arr.mean():.4f}, "
      f"min = {_nmi_arr.min():.4f}  (100 restarts vs seed={SEED})")
# ─────────────────────────────────────────────────────────────────────────────

pos       = nx.spring_layout(G, seed=SEED, k=SPRING_K,
                              iterations=SPRING_ITERS, weight="absw")
edge_list = list(G.edges())
edge_w    = np.array([float(G[u][v]["weight"]) for u, v in edge_list])
edge_abs  = np.abs(edge_w)
abs_min, abs_max = float(edge_abs.min()), float(edge_abs.max())

def edge_style(w, aw):
    t    = (aw - abs_min) / (abs_max - abs_min + 1e-12)
    t2   = _emph(t, p=1.7)
    lw   = EDGE_LW_MIN   + (EDGE_LW_MAX   - EDGE_LW_MIN)   * t2
    alpha = EDGE_ALPHA_MIN + (EDGE_ALPHA_MAX - EDGE_ALPHA_MIN) * t2
    rad  = _scale(1 - t, 0, 1, CURVE_MIN, CURVE_MAX)
    return lw, alpha, rad, EDGE_CMAP(EDGE_NORM(w))

fig, ax = plt.subplots(figsize=(13, 10))
ax.set_axis_off()
ax.set_title(f"Country TCI(t) correlation network, spring layout (top {len(edges_use)} edges)",
             fontsize=13)

for cid in range(K):
    nodes_c = [nd for nd in G.nodes() if communities.get(nd, 0) == cid]
    if not nodes_c: continue
    col   = COMMUNITY_CMAP(cid % COMMUNITY_CMAP.N)
    sizes = [node_sizes[nd] for nd in nodes_c]
    nx.draw_networkx_nodes(G, pos, nodelist=nodes_c, node_size=sizes,
                           node_color=[col] * len(nodes_c), alpha=0.96,
                           linewidths=1.2, edgecolors="black", ax=ax)

pos_idx = np.where(edge_w >= 0)[0]
neg_idx = np.where(edge_w < 0)[0]

def draw_edge(i, z):
    u, v = edge_list[i]
    w, aw = float(G[u][v]["weight"]), abs(float(G[u][v]["weight"]))
    lw, alpha, rad, color = edge_style(w, aw)
    sgn = -1 if (hash(u + v) % 2 == 0) else 1
    ax.add_patch(FancyArrowPatch(
        posA=pos[u], posB=pos[v],
        connectionstyle=f"arc3,rad={sgn * rad}",
        arrowstyle="-", linewidth=lw, color=color, alpha=alpha, zorder=z))

for i in pos_idx[np.argsort(edge_abs[pos_idx])]:
    draw_edge(int(i), z=1)
for i in neg_idx[np.argsort(edge_abs[neg_idx])]:
    draw_edge(int(i), z=3)

for nd, (x, y_pos) in pos.items():
    ax.text(x, y_pos, nd, ha="center", va="center",
            fontsize=10, fontweight="bold",
            bbox=dict(boxstyle="round,pad=0.22", facecolor="white",
                      edgecolor="none", alpha=0.75), zorder=5)

sm = mpl.cm.ScalarMappable(cmap=EDGE_CMAP, norm=EDGE_NORM)
sm.set_array([])
cbar = fig.colorbar(sm, ax=ax, fraction=0.035, pad=0.02)
cbar.set_label("Correlation (TCI(t))", fontsize=11)

handles = [mpl.lines.Line2D([0], [0], marker="o", color="w",
           markerfacecolor=COMMUNITY_CMAP(cid % COMMUNITY_CMAP.N),
           markeredgecolor="black", markersize=9, label=f"group {cid + 1}")
           for cid in range(K)]
ax.legend(handles=handles, loc="lower left", frameon=True, fontsize=9)

plt.tight_layout()
plt.savefig(os.path.join(out_dir, "tci_country_corr_network.png"), dpi=320)
plt.close(fig)
print("[OK] tci_country_corr_network.png")

edges_use.sort_values("w", key=lambda s: s.abs(), ascending=False).to_csv(
    os.path.join(out_dir, "tci_country_network_edges.csv"),
    index=False, float_format="%.6f")
print("[OK] tci_country_network_edges.csv")
print("Done.")
