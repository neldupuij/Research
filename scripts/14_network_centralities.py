import os
import warnings
warnings.filterwarnings("ignore")

import yaml
import numpy as np
import pandas as pd
import networkx as nx
from scipy.stats import spearmanr

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt


# ============================================================
# CONFIG
# ============================================================

with open(os.path.join("config", "project.yml"), encoding="utf-8") as f:
    cfg = yaml.safe_load(f)

paths = cfg.get("paths", {})

THETA_PATH = os.path.join(paths.get("out_var_dy", "outputs/var_dy"), "mat", "DY_theta_long.csv")
RET_PATH   = paths.get("returns_weekly", "data/processed/returns_weekly.csv")
MAP_PATH   = paths.get("ticker_map", "data/processed/ticker_countries.csv")

OUT_TABLES = os.path.join(paths.get("out_compare", "outputs/comparison_var_dcc"), "tables", "centralities")
OUT_FIGS   = os.path.join(paths.get("out_compare", "outputs/comparison_var_dcc"), "fig", "centralities")
OUT_FIGS_CMP = os.path.join(paths.get("out_compare", "outputs/comparison_var_dcc"), "fig")

VAR_MAT = os.path.join(paths.get("out_var_dy", "outputs/var_dy"), "mat")
MST_MAT = os.path.join(paths.get("out_mst", "outputs/mst_global"), "mat")

os.makedirs(OUT_TABLES, exist_ok=True)
os.makedirs(OUT_FIGS, exist_ok=True)

TOP_N_GLOBAL = 10
MAX_EDGES_VAR = 14
MAX_EDGES_DCC = 14

PERIODS = {
    "pre_covid":    ("2017-01-01", "2019-12-31", "Pre-COVID (2017-2019)"),
    "during_covid": ("2020-01-01", "2021-06-30", "During COVID (2020-2021)"),
    "post_covid":   ("2022-01-01", "2023-12-31", "Post-COVID (2022-2023)"),
}

METHOD_ORDER = ["var_dy", "dcc", "mst"]
METHOD_LABELS = {
    "var_dy": "VAR-DY",
    "dcc": "DCC",
    "mst": "MST",
}
PERIOD_ORDER = ["pre_covid", "during_covid", "post_covid"]
PERIOD_LABELS_SHORT = {
    "pre_covid": "Pre",
    "during_covid": "During",
    "post_covid": "Post",
}
METRIC_COLS = ["degree", "strength", "betweenness", "closeness", "eigenvector"]


# ============================================================
# HELPERS
# ============================================================

def get_cluster(ticker, mapping_df):
    row = mapping_df[mapping_df["ticker"] == ticker]
    if len(row):
        return row["cluster"].values[0]
    return "XX"

def global_top_tickers(theta_df, n):
    t = theta_df[theta_df["i"] != theta_df["j"]].copy()
    t["theta"] = pd.to_numeric(t["theta"], errors="coerce")
    t = t.dropna(subset=["theta"])
    if t.empty:
        return []
    score = t.groupby("i")["theta"].mean().sort_values(ascending=False)
    return score.head(n).index.tolist()

def build_var_graph_directed(tp, tickers, max_edges):
    ts = tp[
        tp["i"].isin(tickers) &
        tp["j"].isin(tickers) &
        (tp["i"] != tp["j"])
    ].copy()

    ts["theta"] = pd.to_numeric(ts["theta"], errors="coerce")
    ts = ts.dropna(subset=["theta"])

    G = nx.DiGraph()
    G.add_nodes_from(tickers)

    if ts.empty:
        return G

    tm = (
        ts.groupby(["i", "j"], as_index=False)["theta"]
          .mean()
          .sort_values("theta", ascending=False)
          .head(max_edges)
    )

    for _, r in tm.iterrows():
        G.add_edge(r["i"], r["j"], weight=float(r["theta"]))

    return G

def symmetrize_digraph_sum(Gd):
    Gu = nx.Graph()
    Gu.add_nodes_from(Gd.nodes())

    pair_weights = {}
    for u, v, d in Gd.edges(data=True):
        if u == v:
            continue
        key = tuple(sorted((u, v)))
        pair_weights[key] = pair_weights.get(key, 0.0) + float(d.get("weight", 0.0))

    for (u, v), w in pair_weights.items():
        if w > 0:
            Gu.add_edge(u, v, weight=w)

    return Gu

def build_dcc_graph(rs, tickers, max_edges):
    G = nx.Graph()
    G.add_nodes_from(tickers)
    cm = None

    valid_cols = [c for c in tickers if c in rs.columns]
    if len(valid_cols) < 4 or len(rs) < 20:
        return G, cm

    sub = rs[valid_cols].copy()
    cm = sub.corr(method="pearson", min_periods=10)

    pairs = []
    for i in range(len(valid_cols)):
        for j in range(i + 1, len(valid_cols)):
            val = cm.iloc[i, j]
            if np.isfinite(val):
                pairs.append((valid_cols[i], valid_cols[j], float(val)))

    pairs.sort(key=lambda x: abs(x[2]), reverse=True)

    for ti, tj, v in pairs[:max_edges]:
        G.add_edge(ti, tj, weight=v)

    return G, cm

def build_mst_graph(cm, tickers):
    G = nx.Graph()
    G.add_nodes_from(tickers)

    valid_cols = [c for c in tickers if c in cm.columns] if cm is not None else []
    if cm is None or len(valid_cols) < 4:
        return G

    cm2 = cm.loc[valid_cols, valid_cols].copy()
    cv = np.clip(cm2.values, -1, 1)
    dist = np.sqrt(2 * (1 - cv))
    np.fill_diagonal(dist, 0)

    Gf = nx.Graph()
    Gf.add_nodes_from(valid_cols)

    for i in range(len(valid_cols)):
        for j in range(i + 1, len(valid_cols)):
            Gf.add_edge(
                valid_cols[i],
                valid_cols[j],
                weight=float(dist[i, j]),
                corr=float(cm2.iloc[i, j])
            )

    Gmst = nx.minimum_spanning_tree(Gf, weight="weight")

    G = nx.Graph()
    G.add_nodes_from(tickers)
    G.add_edges_from(Gmst.edges(data=True))

    for u, v in G.edges():
        corr = float(G[u][v].get("corr", 0.0))
        G[u][v]["weight"] = abs(corr)

    return G

def make_connected_copy_for_path_metrics(G, use_distance_transform=True):
    H = nx.Graph()
    H.add_nodes_from(G.nodes())

    for u, v, d in G.edges(data=True):
        w = float(d.get("weight", 0.0))
        if not np.isfinite(w) or w <= 0:
            continue

        dist = 1.0 / w if use_distance_transform else w
        H.add_edge(u, v, weight=w, distance=dist)

    return H

def safe_eigenvector_centrality(G, weight="weight", max_iter=5000, tol=1e-8):
    try:
        return nx.eigenvector_centrality(G, weight=weight, max_iter=max_iter, tol=tol)
    except Exception:
        return {n: np.nan for n in G.nodes()}

def compute_common_centralities(G, weight_attr="weight"):
    nodes = list(G.nodes())

    degree_dict = dict(G.degree())
    strength_dict = {
        n: sum(float(d.get(weight_attr, 0.0)) for _, _, d in G.edges(n, data=True))
        for n in nodes
    }

    H = make_connected_copy_for_path_metrics(G, use_distance_transform=True)

    if H.number_of_edges() > 0:
        betweenness_dict = nx.betweenness_centrality(H, weight="distance", normalized=True)
        try:
            closeness_dict = nx.closeness_centrality(H, distance="distance")
        except Exception:
            closeness_dict = {n: np.nan for n in nodes}
    else:
        betweenness_dict = {n: 0.0 for n in nodes}
        closeness_dict = {n: 0.0 for n in nodes}

    if G.number_of_edges() > 0:
        eigenvector_dict = safe_eigenvector_centrality(G, weight=weight_attr)
    else:
        eigenvector_dict = {n: np.nan for n in nodes}

    rows = []
    for n in nodes:
        rows.append({
            "ticker": n,
            "degree": float(degree_dict.get(n, 0.0)),
            "strength": float(strength_dict.get(n, 0.0)),
            "betweenness": float(betweenness_dict.get(n, np.nan)),
            "closeness": float(closeness_dict.get(n, np.nan)),
            "eigenvector": float(eigenvector_dict.get(n, np.nan)),
        })

    return pd.DataFrame(rows)

def rank_within_group(df, metric_cols):
    out = df.copy()
    for col in metric_cols:
        out[f"rank_{col}"] = out.groupby(["period", "method"])[col].rank(
            ascending=False, method="dense"
        )
    return out


# ============================================================
# PLOTTING HELPERS
# ============================================================

def save_rank_heatmap(centralities_all, metric, out_dir):
    rank_col = f"rank_{metric}"

    # échelle commune à toutes les méthodes
    global_min = centralities_all[rank_col].min()
    global_max = centralities_all[rank_col].max()

    fig, axes = plt.subplots(1, 3, figsize=(16, 7))
    fig.suptitle(f"Rank heatmap: {metric}", fontsize=16)

    for ax, method in zip(axes, METHOD_ORDER):
        sub = centralities_all[centralities_all["method"] == method].copy()

        pivot = sub.pivot_table(
            index="ticker",
            columns="period",
            values=rank_col
        )

        cols = [c for c in PERIOD_ORDER if c in pivot.columns]
        pivot = pivot[cols]

        if pivot.empty:
            ax.text(0.5, 0.5, "No data", ha="center", va="center", transform=ax.transAxes)
            ax.set_title(METHOD_LABELS[method])
            ax.axis("off")
            continue

        pivot["__mean__"] = pivot.mean(axis=1, skipna=True)
        pivot = pivot.sort_values("__mean__", ascending=True).drop(columns="__mean__")

        im = ax.imshow(
            pivot.values,
            aspect="auto",
            vmin=global_min,
            vmax=global_max,
            cmap="viridis_r"
        )

        ax.set_title(METHOD_LABELS[method])
        ax.set_xticks(range(len(pivot.columns)))
        ax.set_xticklabels([PERIOD_LABELS_SHORT[c] for c in pivot.columns], rotation=20)
        ax.set_yticks(range(len(pivot.index)))
        ax.set_yticklabels(pivot.index)

        cbar = plt.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
        cbar.ax.set_ylabel(rank_col, rotation=90)

    plt.tight_layout(rect=[0, 0, 1, 0.95])
    plt.savefig(os.path.join(out_dir, f"rank_heatmap_{metric}.png"), dpi=220, bbox_inches="tight")
    plt.close()

def save_strength_trajectory(centralities_all, out_dir, top_n_labels=5):
    fig, axes = plt.subplots(1, 3, figsize=(18, 5))
    fig.suptitle("Strength centrality across periods", fontsize=16)

    x_map = {"pre_covid": 0, "during_covid": 1, "post_covid": 2}
    x_labels = ["Pre", "During", "Post"]

    for ax, method in zip(axes, METHOD_ORDER):
        sub = centralities_all[centralities_all["method"] == method].copy()

        # ne label que les top banques en moyenne
        mean_strength = (
            sub.groupby("ticker")["strength"]
               .mean()
               .sort_values(ascending=False)
        )
        top_label_tickers = set(mean_strength.head(top_n_labels).index.tolist())

        for ticker in sorted(sub["ticker"].unique()):
            st = sub[sub["ticker"] == ticker].copy()
            st["x"] = st["period"].map(x_map)
            st = st.sort_values("x")

            linewidth = 2.2 if ticker in top_label_tickers else 1.0
            alpha = 0.95 if ticker in top_label_tickers else 0.45

            ax.plot(
                st["x"],
                st["strength"],
                marker="o",
                linewidth=linewidth,
                alpha=alpha
            )

            if len(st) and ticker in top_label_tickers:
                last = st.iloc[-1]
                ax.text(last["x"] + 0.04, last["strength"], ticker, fontsize=8)

        ax.set_title(METHOD_LABELS[method])
        ax.set_xticks([0, 1, 2])
        ax.set_xticklabels(x_labels)
        ax.set_xlabel("Period")
        ax.set_ylabel("Strength")
        ax.grid(alpha=0.3)

    plt.tight_layout(rect=[0, 0, 1, 0.93])
    plt.savefig(os.path.join(out_dir, "trajectory_strength.png"), dpi=220, bbox_inches="tight")
    plt.close()

def save_top5_strength_by_method_period(centralities_all, out_dir):
    fig, axes = plt.subplots(3, 3, figsize=(16, 13))
    fig.suptitle("Top 5 banks by strength: methods × periods", fontsize=16)

    for i, period_key in enumerate(PERIOD_ORDER):
        for j, method in enumerate(METHOD_ORDER):
            ax = axes[i, j]

            sub = centralities_all[
                (centralities_all["period"] == period_key) &
                (centralities_all["method"] == method)
            ].copy()

            sub = sub.sort_values("strength", ascending=False).head(5)
            sub = sub.sort_values("strength", ascending=True)

            if sub.empty:
                ax.text(0.5, 0.5, "No data", ha="center", va="center", transform=ax.transAxes)
                ax.set_title(f"{period_key} | {method}")
                continue

            ax.barh(sub["ticker"], sub["strength"])
            ax.set_title(f"{period_key} | {METHOD_LABELS[method]}")
            ax.set_xlabel("Strength")
            ax.grid(axis="x", alpha=0.3)

    plt.tight_layout(rect=[0, 0, 1, 0.96])
    plt.savefig(os.path.join(out_dir, "top5_strength_by_method_period.png"), dpi=220, bbox_inches="tight")
    plt.close()


# ============================================================
# LOAD DATA
# ============================================================

print("Loading data...")

ta = pd.read_csv(THETA_PATH)
ta["date"] = pd.to_datetime(ta["date"], errors="coerce")
ta = ta.dropna(subset=["date"])

ret = pd.read_csv(RET_PATH)
ret["date"] = pd.to_datetime(ret["date"], errors="coerce")
ret = ret.dropna(subset=["date"]).sort_values("date").reset_index(drop=True)

mdf = pd.read_csv(MAP_PATH)
mdf.columns = ["ticker", "cluster"]
mdf["ticker"] = mdf["ticker"].astype(str)

# ============================================================
# FIXED GLOBAL PANEL
# ============================================================

global_tickers = global_top_tickers(ta, TOP_N_GLOBAL)
print(f"Fixed global panel ({len(global_tickers)} banks): {global_tickers}")

# ============================================================
# BUILD NETWORKS + CENTRALITIES
# ============================================================

all_rows = []

for period_key, (d0s, d1s, period_label) in PERIODS.items():
    print(f"\n=== {period_label} ===")

    d0, d1 = pd.Timestamp(d0s), pd.Timestamp(d1s)

    tp = ta[(ta["date"] >= d0) & (ta["date"] <= d1)].copy()
    rp = ret[(ret["date"] >= d0) & (ret["date"] <= d1)].copy()
    rs = rp[[c for c in global_tickers if c in rp.columns]].copy()

    # VAR-DY symmetrized
    Gv_dir = build_var_graph_directed(tp, global_tickers, MAX_EDGES_VAR)
    Gv = symmetrize_digraph_sum(Gv_dir)

    # DCC absolute correlation
    Gd, cm = build_dcc_graph(rs, global_tickers, MAX_EDGES_DCC)
    Gd_abs = nx.Graph()
    Gd_abs.add_nodes_from(Gd.nodes())
    for u, v, d in Gd.edges(data=True):
        Gd_abs.add_edge(u, v, weight=abs(float(d.get("weight", 0.0))))

    # MST
    Gm = build_mst_graph(cm, global_tickers)

    network_dict = {
        "var_dy": Gv,
        "dcc": Gd_abs,
        "mst": Gm
    }

    for method_name, G in network_dict.items():
        cent = compute_common_centralities(G, weight_attr="weight")
        cent["period"] = period_key
        cent["period_label"] = period_label
        cent["method"] = method_name
        cent["cluster"] = cent["ticker"].map(lambda x: get_cluster(x, mdf))

        cent = cent[
            [
                "period", "period_label", "method", "ticker", "cluster",
                "degree", "strength", "betweenness", "closeness", "eigenvector"
            ]
        ]

        all_rows.append(cent)

        out_file = os.path.join(OUT_TABLES, f"centralities_{period_key}_{method_name}.csv")
        cent.to_csv(out_file, index=False)
        print(f"  [OK] {os.path.basename(out_file)}")

# ============================================================
# COMBINED TABLE
# ============================================================

centralities_all = pd.concat(all_rows, ignore_index=True)
centralities_all = rank_within_group(centralities_all, METRIC_COLS)

combined_file = os.path.join(OUT_TABLES, "centralities_all_methods_periods.csv")
centralities_all.to_csv(combined_file, index=False)
print(f"\n[OK] {os.path.basename(combined_file)}")

# ============================================================
# TOP 5 SUMMARY
# ============================================================

summary_rows = []

for (period_key, method_name), sub in centralities_all.groupby(["period", "method"]):
    for metric in METRIC_COLS:
        top_sub = sub.sort_values(metric, ascending=False).head(5).copy()
        top_sub["metric"] = metric
        top_sub["top_k"] = range(1, len(top_sub) + 1)
        summary_rows.append(
            top_sub[
                [
                    "period", "period_label", "method", "metric", "top_k",
                    "ticker", "cluster", metric
                ]
            ].rename(columns={metric: "value"})
        )

summary_df = pd.concat(summary_rows, ignore_index=True)
summary_file = os.path.join(OUT_TABLES, "centralities_top5_summary.csv")
summary_df.to_csv(summary_file, index=False)
print(f"[OK] {os.path.basename(summary_file)}")

# ============================================================
# PIVOT TABLES
# ============================================================

for metric in METRIC_COLS:
    pivot = centralities_all.pivot_table(
        index=["ticker", "cluster"],
        columns=["period", "method"],
        values=metric
    )
    pivot = pivot.sort_index(axis=1)
    pivot_file = os.path.join(OUT_TABLES, f"pivot_{metric}.csv")
    pivot.to_csv(pivot_file)
    print(f"[OK] {os.path.basename(pivot_file)}")

# ============================================================
# FIGURES (SIMPLIFIED + IMPROVED)
# ============================================================

print("\nCreating figures...")

save_rank_heatmap(centralities_all, "strength", OUT_FIGS)
print("  [OK] rank_heatmap_strength.png")

save_rank_heatmap(centralities_all, "betweenness", OUT_FIGS)
print("  [OK] rank_heatmap_betweenness.png")

save_strength_trajectory(centralities_all, OUT_FIGS, top_n_labels=5)
print("  [OK] trajectory_strength.png")

save_top5_strength_by_method_period(centralities_all, OUT_FIGS)
print("  [OK] top5_strength_by_method_period.png")

# ============================================================
# CROSS-METHOD COMPARABLE STRENGTH (full sample)
# VAR-DY TO strength vs Corr strength vs MST weighted degree
# ============================================================

print("\nCross-method comparable strength comparison...")

dy_to = pd.read_csv(os.path.join(VAR_MAT, "DY_to.csv"))
var_to = dy_to.mean(axis=0)
var_to.name = "var_to"

corr_mat_full = pd.read_csv(os.path.join(MST_MAT, "corr_weekly_global.csv"), index_col=0)
np.fill_diagonal(corr_mat_full.values, 0)
corr_strength = corr_mat_full.abs().sum(axis=1)
corr_strength.name = "corr_strength"

mst_edges = pd.read_csv(os.path.join(MST_MAT, "mst_edges_global.csv"))
mst_w = {}
for _, row in mst_edges.iterrows():
    for node in [row["u"], row["v"]]:
        mst_w[node] = mst_w.get(node, 0.0) + abs(row["corr"])
mst_weighted = pd.Series(mst_w, name="mst_strength")

df_cmp = pd.DataFrame({
    "var_to":        var_to,
    "corr_strength": corr_strength,
    "mst_strength":  mst_weighted,
}).dropna()

print(f"  Institutions in merged dataset: {len(df_cmp)}")

rho_c, p_c = spearmanr(df_cmp["var_to"], df_cmp["corr_strength"])
rho_m, p_m = spearmanr(df_cmp["var_to"], df_cmp["mst_strength"])
print(f"  VAR-DY TO vs Corr strength  : Spearman rho = {rho_c:.3f}  (p = {p_c:.2e})")
print(f"  VAR-DY TO vs MST wgt degree : Spearman rho = {rho_m:.3f}  (p = {p_m:.2e})")

BLUE  = "#2E86DE"
GREEN = "#27AE60"

fig, axes = plt.subplots(1, 2, figsize=(11, 4.5))
fig.patch.set_facecolor("white")

panels = [
    (axes[0], "corr_strength",
     r"Correlation strength  $\sum_{j}|\hat\rho_{ij}|$",
     rho_c, p_c, BLUE),
    (axes[1], "mst_strength",
     r"MST weighted degree  $\sum_{(i,j)\in\mathcal{T}}|\hat\rho_{ij}|$",
     rho_m, p_m, GREEN),
]

for ax, ycol, ylabel, rho, pval, color in panels:
    x = df_cmp["var_to"].values
    y = df_cmp[ycol].values
    ax.scatter(x, y, s=38, alpha=0.78, color=color,
               edgecolors="white", linewidths=0.5, zorder=3)
    m, b = np.polyfit(x, y, 1)
    xr = np.linspace(x.min(), x.max(), 300)
    ax.plot(xr, m * xr + b, color="black", linewidth=1.0,
            linestyle="--", alpha=0.6, zorder=2)
    pstr = "0" if pval < 5e-4 else f"{pval:.3f}"
    ax.text(0.05, 0.95,
            f"Spearman $\\rho = {rho:.2f}$\n$p \\approx {pstr}$",
            transform=ax.transAxes, va="top", fontsize=9,
            bbox=dict(boxstyle="round,pad=0.3", facecolor="white",
                      alpha=0.88, edgecolor="none"))
    ax.set_xlabel("VAR-DY time-averaged TO strength", fontsize=9)
    ax.set_ylabel(ylabel, fontsize=9)
    ax.tick_params(labelsize=8)
    ax.spines[["top", "right"]].set_visible(False)
    ax.grid(True, linestyle=":", linewidth=0.5, alpha=0.5)

fig.suptitle(
    "Cross-method centrality: comparable strength basis (full sample, $n=63$)",
    fontsize=10, y=1.02
)
plt.tight_layout()
out_path = os.path.join(OUT_FIGS_CMP, "centrality_strength_comparison.png")
fig.savefig(out_path, dpi=150, bbox_inches="tight")
plt.close()
print(f"  [OK] centrality_strength_comparison.png")

print("\nDone.")