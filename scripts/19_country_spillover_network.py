"""
Script 19 — Between-country VAR-DY spillover network
-----------------------------------------------------
Computes average directional spillover FROM country c TO country c':

    S_{c->c', w} = 1/(|G_c|*|G_c'|) * sum_{i in G_c} sum_{j in G_c'} theta_{ij,w}

Averaged over rolling windows (full sample + calm / crisis regimes).

Outputs:
  outputs/country_spillovers/mat/  — S matrix CSVs
  outputs/country_spillovers/fig/  — heatmap + directed network figures
"""

import os
import warnings
warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import networkx as nx

# ── PATHS ─────────────────────────────────────────────────────────────────────
ROOT      = os.path.join(os.path.dirname(__file__), "..")
THETA_PATH   = os.path.join(ROOT, "outputs", "var_dy",         "mat", "DY_theta_long.csv")
MAP_PATH     = os.path.join(ROOT, "data",    "processed",       "ticker_countries.csv")
REGIME_PATH  = os.path.join(ROOT, "outputs", "cluster_regimes", "mat", "REGIME_assignments.csv")
OUT_MAT      = os.path.join(ROOT, "outputs", "country_spillovers", "mat")
OUT_FIG      = os.path.join(ROOT, "outputs", "country_spillovers", "fig")

# ── LOAD ──────────────────────────────────────────────────────────────────────
print("Loading data...")
theta = pd.read_csv(THETA_PATH)
theta["date"] = pd.to_datetime(theta["date"])

cmap_df = pd.read_csv(MAP_PATH)
cmap_df.columns = ["ticker", "country"]
ticker2country = dict(zip(cmap_df["ticker"], cmap_df["country"]))

regimes = pd.read_csv(REGIME_PATH)
regimes["date"] = pd.to_datetime(regimes["date"])
calm_dates   = set(regimes.loc[regimes["regime"] == "Regime 1", "date"])
crisis_dates = set(regimes.loc[regimes["regime"] == "Regime 2", "date"])

# ── MAP TICKERS TO COUNTRIES ───────────────────────────────────────────────────
theta["c_i"] = theta["i"].map(ticker2country)
theta["c_j"] = theta["j"].map(ticker2country)

# keep only between-country pairs (i != j and different countries)
between = theta[(theta["i"] != theta["j"]) & (theta["c_i"] != theta["c_j"])].copy()
between = between.dropna(subset=["c_i", "c_j"])

countries = sorted(set(between["c_i"]) | set(between["c_j"]))
print(f"Countries: {len(countries)} — {countries}")

# ── COUNTRY SIZE (number of institutions) ────────────────────────────────────
country_size = cmap_df["country"].value_counts().to_dict()


# ── HELPER: build S matrix from a subset of theta rows ───────────────────────
def build_S_matrix(df_sub, countries, country_size):
    """
    Returns DataFrame [countries x countries] with S_{c->c'}.
    Normalised by |G_c| * |G_c'|.
    """
    agg = (
        df_sub.groupby(["c_i", "c_j"])["theta"]
        .mean()          # mean over windows (already per-pair mean)
        .reset_index()
    )
    S = pd.DataFrame(0.0, index=countries, columns=countries)
    for _, row in agg.iterrows():
        c, cp = row["c_i"], row["c_j"]
        if c in S.index and cp in S.columns:
            S.loc[c, cp] = row["theta"]
    return S


# ── COMPUTE MATRICES ──────────────────────────────────────────────────────────
print("Computing spillover matrices...")

S_full   = build_S_matrix(between,                                       countries, country_size)
S_calm   = build_S_matrix(between[between["date"].isin(calm_dates)],     countries, country_size)
S_crisis = build_S_matrix(between[between["date"].isin(crisis_dates)],   countries, country_size)

S_full.to_csv(os.path.join(OUT_MAT, "S_between_full.csv"))
S_calm.to_csv(os.path.join(OUT_MAT, "S_between_calm.csv"))
S_crisis.to_csv(os.path.join(OUT_MAT, "S_between_crisis.csv"))

# Net spillover per country (full sample): TO_c - FROM_c
net_full = S_full.sum(axis=1) - S_full.sum(axis=0)   # out - in
net_full.name = "net_spillover"
net_full.to_csv(os.path.join(OUT_MAT, "net_country_full.csv"))
print("  Matrices saved.")


# ── PLOTTING HELPERS ──────────────────────────────────────────────────────────
def plot_heatmap(S, title, out_path):
    fig, ax = plt.subplots(figsize=(10, 8))
    vmax = np.nanpercentile(S.values[S.values > 0], 95)
    im = ax.imshow(S.values, cmap="YlOrRd", vmin=0, vmax=vmax, aspect="auto")
    ax.set_xticks(range(len(S.columns)))
    ax.set_xticklabels(S.columns, rotation=45, ha="right", fontsize=8)
    ax.set_yticks(range(len(S.index)))
    ax.set_yticklabels(S.index, fontsize=8)
    ax.set_xlabel("Recipient country $c'$", fontsize=9)
    ax.set_ylabel("Source country $c$", fontsize=9)
    ax.set_title(title, fontsize=11, fontweight="bold")
    plt.colorbar(im, ax=ax, label="Avg spillover $S_{c \\to c'}$", shrink=0.8)
    plt.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close()
    print(f"  Saved: {os.path.basename(out_path)}")


def plot_directed_network(S, net, title, out_path, top_edges=40):
    """
    Directed network: nodes = countries, edges = S_{c->c'}.
    Node colour = net spillover (red = emitter, blue = receiver).
    Node size    = total outgoing strength.
    Edge colour  = spillover magnitude (light orange → dark red).
    Edge width   = spillover magnitude. Only top_edges shown.
    """
    countries_here = S.index.tolist()

    G = nx.DiGraph()
    G.add_nodes_from(countries_here)

    # add all edges, keep top by weight for display
    edge_list = []
    for c in countries_here:
        for cp in countries_here:
            w = S.loc[c, cp]
            if w > 0:
                edge_list.append((c, cp, w))
    edge_list.sort(key=lambda x: x[2], reverse=True)
    for c, cp, w in edge_list[:top_edges]:
        G.add_edge(c, cp, weight=w)

    pos = nx.spring_layout(G, seed=42, k=2.5)

    # node attributes
    out_strength = S.sum(axis=1)
    node_sizes   = [300 + 1200 * (out_strength.get(n, 0) / out_strength.max()) for n in G.nodes()]
    net_vals     = [net.get(n, 0) for n in G.nodes()]
    vabs         = max(abs(v) for v in net_vals) if net_vals else 1
    norm         = mcolors.TwoSlopeNorm(vmin=-vabs, vcenter=0, vmax=vabs)
    node_colors  = [plt.cm.RdBu_r(norm(v)) for v in net_vals]

    # edge attributes — colour by spillover magnitude
    weights  = [G[u][v]["weight"] for u, v in G.edges()]
    wmax     = max(weights) if weights else 1
    wmin     = min(weights) if weights else 0
    widths   = [0.5 + 3.5 * (w - wmin) / (wmax - wmin + 1e-9) for w in weights]
    edge_norm   = mcolors.Normalize(vmin=wmin, vmax=wmax)
    edge_cmap   = plt.cm.YlOrRd
    edge_colors = [edge_cmap(edge_norm(w)) for w in weights]

    fig, ax = plt.subplots(figsize=(12, 8))
    fig.patch.set_facecolor("white")

    # draw edges one by one to apply per-edge colour
    for (u, v), w_val, wd, ec in zip(G.edges(), weights, widths, edge_colors):
        nx.draw_networkx_edges(
            G, pos, edgelist=[(u, v)], ax=ax,
            width=wd, alpha=0.85, edge_color=[ec],
            arrows=True, arrowsize=14,
            connectionstyle="arc3,rad=0.15",
            min_source_margin=18, min_target_margin=18
        )

    # draw nodes
    sc = nx.draw_networkx_nodes(
        G, pos, ax=ax,
        node_size=node_sizes,
        node_color=node_colors,
        edgecolors="white", linewidths=1.0
    )
    nx.draw_networkx_labels(G, pos, ax=ax, font_size=8, font_weight="bold", font_color="white")

    # colourbar for node net spillover
    sm_node = plt.cm.ScalarMappable(cmap="RdBu_r", norm=norm)
    sm_node.set_array([])
    cb_node = plt.colorbar(sm_node, ax=ax, shrink=0.55, pad=0.02, location="right")
    cb_node.set_label("Node: net spillover (+ emitter / − receiver)", fontsize=8)

    # colourbar for edge magnitude
    sm_edge = plt.cm.ScalarMappable(cmap="YlOrRd", norm=edge_norm)
    sm_edge.set_array([])
    cb_edge = plt.colorbar(sm_edge, ax=ax, shrink=0.55, pad=0.08, location="right")
    cb_edge.set_label("Edge: spillover $S_{c \\to c'}$", fontsize=8)

    ax.set_title(title, fontsize=11, fontweight="bold", pad=10)
    ax.axis("off")

    # legend for node size
    for frac, label in [(0.33, "Low"), (0.66, "Med"), (1.0, "High")]:
        ax.scatter([], [], s=300 + 1200 * frac, color="grey", alpha=0.6,
                   label=f"{label} outgoing strength")
    ax.legend(scatterpoints=1, frameon=False, labelspacing=1.0,
              title="Outgoing strength", title_fontsize=7, fontsize=7,
              loc="lower left")

    plt.tight_layout()
    fig.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close()
    print(f"  Saved: {os.path.basename(out_path)}")


# ── HEATMAPS ──────────────────────────────────────────────────────────────────
print("\nPlotting heatmaps...")
plot_heatmap(S_full,
             "Between-country VAR-DY spillovers: full sample",
             os.path.join(OUT_FIG, "country_spillover_heatmap_full.png"))
plot_heatmap(S_calm,
             "Between-country VAR-DY spillovers: calm regime",
             os.path.join(OUT_FIG, "country_spillover_heatmap_calm.png"))
plot_heatmap(S_crisis,
             "Between-country VAR-DY spillovers: crisis regime",
             os.path.join(OUT_FIG, "country_spillover_heatmap_crisis.png"))

# ── DIRECTED NETWORKS ─────────────────────────────────────────────────────────
print("\nPlotting directed networks...")
net_calm   = S_calm.sum(axis=1)   - S_calm.sum(axis=0)
net_crisis = S_crisis.sum(axis=1) - S_crisis.sum(axis=0)

plot_directed_network(
    S_full, net_full,
    "Between-country spillover network: full sample\n"
    "Red = net emitter, blue = net receiver; size = outgoing strength; top 40 edges",
    os.path.join(OUT_FIG, "country_spillover_network_full.png")
)
plot_directed_network(
    S_calm, net_calm,
    "Between-country spillover network: calm regime",
    os.path.join(OUT_FIG, "country_spillover_network_calm.png")
)
plot_directed_network(
    S_crisis, net_crisis,
    "Between-country spillover network: crisis regime",
    os.path.join(OUT_FIG, "country_spillover_network_crisis.png")
)

print("\nDone.")
