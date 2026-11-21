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
import matplotlib as mpl
from matplotlib.patches import FancyArrowPatch


# ============================================================
# CONFIG
# ============================================================

with open(os.path.join("config", "project.yml"), encoding="utf-8") as f:
    cfg = yaml.safe_load(f)

paths = cfg.get("paths", {})

THETA_PATH = os.path.join(paths.get("out_var_dy", "outputs/var_dy"), "mat", "DY_theta_long.csv")
RET_PATH   = paths.get("returns_weekly", "data/processed/returns_weekly.csv")
MAP_PATH   = paths.get("ticker_map", "data/processed/ticker_countries.csv")
OUT_DIR    = os.path.join(paths.get("out_compare", "outputs/comparison_var_dcc"), "fig", "snapshots_fixed_panel")

os.makedirs(OUT_DIR, exist_ok=True)

TOP_N_GLOBAL = 10
MAX_EDGES_VAR = 14
MAX_EDGES_DCC = 14

PERIODS = {
    "pre_covid":    ("2017-01-01", "2019-12-31", "Pre-COVID (2017-2019)"),
    "during_covid": ("2020-01-01", "2021-06-30", "During COVID (2020-2021)"),
    "post_covid":   ("2022-01-01", "2023-12-31", "Post-COVID (2022-2023)"),
}

CCOL = {
    "FR": "#1f77b4",
    "DE": "#7f7f7f",
    "ES": "#ff7f0e",
    "IT": "#f5c07a",
    "NL": "#2ca02c",
    "BE": "#98df8a",
    "AT": "#d62728",
    "UK": "#f7b6d2",
    "CH": "#9467bd",
    "SE": "#c5b0d5",
    "DK": "#8c564b",
    "NO": "#c49c94",
    "PT": "#e377c2",
    "PL": "#bcbd22",
    "CZ": "#7f7f7f",
    "IE": "#17becf",
    "GR": "#aec7e8",
    "RO": "#dbdb8d",
    "MT": "#9edae5",
    "IS": "#cedb9c",
}

BG = "#0d1117"
SUBTLE = "#8b949e"
EDGE_POS = "#e74c3c"
EDGE_NEG = "#3498db"
EDGE_VAR = "#5fa8ff"


# ============================================================
# HELPERS
# ============================================================

def get_cluster(ticker, mapping_df):
    row = mapping_df[mapping_df["ticker"] == ticker]
    if len(row):
        return row["cluster"].values[0]
    return "XX"

def node_color(ticker, mapping_df):
    return CCOL.get(get_cluster(ticker, mapping_df), "#aaaaaa")

def safe_layout(G, seed=42):
    if G.number_of_nodes() == 0:
        return {}
    if G.number_of_nodes() == 1:
        n = list(G.nodes())[0]
        return {n: np.array([0.0, 0.0])}
    try:
        return nx.spring_layout(
            G,
            seed=seed,
            k=1.8 / np.sqrt(max(G.number_of_nodes(), 1)),
            iterations=300
        )
    except Exception:
        return nx.kamada_kawai_layout(G)

def normalize_positions(pos):
    if not pos:
        return {}

    xs = np.array([p[0] for p in pos.values()], dtype=float)
    ys = np.array([p[1] for p in pos.values()], dtype=float)

    xmin, xmax = xs.min(), xs.max()
    ymin, ymax = ys.min(), ys.max()

    dx = xmax - xmin
    dy = ymax - ymin

    if dx < 1e-12:
        dx = 1.0
    if dy < 1e-12:
        dy = 1.0

    out = {}
    for n, (x, y) in pos.items():
        xn = -1 + 2 * (x - xmin) / dx
        yn = -1 + 2 * (y - ymin) / dy
        out[n] = np.array([xn, yn])

    return out

def global_top_tickers(theta_df, n):
    t = theta_df[theta_df["i"] != theta_df["j"]].copy()
    t["theta"] = pd.to_numeric(t["theta"], errors="coerce")
    t = t.dropna(subset=["theta"])
    if t.empty:
        return []
    score = t.groupby("i")["theta"].mean().sort_values(ascending=False)
    return score.head(n).index.tolist()

def build_var_graph(tp, tickers, max_edges):
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
    G.add_edges_from(Gmst.edges(data=True))
    return G

def union_graph_layout(graphs, directed=False, seed=42):
    if directed:
        U = nx.Graph()
        for G in graphs:
            U.add_nodes_from(G.nodes())
            for u, v, d in G.edges(data=True):
                w = float(d.get("weight", 1.0))
                if U.has_edge(u, v):
                    U[u][v]["weight"] = max(U[u][v]["weight"], w)
                else:
                    U.add_edge(u, v, weight=w)
    else:
        U = nx.Graph()
        for G in graphs:
            U.add_nodes_from(G.nodes())
            for u, v, d in G.edges(data=True):
                w = abs(float(d.get("weight", d.get("corr", 1.0))))
                if U.has_edge(u, v):
                    U[u][v]["weight"] = max(U[u][v]["weight"], w)
                else:
                    U.add_edge(u, v, weight=w)

    for n in U.nodes():
        if U.degree(n) == 0:
            U.add_edge(n, n, weight=1e-6)

    pos = safe_layout(U, seed=seed)
    pos = {n: p for n, p in pos.items() if n in U.nodes()}
    return normalize_positions(pos)

def scale_value(x, xmin, xmax, ymin, ymax):
    if xmax - xmin < 1e-12:
        return 0.5 * (ymin + ymax)
    return ymin + (x - xmin) * (ymax - ymin) / (xmax - xmin)

def draw_network(ax, G, pos, mapping_df, title, title_col, subtitle, method_name,
                 node_size_map=None, edge_width_map=None, show_legend=True, show_subtitle=True):
    ax.set_facecolor(BG)
    ax.set_axis_off()
    ax.set_title(title, fontsize=18, fontweight="bold", color=title_col, pad=16)

    nodes = list(G.nodes())
    conn = [n for n in nodes if G.degree(n) > 0 and n in pos]

    if len(conn) < 2:
        ax.text(
            0.5, 0.55, "Not enough connected nodes",
            ha="center", va="center",
            color="white", fontsize=13,
            transform=ax.transAxes
        )
        if show_subtitle:
            ax.text(
                0.5, -0.12, subtitle,
                ha="center", va="top",
                fontsize=9, color=SUBTLE, style="italic",
                transform=ax.transAxes
            )
        return

    pos_s = {n: pos[n] for n in conn}

    if node_size_map is None:
        node_size_map = {n: 900 for n in nodes}
    if edge_width_map is None:
        edge_width_map = {}

    node_sizes = [node_size_map.get(n, 900) for n in conn]
    node_cols = [node_color(n, mapping_df) for n in conn]

    nx.draw_networkx_nodes(
        G, pos_s,
        nodelist=conn,
        node_size=node_sizes,
        node_color=node_cols,
        alpha=0.96,
        linewidths=1.8,
        edgecolors="white",
        ax=ax
    )

    if method_name == "var_dy":
        all_edges = [(u, v) for (u, v) in G.edges() if u in pos_s and v in pos_s]
        for (u, v) in all_edges:
            lw = edge_width_map.get((u, v), 2.0)
            rad = 0.18 if u != v else 0.0
            ax.add_patch(
                FancyArrowPatch(
                    posA=pos_s[u],
                    posB=pos_s[v],
                    connectionstyle=f"arc3,rad={rad}",
                    arrowstyle="-|>",
                    mutation_scale=20,
                    linewidth=lw,
                    color=EDGE_VAR,
                    alpha=0.78,
                    zorder=2
                )
            )
    else:
        all_edges = [(u, v) for (u, v) in G.edges() if u in pos_s and v in pos_s]
        widths = [edge_width_map.get((u, v), 2.0) for (u, v) in all_edges]

        if method_name == "dcc":
            colors = [EDGE_POS if G[u][v]["weight"] >= 0 else EDGE_NEG for (u, v) in all_edges]
        else:
            colors = [
                EDGE_POS if G[u][v].get("corr", 0.0) >= 0 else EDGE_NEG
                for (u, v) in all_edges
            ]

        nx.draw_networkx_edges(
            G, pos_s,
            edgelist=all_edges,
            width=widths,
            edge_color=colors,
            alpha=0.72,
            ax=ax
        )

    nx.draw_networkx_labels(
        G, pos_s,
        labels={n: n for n in conn},
        font_size=11,
        font_color="white",
        font_weight="bold",
        ax=ax
    )

    countries = sorted(set(get_cluster(n, mapping_df) for n in conn))
    handles = [
        mpl.patches.Patch(color=CCOL.get(c, "#aaa"), label=c)
        for c in countries if c in CCOL
    ]

    if show_legend and handles:
        legend = ax.legend(
            handles=handles,
            loc="upper center",
            bbox_to_anchor=(0.5, -0.03),
            ncol=min(6, len(handles)),
            fontsize=9,
            framealpha=0.25,
            facecolor="#161b22",
            edgecolor="none",
            labelcolor="white",
            title="Country",
            title_fontsize=9
        )
        legend.get_title().set_color("white")

    if show_subtitle:
        ax.text(
            0.5, -0.12, subtitle,
            ha="center", va="top",
            fontsize=9, color=SUBTLE, style="italic",
            transform=ax.transAxes
        )


# ============================================================
# LOAD DATA
# ============================================================

print("Loading...")

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
print(f"\nFixed global panel ({len(global_tickers)} banks): {global_tickers}")

# ============================================================
# BUILD ALL PERIOD GRAPHS FIRST
# ============================================================

period_results = {}
all_var_graphs = []
all_dcc_graphs = []
all_mst_graphs = []

for pk, (d0s, d1s, lbl) in PERIODS.items():
    print(f"\n=== Preparing {lbl} ===")
    d0, d1 = pd.Timestamp(d0s), pd.Timestamp(d1s)

    tp = ta[(ta["date"] >= d0) & (ta["date"] <= d1)].copy()
    rp = ret[(ret["date"] >= d0) & (ret["date"] <= d1)].copy()
    rs = rp[[c for c in global_tickers if c in rp.columns]].copy()

    Gv = build_var_graph(tp, global_tickers, MAX_EDGES_VAR)
    Gd, cm = build_dcc_graph(rs, global_tickers, MAX_EDGES_DCC)
    Gm = build_mst_graph(cm, global_tickers)

    all_var_graphs.append(Gv)
    all_dcc_graphs.append(Gd)
    all_mst_graphs.append(Gm)

    period_results[pk] = {
        "label": lbl,
        "Gv": Gv,
        "Gd": Gd,
        "Gm": Gm
    }

# ============================================================
# FIXED LAYOUTS PER METHOD
# ============================================================

print("\nComputing fixed layouts...")

pos_var = union_graph_layout(all_var_graphs, directed=True, seed=42)
pos_dcc = union_graph_layout(all_dcc_graphs, directed=False, seed=42)
pos_mst = union_graph_layout(all_mst_graphs, directed=False, seed=42)

# ============================================================
# GLOBAL VISUAL SCALES
# ============================================================

all_var_out_strengths = []
all_var_edge_weights = []
all_dcc_degrees = []
all_dcc_edge_weights = []
all_mst_degrees = []
all_mst_edge_weights = []

for pk in PERIODS:
    Gv = period_results[pk]["Gv"]
    Gd = period_results[pk]["Gd"]
    Gm = period_results[pk]["Gm"]

    all_var_out_strengths.extend([
        sum(d["weight"] for _, _, d in Gv.out_edges(n, data=True))
        for n in Gv.nodes()
    ])
    all_var_edge_weights.extend([Gv[u][v]["weight"] for u, v in Gv.edges()])

    all_dcc_degrees.extend([Gd.degree(n) for n in Gd.nodes()])
    all_dcc_edge_weights.extend([abs(Gd[u][v]["weight"]) for u, v in Gd.edges()])

    all_mst_degrees.extend([Gm.degree(n) for n in Gm.nodes()])
    all_mst_edge_weights.extend([abs(Gm[u][v].get("corr", 0.0)) for u, v in Gm.edges()])

if len(all_var_out_strengths) == 0:
    all_var_out_strengths = [1]
if len(all_var_edge_weights) == 0:
    all_var_edge_weights = [1]
if len(all_dcc_degrees) == 0:
    all_dcc_degrees = [1]
if len(all_dcc_edge_weights) == 0:
    all_dcc_edge_weights = [1]
if len(all_mst_degrees) == 0:
    all_mst_degrees = [1]
if len(all_mst_edge_weights) == 0:
    all_mst_edge_weights = [1]

var_out_min, var_out_max = min(all_var_out_strengths), max(all_var_out_strengths)
var_w_min, var_w_max = min(all_var_edge_weights), max(all_var_edge_weights)

dcc_deg_min, dcc_deg_max = min(all_dcc_degrees), max(all_dcc_degrees)
dcc_w_min, dcc_w_max = min(all_dcc_edge_weights), max(all_dcc_edge_weights)

mst_deg_min, mst_deg_max = min(all_mst_degrees), max(all_mst_degrees)
mst_w_min, mst_w_max = min(all_mst_edge_weights), max(all_mst_edge_weights)

# ============================================================
# DRAW PERIOD FIGURES
# ============================================================

print("\nDrawing individual figures...")

for pk, (_, _, lbl) in PERIODS.items():
    Gv = period_results[pk]["Gv"]
    Gd = period_results[pk]["Gd"]
    Gm = period_results[pk]["Gm"]

    out_strength = {
        n: sum(d["weight"] for _, _, d in Gv.out_edges(n, data=True))
        for n in Gv.nodes()
    }
    node_size_var = {
        n: scale_value(out_strength.get(n, 0.0), var_out_min, var_out_max, 700, 1900)
        for n in Gv.nodes()
    }
    edge_width_var = {
        (u, v): scale_value(Gv[u][v]["weight"], var_w_min, var_w_max, 2.5, 8.0)
        for u, v in Gv.edges()
    }

    node_size_dcc = {
        n: scale_value(Gd.degree(n), dcc_deg_min, dcc_deg_max, 650, 1700)
        for n in Gd.nodes()
    }
    edge_width_dcc = {
        (u, v): scale_value(abs(Gd[u][v]["weight"]), dcc_w_min, dcc_w_max, 2.0, 7.0)
        for u, v in Gd.edges()
    }

    node_size_mst = {
        n: scale_value(Gm.degree(n), mst_deg_min, mst_deg_max, 650, 1800)
        for n in Gm.nodes()
    }
    edge_width_mst = {
        (u, v): scale_value(abs(Gm[u][v].get("corr", 0.0)), mst_w_min, mst_w_max, 2.0, 7.0)
        for u, v in Gm.edges()
    }

    figs = [
        (
            "var_dy",
            Gv, pos_var,
            node_size_var, edge_width_var,
            f"VAR-DY Connectedness: {lbl}",
            "#74b9ff",
            f"Fixed panel ({TOP_N_GLOBAL} banks) | fixed layout | top {MAX_EDGES_VAR} directed links | node size = out-strength"
        ),
        (
            "dcc",
            Gd, pos_dcc,
            node_size_dcc, edge_width_dcc,
            f"DCC-GARCH Correlation: {lbl}",
            "#fd79a8",
            f"Fixed panel ({TOP_N_GLOBAL} banks) | fixed layout | top {MAX_EDGES_DCC} links by |correlation|"
        ),
        (
            "mst",
            Gm, pos_mst,
            node_size_mst, edge_width_mst,
            f"MST Network: {lbl}",
            "#55efc4",
            f"Fixed panel ({TOP_N_GLOBAL} banks) | fixed layout | node size = degree | edge width = |correlation|"
        ),
    ]

    for method_name, G, pos, ns_map, ew_map, title, tcol, subtitle in figs:
        fig, ax = plt.subplots(figsize=(11, 10))
        fig.patch.set_facecolor(BG)

        draw_network(
            ax=ax,
            G=G,
            pos=pos,
            mapping_df=mdf,
            title=title,
            title_col=tcol,
            subtitle=subtitle,
            method_name=method_name,
            node_size_map=ns_map,
            edge_width_map=ew_map,
            show_legend=True,
            show_subtitle=True
        )

        out_path = os.path.join(OUT_DIR, f"snapshot_{pk}_{method_name}.png")
        plt.tight_layout(rect=[0, 0.20, 1, 1])
        plt.savefig(out_path, dpi=230, bbox_inches="tight", facecolor=fig.get_facecolor())
        plt.close()
        print(f"  [OK] {os.path.basename(out_path)}")

# ============================================================
# COMPARATIVE PANELS
# ============================================================

print("\nDrawing comparative panels...")

comparative_configs = [
    ("var_dy", "Gv", pos_var, "#74b9ff", "VAR-DY Connectedness"),
    ("dcc",    "Gd", pos_dcc, "#fd79a8", "DCC-GARCH Correlation"),
    ("mst",    "Gm", pos_mst, "#55efc4", "MST Network"),
]

for method_name, key, pos_fixed, title_color, main_title in comparative_configs:
    fig, axes = plt.subplots(1, 3, figsize=(22, 7.5))
    fig.patch.set_facecolor(BG)

    for ax, pk in zip(axes, PERIODS.keys()):
        lbl = period_results[pk]["label"]
        G = period_results[pk][key]

        if method_name == "var_dy":
            out_strength = {
                n: sum(d["weight"] for _, _, d in G.out_edges(n, data=True))
                for n in G.nodes()
            }
            ns_map = {
                n: scale_value(out_strength.get(n, 0.0), var_out_min, var_out_max, 650, 1700)
                for n in G.nodes()
            }
            ew_map = {
                (u, v): scale_value(G[u][v]["weight"], var_w_min, var_w_max, 2.5, 7.5)
                for u, v in G.edges()
            }
            subtitle = "Fixed panel + fixed layout"
        elif method_name == "dcc":
            ns_map = {
                n: scale_value(G.degree(n), dcc_deg_min, dcc_deg_max, 620, 1500)
                for n in G.nodes()
            }
            ew_map = {
                (u, v): scale_value(abs(G[u][v]["weight"]), dcc_w_min, dcc_w_max, 2.0, 6.5)
                for u, v in G.edges()
            }
            subtitle = "Fixed panel + fixed layout"
        else:
            ns_map = {
                n: scale_value(G.degree(n), mst_deg_min, mst_deg_max, 620, 1550)
                for n in G.nodes()
            }
            ew_map = {
                (u, v): scale_value(abs(G[u][v].get("corr", 0.0)), mst_w_min, mst_w_max, 2.0, 6.5)
                for u, v in G.edges()
            }
            subtitle = "Fixed panel + fixed layout"

        draw_network(
            ax=ax,
            G=G,
            pos=pos_fixed,
            mapping_df=mdf,
            title=lbl,
            title_col=title_color,
            subtitle=subtitle,
            method_name=method_name,
            node_size_map=ns_map,
            edge_width_map=ew_map,
            show_legend=False,
            show_subtitle=False
        )

    countries_panel = sorted(set(get_cluster(n, mdf) for n in global_tickers))
    handles_panel = [
        mpl.patches.Patch(color=CCOL.get(c, "#aaa"), label=c)
        for c in countries_panel if c in CCOL
    ]

    if handles_panel:
        legend = fig.legend(
            handles=handles_panel,
            loc="lower center",
            bbox_to_anchor=(0.5, 0.06),
            ncol=min(8, len(handles_panel)),
            fontsize=10,
            framealpha=0.25,
            facecolor="#161b22",
            edgecolor="none",
            labelcolor="white",
            title="Country",
            title_fontsize=10
        )
        legend.get_title().set_color("white")

    fig.text(
        0.5, 0.025,
        "Fixed panel + fixed layout",
        ha="center", va="bottom",
        fontsize=10, color=SUBTLE, style="italic"
    )

    fig.suptitle(
        f"{main_title}: cross-period comparison",
        fontsize=20,
        fontweight="bold",
        color=title_color,
        y=0.98
    )

    out_path = os.path.join(OUT_DIR, f"panel_compare_{method_name}.png")
    plt.tight_layout(rect=[0, 0.14, 1, 0.94])
    plt.savefig(out_path, dpi=240, bbox_inches="tight", facecolor=fig.get_facecolor())
    plt.close()
    print(f"  [OK] {os.path.basename(out_path)}")

print("\nDone.")