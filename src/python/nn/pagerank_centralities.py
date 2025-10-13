import pandas as pd
import networkx as nx
from pathlib import Path
import sys

# __file__ = .../src/python/nn/pagerank_centralities.py
ROOT = Path(__file__).resolve().parents[3]
nn_dir = ROOT / "reports" / "exports_nn"
edges_path = nn_dir / "edges_last_window.csv"
strengths_path = nn_dir / "strengths_last_window.csv"
out_path = nn_dir / "centralities_last_window.csv"

if not edges_path.exists():
    sys.exit(f"[pagerank_centralities] Introuvable: {edges_path} — lance d'abord prepare_graph_series.py / make nn")
if not strengths_path.exists():
    sys.exit(f"[pagerank_centralities] Introuvable: {strengths_path} — lance d'abord prepare_graph_series.py / make nn")

# --- lecture edges avec normalisation des colonnes ---
edges = pd.read_csv(edges_path)
# normalise les noms (lower + trim)
edges.columns = [str(c).strip().lower() for c in edges.columns]

# cas standard: colonnes attendues src/dst/weight
if {"src","dst","weight"}.issubset(set(edges.columns)):
    e = edges[["src","dst","weight"]].copy()
# fallback: prendre les 3 premières colonnes et les renommer
elif edges.shape[1] >= 3:
    e = edges.iloc[:, :3].copy()
    e.columns = ["src","dst","weight"]
else:
    raise SystemExit(f"[pagerank_centralities] Colonnes inattendues dans {edges_path}: {list(edges.columns)}")

# coercition numerique et filtre
e["weight"] = pd.to_numeric(e["weight"], errors="coerce").fillna(0.0)
e = e[e["weight"] > 0]

# --- build graph ---
G = nx.DiGraph()
G.add_weighted_edges_from(e[["src","dst","weight"]].itertuples(index=False, name=None))

# PageRank pondéré
pr = pd.Series(nx.pagerank(G, alpha=0.85, weight="weight"), name="pagerank").sort_values(ascending=False)

# Eigenvector (sur graphe non orienté; robuste)
try:
    ev = pd.Series(nx.eigenvector_centrality_numpy(G.to_undirected(), weight="weight"), name="eigenvector_centrality")
except Exception as err:
    print(f"[pagerank_centralities] eigenvector failed: {err}")
    ev = pd.Series(dtype=float, name="eigenvector_centrality")

# Strengths (in/out)
strengths = pd.read_csv(strengths_path)
# standardise: s'assurer d'une colonne 'symbol' pour l'index
if "symbol" not in strengths.columns:
    strengths = strengths.rename(columns={strengths.columns[0]: "symbol"})
strengths = strengths.set_index("symbol")

# fusion et sauvegarde
out = pd.concat([pr, ev, strengths], axis=1)
out.index.name = "symbol"
out = out.sort_values("pagerank", ascending=False)
out.to_csv(out_path)
print(f"[pagerank_centralities] saved → {out_path} (rows={len(out)})")
