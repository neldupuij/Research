import pandas as pd
import numpy as np
import networkx as nx
from pathlib import Path
import sys

# __file__ = .../src/python/nn/prepare_graph_series.py
# parents[0]=nn, [1]=python, [2]=src, [3]=Research (racine)
ROOT = Path(__file__).resolve().parents[3]
mat_path = ROOT / "reports" / "exports" / "spillover_matrix_kxk.csv"
out_dir = ROOT / "reports" / "exports_nn"
out_dir.mkdir(parents=True, exist_ok=True)

if not mat_path.exists():
    sys.exit(f"[prepare_graph_series] Fichier introuvable: {mat_path}\n"
             f"→ Lance d'abord la pipeline principale: make run")

df = pd.read_csv(mat_path, index_col=0)
# Retirer l'auto-influence pour les arêtes
np.fill_diagonal(df.values, 0.0)

# Edges i->j pondérés (en %)
edges = (
    df.stack().reset_index()
      .rename(columns={"level_0":"src","level_1":"dst",0:"weight"})
)
edges = edges[edges["weight"] > 0].sort_values("weight", ascending=False)

# Strengths directionnels (somme ligne/colonne)
out_strength = df.sum(axis=1).rename("out_strength")
in_strength  = df.sum(axis=0).rename("in_strength")
strengths = pd.concat([out_strength, in_strength], axis=1)
strengths.index.name = "symbol"

# Sauvegardes
edges.to_csv(out_dir / "edges_last_window.csv", index=False)
strengths.to_csv(out_dir / "strengths_last_window.csv")
print(f"[prepare_graph_series] edges={len(edges):,} → {out_dir/'edges_last_window.csv'}")
print(f"[prepare_graph_series] strengths → {out_dir/'strengths_last_window.csv'}")
