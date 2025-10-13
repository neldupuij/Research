import pandas as pd
import matplotlib.pyplot as plt
from pathlib import Path
import sys

# __file__ = .../src/python/nn/render_nn.py
# parents[0]=nn, [1]=python, [2]=src, [3]=Research (racine)
ROOT = Path(__file__).resolve().parents[3]
nn_dir = ROOT / "reports" / "exports_nn"
fig_dir = ROOT / "figures_nn"
fig_dir.mkdir(parents=True, exist_ok=True)

cent_path = nn_dir / "centralities_last_window.csv"
if not cent_path.exists():
    sys.exit(f"[render_nn] Introuvable: {cent_path} — lance d'abord pagerank_centralities.py (make nn)")

df = pd.read_csv(cent_path)

# Assurer la présence de 'symbol' en colonne (si c'est l'index)
if "symbol" not in df.columns:
    # si le CSV a écrit l'index sans nom, il peut apparaître comme 'Unnamed: 0'
    if "Unnamed: 0" in df.columns:
        df = df.rename(columns={"Unnamed: 0": "symbol"})
    else:
        # sinon, tente de reconstruire depuis l'index
        df.index.name = "symbol"
        df = df.reset_index()

# Sanity: colonnes pagerank/centralité
for col in ["pagerank"]:
    if col not in df.columns:
        sys.exit(f"[render_nn] Colonne manquante '{col}' dans {cent_path}")

# Top 15 par PageRank
top = df.sort_values("pagerank", ascending=False).head(15).copy()
top = top.iloc[::-1]  # pour barh ascendante

plt.figure(figsize=(8,6))
plt.barh(top["symbol"], top["pagerank"])
plt.title("Top 15 PageRank — dernier window")
plt.xlabel("PageRank (pondéré)")
plt.tight_layout()
out_path = fig_dir / "centrality_pagerank_top15.png"
plt.savefig(out_path, dpi=150)
plt.close()
print(f"[render_nn] figure saved → {out_path}")
