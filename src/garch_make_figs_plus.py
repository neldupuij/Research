# src/python/garch_make_figs_plus.py
import argparse, os, glob
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
plt.switch_backend("Agg")

def load_corr(path: str) -> pd.DataFrame:
    df = pd.read_csv(path)
    if "rowname" in df.columns:
        df = df.set_index("rowname")
    return df.apply(pd.to_numeric, errors="coerce")

def plot_heatmap(R: pd.DataFrame, title: str, out: str):
    M = R.copy().astype(float)
    np.fill_diagonal(M.values, np.nan)  # masque diag
    fig = plt.figure(figsize=(7,6))
    ax = plt.gca()
    im = ax.imshow(M.values, vmin=-1, vmax=1, aspect="auto")
    ax.set_title(title)
    plt.colorbar(im, ax=ax, fraction=0.046)
    plt.tight_layout()
    fig.savefig(out, dpi=200)
    plt.close(fig)

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--reports", required=True)  # ex: reports/exports_garch
    ap.add_argument("--outdir", required=True)   # ex: figures_garch
    ap.add_argument("--stamp",  required=True)   # ex: figures_garch/_ok_plus.stamp
    args = ap.parse_args()

    os.makedirs(args.outdir, exist_ok=True)
    files = sorted(glob.glob(os.path.join(args.reports, "*_correlation_matrix*.csv")))
    if not files:
        print("[WARN] no *_correlation_matrix*.csv found")
    for f in files:
        R = load_corr(f)
        base = os.path.splitext(os.path.basename(f))[0]
        title = base.replace("_", " ")
        out = os.path.join(args.outdir, f"{base}_heatmap_masked.png")
        plot_heatmap(R, title, out)
        print(f"[OK] {out}")

    # stamp
    os.makedirs(os.path.dirname(args.stamp), exist_ok=True)
    with open(args.stamp, "w", encoding="utf-8") as fh:
        fh.write("ok\n")
    print(f"[OK] stamp: {args.stamp}")

if __name__ == "__main__":
    main()
