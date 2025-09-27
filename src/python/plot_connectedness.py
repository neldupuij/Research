#!/usr/bin/env python3
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.dates import YearLocator, DateFormatter

FIG_DIR = Path("figures")
FIG_DIR.mkdir(parents=True, exist_ok=True)

def save(fig, name):
    fig.tight_layout()
    fig.savefig(FIG_DIR / f"{name}.png", dpi=300)
    fig.savefig(FIG_DIR / f"{name}.pdf", dpi=300)
    plt.close(fig)

# ---------------- Helpers ----------------

def _looks_like_ticker(s: str) -> bool:
    s = str(s).strip()
    return any(c.isalpha() for c in s) and len(s) <= 15

def _robust_kxk_read(path: str) -> pd.DataFrame:
    """
    Robustly read a spillover k×k CSV, even if:
      - row labels were not written,
      - first column is numeric,
      - the matrix is not square on read.
    Strategy:
      1) Try to set an object/label column as index, else proceed without.
      2) Keep only numeric data columns.
      3) Force a square matrix using top-left k×k where k = min(n_rows, n_numeric_cols).
      4) If we have ticker-like column names, use them as both rows and columns.
    """
    df0 = pd.read_csv(path)

    # Step 1: try to find a label column
    label_col = None
    for c in df0.columns:
        if df0[c].dtype == object:
            share_letters = pd.Series(df0[c].astype(str).str.contains(r"[A-Za-z]")).mean()
            if share_letters >= 0.7:
                label_col = c
                break

    if label_col is not None:
        df0[label_col] = df0[label_col].astype(str).str.strip()
        df = df0.set_index(label_col, drop=True)
    else:
        df = df0.copy()

    # Step 2: keep only numeric columns (spillover values)
    num_cols = [c for c in df.columns if pd.api.types.is_numeric_dtype(df[c])]
    if not num_cols:
        raise ValueError("No numeric columns found in k×k CSV.")
    M = df[num_cols].copy()

    # Step 3: force square
    # If we have ticker-like column names, prefer those as labels
    col_labels = [c for c in M.columns if _looks_like_ticker(c)]
    k = min(M.shape[0], M.shape[1])
    if k < 2:
        raise ValueError(f"k×k matrix too small after numeric filter: shape={M.shape}")

    # top-left square
    M = M.iloc[:k, :k]

    # Step 4: set final labels
    # Prefer column names if they look like tickers, else generate generic labels
    if len(col_labels) >= k:
        labels = [str(x).strip() for x in M.columns[:k]]
    else:
        labels = [f"V{i+1}" for i in range(k)]
    M.columns = labels
    if label_col is not None and df.index.dtype == object and df.index.size >= k:
        # Try to reuse index if it looks reasonable
        idx = [str(x).strip() for x in df.index[:k]]
        if sum(_looks_like_ticker(x) for x in idx) >= k * 0.7:
            M.index = idx
        else:
            M.index = labels
    else:
        M.index = labels

    # final safety: ensure exact square with same labels on both axes
    common = [l for l in M.columns if l in M.index]
    if len(common) < k:
        # overwrite to make rows == cols
        M = pd.DataFrame(M.values[:len(common), :len(common)], index=common, columns=common)

    return M

def _safe_order(M: pd.DataFrame, order_symbols: list[str]) -> pd.DataFrame:
    order_symbols = [str(s).strip() for s in order_symbols]
    common = [s for s in order_symbols if s in M.index]
    if len(common) >= 2:
        return M.loc[common, common]
    return M

# ---------------- 1) Rolling TCI ----------------
try:
    tr = pd.read_csv("reports/exports/tci_rolling.csv", parse_dates=["date_end"])
    if len(tr):
        fig, ax = plt.subplots(figsize=(10, 4.5), dpi=150)
        ax.plot(tr["date_end"], tr["TCI"], lw=2)
        w = int(tr["window"].iloc[0]); s = int(tr["step"].iloc[0]); H = int(tr["H"].iloc[0])
        ax.set_title(f"TCI (rolling window) — W={w}, step={s}, H={H}")
        ax.set_ylabel("TCI (%)"); ax.set_xlabel("Window end date")
        ax.grid(True, alpha=0.25)
        ax.xaxis.set_major_locator(YearLocator(1))
        ax.xaxis.set_major_formatter(DateFormatter("%Y"))
        save(fig, "tci_rolling")
except Exception as e:
    print(f"[plot] skip rolling TCI: {e}")

# ---------------- 2) Net spillovers (latest window) ----------------
try:
    df = pd.read_csv("reports/exports/net_spillovers.csv")
    if len(df):
        df = df.sort_values("NET")
        height = max(5, 0.24 * len(df) + 2)
        fig, ax = plt.subplots(figsize=(10, height), dpi=150)
        colors = np.where(df["NET"] >= 0, "#1f77b4", "#d62728")
        ax.barh(df["symbol"], df["NET"], color=colors, edgecolor="none")
        ax.axvline(0, lw=0.8, color="black", alpha=0.5)
        ax.set_xlabel("NET (%)")
        ax.set_title("Net Spillovers (TO − FROM) — latest window")
        ax.grid(True, axis="x", alpha=0.25)
        save(fig, "net_spillovers")
except Exception as e:
    print(f"[plot] skip net bars: {e}")

# ---------------- 3) Heatmap k×k (latest window) ----------------
try:
    M = _robust_kxk_read("reports/exports/spillover_matrix_kxk.csv")
    # Optional: align to NET ordering
    try:
        order = pd.read_csv("reports/exports/net_spillovers.csv")["symbol"].tolist()
        M = _safe_order(M, order)
    except Exception as e:
        print(f"[plot] cannot align heatmap order: {e}")

    k = M.shape[0]
    fig, ax = plt.subplots(figsize=(min(22, max(8, k * 0.25)),) * 2, dpi=150)
    im = ax.imshow(M.values, cmap="viridis", aspect="equal", interpolation="nearest")

    ax.set_xticks(np.arange(k)); ax.set_yticks(np.arange(k))
    ax.set_xticklabels(M.columns, rotation=90, fontsize=6)
    ax.set_yticklabels(M.index, fontsize=6)

    cbar = fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    cbar.set_label("FEVD share (%)")

    if k <= 25:
        mmean = np.nanmean(M.values)
        for i in range(k):
            for j in range(k):
                val = M.values[i, j]
                ax.text(j, i, f"{val:.1f}", ha="center", va="center",
                        fontsize=6, color=("white" if val > mmean else "black"))

    ax.set_title("Spillover Matrix (k×k) — latest window")
    ax.grid(False)
    save(fig, "spillover_matrix_heatmap")
except Exception as e:
    print(f"[plot] skip heatmap: {e}")