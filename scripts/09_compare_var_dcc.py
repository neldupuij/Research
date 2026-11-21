import os, warnings, math
warnings.filterwarnings("ignore")

import yaml
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from textwrap import dedent
from pandas.tseries.offsets import MonthEnd
from mpl_toolkits.mplot3d import Axes3D

# ==============================================================================
# CONFIG
# ==============================================================================
with open(os.path.join("config", "project.yml"), encoding="utf-8") as f:
    cfg = yaml.safe_load(f)

paths = cfg.get("paths", {})

DY_TCI_PATH    = os.path.join(paths.get("out_var_dy", "outputs/var_dy"),   "mat", "DY_tci.csv")
DY_CT_PATH     = os.path.join(paths.get("out_var_dy", "outputs/var_dy"),   "mat", "DY_CT_last.csv")
DCC_PATH       = os.path.join(paths.get("out_dcc",    "outputs/dcc_garch"),"mat", "dcc_meancorr_rolling.csv")
DCC_EIGEN_PATH = os.path.join(paths.get("out_dcc",    "outputs/dcc_garch"),"mat", "dcc_eigen_centrality.csv")
DCC_CT_PATH    = os.path.join(paths.get("out_dcc",    "outputs/dcc_garch"),"mat", "DCC_CT_last.csv")

out_base = paths.get("out_compare", "outputs/comparison_var_dcc")
mat_dir  = os.path.join(out_base, "mat");     os.makedirs(mat_dir, exist_ok=True)
fig_dir  = os.path.join(out_base, "fig");     os.makedirs(fig_dir, exist_ok=True)
sum_dir  = os.path.join(out_base, "summary"); os.makedirs(sum_dir, exist_ok=True)

def savefig(path):
    plt.tight_layout()
    plt.savefig(path, dpi=220)
    plt.close()

# ==============================================================================
# PLOTS — TCI seul (en premier, independant du reste)
# ==============================================================================
if os.path.exists(DY_TCI_PATH):
    _dy = pd.read_csv(DY_TCI_PATH)
    if "TCI" in _dy.columns and "tci" not in _dy.columns:
        _dy = _dy.rename(columns={"TCI": "tci"})
    _dcol = next((c for c in _dy.columns if "date" in c.lower()), None)
    if _dcol:
        _dy[_dcol] = pd.to_datetime(_dy[_dcol], errors="coerce")
        plt.figure(figsize=(12, 4.6))
        plt.plot(_dy[_dcol], pd.to_numeric(_dy["tci"], errors="coerce"),
                 lw=2, color="#1f77b4")
        plt.title("VAR-DY: Total Connectedness Index (TCI) [%]")
        plt.xlabel("Date"); plt.ylabel("TCI (%)"); plt.grid(alpha=0.3)
        savefig(os.path.join(fig_dir, "VAR_DY_tci_line.png"))
        print("[OK] VAR_DY_tci_line.png")

# ==============================================================================
# CHARGEMENT ET FUSION DY + DCC
# ==============================================================================
for p in [DY_TCI_PATH, DCC_PATH]:
    if not os.path.exists(p):
        raise FileNotFoundError(f"Introuvable : {p}")

def load_dy(path):
    df = pd.read_csv(path)
    if "TCI" in df.columns and "tci" not in df.columns:
        df = df.rename(columns={"TCI": "tci"})
    df["date"] = pd.to_datetime(df.get("date", df.iloc[:, 0]), errors="coerce")
    df["tci"]  = pd.to_numeric(df["tci"], errors="coerce")
    df = df.dropna(subset=["date", "tci"]).sort_values("date")
    df["date"] = df["date"] + MonthEnd(0)
    return df.groupby("date", as_index=False)["tci"].mean().sort_values("date")

def load_dcc(path):
    df = pd.read_csv(path)
    if "end" in df.columns:
        df = df.rename(columns={"end": "date"})
    df["date"]    = pd.to_datetime(df["date"], errors="coerce")
    df["dcc_pct"] = 100.0 * pd.to_numeric(df["dcc_meancorr"], errors="coerce")
    df = df.dropna(subset=["date", "dcc_pct"]).sort_values("date")
    df["date"] = df["date"] + MonthEnd(0)
    return df.groupby("date", as_index=False)["dcc_pct"].mean().sort_values("date")

dy     = load_dy(DY_TCI_PATH)
dcc    = load_dcc(DCC_PATH)
merged = pd.merge(dy, dcc, on="date", how="inner").sort_values("date").reset_index(drop=True)

if len(merged) < 20:
    raise SystemExit(f"Trop peu de points apres fusion : n={len(merged)}")

merged.to_csv(os.path.join(mat_dir, "compare_merged.csv"), index=False)
print(f"Donnees fusionnees : {len(merged)} points")

# ==============================================================================
# CORE — calculs OLS, rolling corr, cross-corr
# ==============================================================================
x = merged["dcc_pct"].to_numpy()
y = merged["tci"].to_numpy()
mask = np.isfinite(x) & np.isfinite(y)
x, y = x[mask], y[mask]
dates = merged.loc[mask, "date"].values

X    = np.c_[np.ones_like(x), x]
beta = np.linalg.lstsq(X, y, rcond=None)[0]
yhat = X @ beta
r    = float(np.corrcoef(x, y)[0, 1])
r2   = r ** 2

dt  = merged["date"].diff().median()
win = 52 if dt <= pd.Timedelta(days=10) else 12
win = max(5, min(win, len(x) // 3))

s_y  = pd.Series(y, index=pd.to_datetime(dates))
s_x  = pd.Series(x, index=pd.to_datetime(dates))
roll = s_y.rolling(win, min_periods=max(5, win // 2)).corr(s_x)

def xcorr(x, y, max_lag):
    x0, y0 = x - x.mean(), y - y.mean()
    lags, ccf = [], []
    for k in range(-max_lag, max_lag + 1):
        if k < 0:   a, b = x0[-k:], y0[:len(y0)+k]
        elif k > 0: a, b = x0[:len(x0)-k], y0[k:]
        else:       a, b = x0, y0
        if len(a) < 10: continue
        va, vb = np.nanvar(a), np.nanvar(b)
        if va == 0 or vb == 0: continue
        lags.append(k)
        ccf.append(float(np.nanmean(a*b) / math.sqrt(va*vb)))
    return np.array(lags), np.array(ccf)

max_lag       = min(60, len(x)//3)
lags, ccf     = xcorr(x, y, max_lag)
best_idx      = int(np.nanargmax(np.abs(ccf)))
best_lag      = int(lags[best_idx])
best_cc       = float(ccf[best_idx])

# ==============================================================================
# PLOTS — comparaison VAR-DY vs DCC
# ==============================================================================
fig, ax = plt.subplots(figsize=(11, 5))
ax.plot(pd.to_datetime(dates), y, lw=2, label="VAR-DY TCI (%)")
ax.plot(pd.to_datetime(dates), x, lw=2, alpha=0.85, label="DCC mean |corr| (%)")
ax.plot(pd.to_datetime(dates), yhat, lw=1.8, ls="--",
        label=f"OLS: TCI = {beta[0]:.2f} + {beta[1]:.2f} * DCC")
ax.set_title("DY vs DCC: same units (%)"); ax.set_xlabel("Date"); ax.set_ylabel("%")
ax.grid(alpha=0.3); ax.legend()
savefig(os.path.join(fig_dir, "DY_DCC_levels_reg.png"))

z_tci = (s_y - s_y.mean()) / s_y.std(ddof=0)
z_dcc = (s_x - s_x.mean()) / s_x.std(ddof=0)
fig, ax = plt.subplots(figsize=(11, 4.8))
ax.plot(z_tci.index, z_tci.values, lw=2, label="TCI (z)")
ax.plot(z_dcc.index, z_dcc.values, lw=2, alpha=0.85, label="DCC% (z)")
ax.axhline(0, lw=1); ax.set_title("DY vs DCC: z-scores")
ax.set_xlabel("Date"); ax.set_ylabel("std. deviation"); ax.grid(alpha=0.3); ax.legend()
savefig(os.path.join(fig_dir, "DY_DCC_zscores.png"))

fig, ax = plt.subplots(figsize=(6.8, 5.6))
ax.scatter(x, y, s=18, alpha=0.65)
xx = np.linspace(np.nanmin(x), np.nanmax(x), 100)
ax.plot(xx, beta[0] + beta[1]*xx, lw=2)
ax.set_title(f"TCI vs DCC%  (r={r:.2f}, R2={r2:.2f})")
ax.set_xlabel("DCC mean |corr| (%)"); ax.set_ylabel("TCI (%)"); ax.grid(alpha=0.3)
savefig(os.path.join(fig_dir, "DY_DCC_scatter_reg.png"))

if len(lags):
    fig, ax = plt.subplots(figsize=(9.6, 4.6))
    ax.stem(lags, ccf, basefmt=" ")
    ax.axvline(0, lw=1); ax.axhline(0, lw=1)
    ax.set_title(f"Cross-correlation (lag>0: DCC leads), max|corr|={abs(best_cc):.2f} at lag={best_lag}")
    ax.set_xlabel("Lag (points)"); ax.set_ylabel("corr")
    savefig(os.path.join(fig_dir, "DY_DCC_xcorr.png"))

fig, ax = plt.subplots(figsize=(11, 4.6))
ax.plot(roll.index, roll.values, lw=2); ax.axhline(0, lw=1)
ax.set_title(f"Rolling correlation (window={win} points) TCI vs DCC%")
ax.set_xlabel("Date"); ax.set_ylabel("corr"); ax.grid(alpha=0.3)
savefig(os.path.join(fig_dir, "DY_DCC_rolling_corr.png"))

print("[OK] Figures DY vs DCC")

# ==============================================================================
# PLOTS — DCC seul
# ==============================================================================
dcc_raw = pd.read_csv(DCC_PATH)
if "end" in dcc_raw.columns:
    dcc_raw = dcc_raw.rename(columns={"end": "date"})
dcc_raw["date"] = pd.to_datetime(dcc_raw["date"], errors="coerce")
dcc_raw["dcc_meancorr"] = pd.to_numeric(dcc_raw["dcc_meancorr"], errors="coerce")
dcc_raw = dcc_raw.dropna(subset=["date", "dcc_meancorr"]).sort_values("date")

plt.figure(figsize=(12, 4.6))
plt.plot(dcc_raw["date"], dcc_raw["dcc_meancorr"], lw=2, color="#ff7f0e")
plt.title("DCC-GARCH: mean absolute correlation (level)")
plt.xlabel("Date"); plt.ylabel("mean |corr|"); plt.grid(alpha=0.3)
savefig(os.path.join(fig_dir, "DCC_mean_abs_corr_line.png"))
print("[OK] DCC_mean_abs_corr_line.png")

# ==============================================================================
# PLOTS — centralite eigenvector DCC top-10
# ==============================================================================
if os.path.exists(DCC_EIGEN_PATH):
    ec = pd.read_csv(DCC_EIGEN_PATH)
    ec["end"] = pd.to_datetime(ec["end"], errors="coerce")
    if {"ticker", "eigen"}.issubset(ec.columns):
        top10 = (ec.groupby("ticker", as_index=False)["eigen"]
                 .mean().dropna()
                 .sort_values("eigen", ascending=False)
                 .head(10)["ticker"].tolist())
        ec2 = ec[ec["ticker"].isin(top10)]
        plt.figure(figsize=(12, 6))
        for tkr, g in ec2.groupby("ticker"):
            plt.plot(g["end"], g["eigen"], lw=1.8, label=tkr)
        plt.title("DCC: eigenvector centrality (top-10 average)")
        plt.xlabel("Date"); plt.ylabel("eigen"); plt.grid(alpha=0.3)
        plt.legend(ncol=2, fontsize=8)
        savefig(os.path.join(fig_dir, "DCC_eigen_top10_timeseries.png"))
        print("[OK] DCC_eigen_top10_timeseries.png")

# ==============================================================================
# PLOTS — heatmap DCC derniere fenetre
# ==============================================================================
if os.path.exists(DCC_CT_PATH):
    m = pd.read_csv(DCC_CT_PATH)
    if "from" in m.columns:
        m = m.set_index("from")
    cols = [c for c in m.columns if c in m.index]
    if len(cols) >= 3:
        M = m.loc[cols, cols].astype(float).values
        plt.figure(figsize=(8.8, 7.3))
        im = plt.imshow(M, aspect="auto", interpolation="nearest")
        plt.title("DCC: correlation (last window)")
        plt.colorbar(im, fraction=0.046, pad=0.04)
        step  = max(1, len(cols)//25)
        ticks = list(range(0, len(cols), step))
        plt.xticks(ticks, [cols[i] for i in ticks], rotation=90, fontsize=7)
        plt.yticks(ticks, [cols[i] for i in ticks], fontsize=7)
        savefig(os.path.join(fig_dir, "DCC_corr_last_heatmap.png"))
        print("[OK] DCC_corr_last_heatmap.png")

# ==============================================================================
# PLOTS — spirales helicoidales VAR-DY (TO / FROM / NET)
# ==============================================================================
def spiral_coords(n, a=0.15, b=0.25):
    k     = np.arange(n, dtype=float)
    theta = k * np.deg2rad(137.508)
    r_    = a + b * theta
    return r_*np.cos(theta), r_*np.sin(theta), 0.12*theta, theta, r_

def scale_sizes(vals, smin=80, smax=900):
    v = np.nan_to_num(np.asarray(vals, float), nan=0.0)
    if v.max() == v.min(): return np.full_like(v, (smin+smax)/2)
    return smin + (v-v.min())/(v.max()-v.min())*(smax-smin)

def helix_plot(series, title, out_png, topn=20, cmap="viridis"):
    s = series.dropna().sort_values(ascending=False).head(min(topn, len(series)))
    if s.empty: return
    names, vals = s.index.to_numpy(), s.values
    x_, y_, _, theta, r_ = spiral_coords(len(s))
    sizes = scale_sizes(np.abs(vals))
    plt.figure(figsize=(9, 9))
    plt.scatter(x_, y_, s=sizes, c=vals, cmap=cmap, alpha=0.9,
                edgecolor="k", linewidths=0.4)
    for xx, yy, lab in zip(x_, y_, names):
        plt.text(xx, yy, lab, fontsize=8, ha="center", va="center")
    t  = np.linspace(0, theta.max(), 600)
    rr = 0.15 + 0.25*t
    plt.plot(rr*np.cos(t), rr*np.sin(t), lw=1.0, alpha=0.6)
    plt.title(f"{title}: spiral 2D"); plt.axis("equal"); plt.axis("off")
    plt.tight_layout()
    plt.savefig(out_png.replace(".png", "_spiral.png"), dpi=220)
    plt.close()
    x_, y_, z_, theta, r_ = spiral_coords(len(s))
    fig = plt.figure(figsize=(10, 8))
    ax  = fig.add_subplot(111, projection="3d")
    p   = ax.scatter(x_, y_, z_, s=scale_sizes(np.abs(vals), 30, 240),
                     c=vals, cmap=cmap, alpha=0.95, depthshade=True)
    for xx, yy, zz, lab in zip(x_, y_, z_, names):
        ax.text(xx, yy, zz, lab, fontsize=7)
    tt = np.linspace(0, theta.max(), 800)
    rr = 0.15+0.25*tt; zz = 0.12*tt
    ax.plot(rr*np.cos(tt), rr*np.sin(tt), zz, lw=1.2, alpha=0.6)
    ax.set_title(f"{title}: 3D helix")
    ax.set_xticks([]); ax.set_yticks([]); ax.set_zticks([])
    fig.colorbar(p, shrink=0.6, pad=0.02)
    plt.tight_layout()
    plt.savefig(out_png, dpi=220)
    plt.close()

if os.path.exists(DY_CT_PATH):
    m = pd.read_csv(DY_CT_PATH)
    if "from" in m.columns:
        m = m.set_index("from")
    cols = [c for c in m.columns if c in m.index]
    if len(cols) >= 3:
        M        = m.loc[cols, cols].astype(float).values
        names    = np.array(cols)
        to_sum   = pd.Series(M.sum(axis=1), index=names)
        from_sum = pd.Series(M.sum(axis=0), index=names)
        net      = to_sum - from_sum
        helix_plot(to_sum,   "VAR-DY: top emitters (TO)",    os.path.join(fig_dir, "VAR_DY_helix_TO.png"),   topn=25)
        helix_plot(from_sum, "VAR-DY: top receivers (FROM)", os.path.join(fig_dir, "VAR_DY_helix_FROM.png"), topn=25)
        helix_plot(net,      "VAR-DY: net (TO-FROM)",        os.path.join(fig_dir, "VAR_DY_helix_NET.png"),  topn=25)
        print("[OK] Spirales helicoidales VAR-DY")

# ==============================================================================
# EXPORT — metriques resume CSV + LaTeX
# ==============================================================================
metrics = {
    "n_obs":             int(len(x)),
    "rolling_window":    int(win),
    "corr_full":         float(r),
    "R2_full":           float(r2),
    "beta_const":        float(beta[0]),
    "beta_slope":        float(beta[1]),
    "best_lag":          int(best_lag),
    "best_lag_corr":     float(best_cc),
    "roll_corr_mean":    float(np.nanmean(roll.values)),
    "roll_corr_median":  float(np.nanmedian(roll.values)),
    "roll_corr_min":     float(np.nanmin(roll.values)),
    "roll_corr_max":     float(np.nanmax(roll.values)),
}
pd.DataFrame([metrics]).to_csv(os.path.join(sum_dir, "metrics.csv"), index=False)

latex = dedent(f"""
\\begin{{table}}[h]
\\centering
\\caption{{VAR-DY (TCI) vs DCC mean |corr| -- metrics}}
\\begin{{tabular}}{{l r}}
\\hline
Observations                  & {metrics["n_obs"]} \\\\
Rolling window (points)       & {metrics["rolling_window"]} \\\\
Full-sample correlation       & {metrics["corr_full"]:.2f} \\\\
$R^2$                         & {metrics["R2_full"]:.2f} \\\\
OLS intercept                 & {metrics["beta_const"]:.2f} \\\\
OLS slope                     & {metrics["beta_slope"]:.2f} \\\\
Best lag                      & {metrics["best_lag"]} \\\\
Correlation at best lag       & {metrics["best_lag_corr"]:.2f} \\\\
Rolling corr mean/median      & {metrics["roll_corr_mean"]:.2f} / {metrics["roll_corr_median"]:.2f} \\\\
Min / Max rolling corr        & {metrics["roll_corr_min"]:.2f} / {metrics["roll_corr_max"]:.2f} \\\\
\\hline
\\end{{tabular}}
\\label{{tab:dy_vs_dcc}}
\\end{{table}}
""").strip()

with open(os.path.join(sum_dir, "metrics.tex"), "w", encoding="utf-8") as f:
    f.write(latex)

print(f"r={r:.3f} | R2={r2:.3f} | best_lag={best_lag}")
print("09_compare_var_dcc.py Done.")
