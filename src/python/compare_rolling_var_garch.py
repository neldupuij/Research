import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

def norm_datecol(df):
    for c in ["end", "date_end", "date", "period_end", "window_end"]:
        if c in df.columns:
            df = df.rename(columns={c: "end"})
            break
    if "end" not in df.columns:
        raise RuntimeError("Pas de colonne date dans " + repr(df.columns.tolist()))
    df["end"] = pd.to_datetime(df["end"])
    return df

def ensure_figdir():
    Path("figures_nn").mkdir(parents=True, exist_ok=True)

# --- Paths ---
VAR_TCI  = Path("reports/exports_var/var_rolling_tci.csv")
VAR_NET  = Path("reports/exports_var/var_rolling_varnet.csv")
DCC_MEAN = Path("reports/exports_garch/dcc_meancorr_rolling.csv")
DCC_EIG  = Path("reports/exports_garch/dcc_eigen_centrality.csv")

# --- Load VAR TCI ---
if not VAR_TCI.exists():
    fb = Path("reports/exports/tci_rolling.csv")
    if fb.exists():
        tci = pd.read_csv(fb)
        if "date_end" in tci.columns:
            tci = tci.rename(columns={"date_end": "end"})
        if "window" in tci.columns and "start" not in tci.columns:
            tci["start"] = pd.to_datetime(tci["end"]) - pd.to_timedelta(tci["window"] - 1, unit="D")
        if "TCI" not in tci.columns and "value" in tci.columns:
            tci = tci.rename(columns={"value": "TCI"})
    else:
        raise SystemExit("Aucun TCI trouvÃ© (ni reports/exports_var/var_rolling_tci.csv ni reports/exports/tci_rolling.csv).")
else:
    tci = pd.read_csv(VAR_TCI)

tci = norm_datecol(tci)
if "TCI" not in tci.columns:
    raise SystemExit("La colonne 'TCI' est absente.")

ensure_figdir()

# --- FIGURE 1 : TCI (VAR-FEVD) vs Mean |corr| (DCC) ---
if DCC_MEAN.exists():
    dmean = pd.read_csv(DCC_MEAN)
    dmean = norm_datecol(dmean)
    if "meancorr" not in dmean.columns:
        cands = [c for c in dmean.columns if "mean" in c and "corr" in c]
        if cands:
            dmean = dmean.rename(columns={cands[0]: "meancorr"})
        else:
            raise SystemExit("Colonne 'meancorr' introuvable dans DCC_MEAN.")

    tm = tci.merge(dmean[["end", "meancorr"]], on="end", how="inner").sort_values("end")

    fig, ax1 = plt.subplots(figsize=(9, 6))
    ax2 = ax1.twinx()

    ax1.plot(tm["end"], tm["TCI"], color="tab:blue", label="TCI (VAR-FEVD)", linewidth=2)
    ax1.set_ylabel("TCI (%)", color="tab:blue")
    ax1.tick_params(axis='y', labelcolor='tab:blue')
    ax1.set_ylim(0, 100)

    # DCC mean |corr| en pourcentage
    ax2.plot(tm["end"], tm["meancorr"] * 100, color="tab:orange", label="Mean |corr| (DCC, %)", linestyle="--")
    ax2.set_ylabel("Mean |corr| (%)", color="tab:orange")
    ax2.tick_params(axis='y', labelcolor='tab:orange')
    ax2.set_ylim(0, 100)

    ax1.set_title("TCI (VAR-FEVD) vs DCC mean |corr| (rolling)")
    ax1.set_xlabel("Date (window end)")

    lines, labels = ax1.get_legend_handles_labels()
    lines2, labels2 = ax2.get_legend_handles_labels()
    ax1.legend(lines + lines2, labels + labels2, loc="lower right")

    fig.tight_layout()
    plt.savefig("figures_nn/rolling_tci_vs_meancorr.png", dpi=150)
    plt.close(fig)

else:
    print("[WARN] DCC_MEAN absent â†’ figure 1 ignorÃ©e.")


# --- FIGURE 2 : corr(var_net, eigen) ---
if VAR_NET.exists() and DCC_EIG.exists():
    vnet = pd.read_csv(VAR_NET)
    vnet = norm_datecol(vnet)
    if "symbol" in vnet.columns and "ticker" not in vnet.columns:
        vnet = vnet.rename(columns={"symbol": "ticker"})
    if "var_net" not in vnet.columns and {"TO", "FROM"}.issubset(vnet.columns):
        vnet["var_net"] = vnet["TO"] - vnet["FROM"]

    eig = pd.read_csv(DCC_EIG)
    eig = norm_datecol(eig)
    if "ticker" not in eig.columns:
        for c in ["symbol", "name", "asset"]:
            if c in eig.columns:
                eig = eig.rename(columns={c: "ticker"})
                break
    if "eigen" not in eig.columns:
        cands = [c for c in eig.columns if "eigen" in c]
        if cands:
            eig = eig.rename(columns={cands[0]: "eigen"})
        else:
            raise SystemExit("Aucune colonne 'eigen' dans DCC_EIG.")

    rows = []
    for end, g in eig.groupby("end"):
        left = vnet[vnet["end"] == end][["ticker", "var_net"]]
        right = g[["ticker", "eigen"]]
        M = left.merge(right, on="ticker", how="inner")
        if len(M) >= 3:
            r = np.corrcoef(M["var_net"], M["eigen"])[0, 1]
            if np.isfinite(r):
                rows.append({"end": end, "rho_eigen": r})

    if rows:
        rho_t = pd.DataFrame(rows).sort_values("end")
        plt.figure(figsize=(8, 5))
        plt.plot(rho_t["end"], rho_t["rho_eigen"], label="Ï(var_net, eigen)", linewidth=2)
        plt.axhline(0.0, linestyle="--", color="black", lw=1)
        plt.ylim(-1, 1)
        plt.legend()
        plt.title("Correlation over time: var_net (VAR) vs eigen (DCC)")
        plt.xlabel("Date (window end)")
        plt.tight_layout()
        plt.savefig("figures_nn/rolling_rho_varnet_vs_dcc.png", dpi=150)
        plt.close()
    else:
        print("[WARN] Pas assez de donnÃ©es pour Ï(var_net, eigen).")
else:
    print("[WARN] Fichiers manquants pour figure 2 â†’ ignorÃ©e.")

# --- RÃ©sumÃ© final ---
print("Figures gÃ©nÃ©rÃ©es :")
for f in [
    "figures_nn/rolling_tci_vs_meancorr.png",
    "figures_nn/rolling_rho_varnet_vs_dcc.png",
]:
    if Path(f).exists():
        print(" -", f)
