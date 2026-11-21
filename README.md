# Volatility Spillovers and Connectedness Across Equities of European Financial Institutions

How do volatility shocks travel through Europe's banking system? This repository contains the full research pipeline, results, paper and slides.

**Paper:** [Volatility Spillovers and Connectedness Across Equities of European Financial Institutions](Paper/Volatility_Spillovers_European_Financial_Institutions.pdf), Nel Choisnet Dupuij and Duc Thi Luu, De Vinci Research Center, October 2026. Presented at FMND 2026 (10th International Workshop on Financial Markets and Nonlinear Dynamics, Paris).

## What the study does

- **VAR-DY connectedness (Diebold–Yilmaz):** rolling VAR on weekly log-returns with generalized forecast-error variance decompositions → total, directional and net spillovers across listed banks, insurers and diversified financial firms.
- **Benchmarks:** DCC-GARCH correlations and correlation networks filtered by a minimum spanning tree (MST).
- **Regimes and breaks:** structural break tests on the total connectedness index, data-driven connectedness regimes, frequency-domain decomposition.
- **Country level:** banks grouped by domicile, within- and between-country spillovers, core–periphery structure.
- **Cross-method comparison:** centrality rankings across VAR-DY, DCC-GARCH and MST networks.

Main findings: persistently high system-wide connectedness with a sharp jump during COVID-19; post-COVID connectedness stays above pre-2019 levels; institutions switch roles between emitter and receiver across frequency bands; a core–periphery hierarchy between national banking systems.

## Pipeline

| Step | Script | Purpose |
|------|--------|---------|
| 01 | `scripts/01_download.R` | Download prices |
| 02 | `scripts/02_process_returns.R` | Clean the panel, daily and weekly log-returns |
| 03 | `scripts/03_select_var_lag.R` | VAR lag selection |
| 04 | `scripts/04_var_dy_connectedness.R` | Rolling VAR-DY connectedness (TCI, directional and net spillovers) |
| 05 | `scripts/05_dcc_garch.R` | DCC-GARCH correlations |
| 06–07 | `scripts/06_cluster_regimes.R`, `06b_…`, `07_cluster_tci_by_country.R` | Connectedness regimes, country clusters |
| 08 | `scripts/08_mst_network.py` | MST correlation networks |
| 09–11 | `scripts/09_compare_var_dcc.py`, `10_…`, `11_…` | VAR vs. DCC vs. MST comparison, country network |
| 12 | `scripts/12_structural_break_test.R` | Structural breaks in the TCI |
| 13–14 | `scripts/13_network_snapshot.py`, `13b_network_periods.R`, `14_network_centralities.py` | Pre-COVID / COVID / post-COVID networks, centralities |
| 15 | `scripts/15_freq_connectedness.R` | Frequency-domain connectedness |
| 16 | `scripts/16_summary_statistics.R` | Summary statistics |
| 19 | `scripts/19_country_spillover_network.py` | Between-country spillover network |

All parameters (sample, VAR lag, rolling window of 156 weeks, forecast horizon, paths) live in [`config/project.yml`](config/project.yml).

## Repository layout

```
config/      project parameters
scripts/     R and Python pipeline (run in numerical order)
data/        processed prices and returns
outputs/     tables (mat/, tables/, summary/) and figures (fig/) for every step
Paper/       LaTeX source of the paper and final PDF
Slides/      LaTeX source of the conference slides
```

A few large intermediate files (R objects and long-format rolling matrices) are not versioned; the scripts regenerate them.

## Requirements

R (ConnectednessApproach, vars, rugarch, rmgarch, strucchange, igraph, ggraph, cluster, quantmod, dplyr/tidyr/readr, yaml) and Python 3 (`pip install -r requirements.txt`: pandas, numpy, scipy, networkx, python-louvain, matplotlib).
