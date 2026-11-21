#!/usr/bin/env Rscript
# =============================================================================
# 16_summary_statistics.R
# Descriptive statistics for the weekly log-return panel.
#
# Produces:
#   outputs/summary_stats/mat/summary_statistics.csv
#   outputs/summary_stats/tex/tab_summary_statistics.tex   (LaTeX longtable)
#   outputs/summary_stats/tex/tab_summary_panel.tex        (panel-level summary)
# =============================================================================

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(tidyr); library(tibble)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|",
                            paste0(..., collapse=" "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg      <- yaml::read_yaml("config/project.yml")
ret_path <- cfg$paths$returns_weekly %||% "data/processed/returns_weekly.csv"
meta_path <- "data/processed/ticker_countries.csv"
out_base  <- "outputs/summary_stats"

out_mat <- file.path(out_base, "mat"); dir.create(out_mat, recursive=TRUE, showWarnings=FALSE)
out_tex <- file.path(out_base, "tex"); dir.create(out_tex, recursive=TRUE, showWarnings=FALSE)

# =============================================================================
# LOAD DATA
# =============================================================================
ret      <- readr::read_csv(ret_path, show_col_types=FALSE)
ret$date <- as.Date(ret$date)
tickers  <- setdiff(names(ret), "date")
N        <- length(tickers)
T_obs    <- nrow(ret)

meta <- readr::read_csv(meta_path, show_col_types=FALSE) %>%
  rename(ticker=ticker, country=cluster)

msg("Panel: N=", N, " institutions, T=", T_obs, " weekly observations")

# =============================================================================
# COMPUTE STATISTICS PER INSTITUTION
# =============================================================================
skewness_val <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x); m <- mean(x); s <- sd(x)
  if (s < 1e-12 || n < 3) return(NA_real_)
  mean(((x - m) / s)^3)
}
exc_kurtosis <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x); m <- mean(x); s <- sd(x)
  if (s < 1e-12 || n < 4) return(NA_real_)
  mean(((x - m) / s)^4) - 3
}

stats_list <- lapply(tickers, function(tk) {
  r <- ret[[tk]]
  r <- r[!is.na(r)]
  tibble(
    ticker   = tk,
    n_obs    = length(r),
    mean_pct = round(mean(r)  * 100, 4),
    sd_pct   = round(sd(r)    * 100, 4),
    min_pct  = round(min(r)   * 100, 2),
    max_pct  = round(max(r)   * 100, 2),
    skewness = round(skewness_val(r), 3),
    exc_kurt = round(exc_kurtosis(r), 2)
  )
})

stats_df <- bind_rows(stats_list) %>%
  left_join(meta, by="ticker") %>%
  arrange(country, ticker)

readr::write_csv(stats_df, file.path(out_mat, "summary_statistics.csv"))
msg("summary_statistics.csv written.")

# Print panel summary
msg("Panel means — Mean ret: ", round(mean(stats_df$mean_pct), 4), "%",
    " | SD: ", round(mean(stats_df$sd_pct), 3), "%",
    " | Skew: ", round(mean(stats_df$skew, na.rm=TRUE), 3),
    " | Kurt: ", round(mean(stats_df$exc_kurt, na.rm=TRUE), 2))

# =============================================================================
# PANEL-LEVEL SUMMARY (for in-text reference)
# =============================================================================
panel_summary <- tibble(
  Statistic = c("N institutions", "T weeks", "Mean return (\\%, cross-section avg)",
                "Std dev (\\%, cross-section avg)", "Min Std dev (\\%)",
                "Max Std dev (\\%)", "Avg skewness", "Avg excess kurtosis"),
  Value = c(
    as.character(N),
    as.character(T_obs),
    sprintf("%.4f", mean(stats_df$mean_pct)),
    sprintf("%.3f",  mean(stats_df$sd_pct)),
    sprintf("%.3f",  min(stats_df$sd_pct)),
    sprintf("%.3f",  max(stats_df$sd_pct)),
    sprintf("%.3f",  mean(stats_df$skewness, na.rm=TRUE)),
    sprintf("%.2f",  mean(stats_df$exc_kurt,  na.rm=TRUE))
  )
)

# =============================================================================
# LaTeX TABLE 1: INSTITUTION-LEVEL (longtable, for appendix)
# =============================================================================
country_names <- c(
  AT="Austria", BE="Belgium", CH="Switzerland", CZ="Czech Republic",
  DE="Germany", DK="Denmark", ES="Spain", FR="France", GR="Greece",
  IE="Ireland", IT="Italy", NL="Netherlands", NO="Norway", PL="Poland",
  PT="Portugal", RO="Romania", SE="Sweden", UK="United Kingdom"
)

# Institution names (for display)
inst_names <- c(
  A5G.IR="AIB Group", ACA.PA="Cr\\'{e}dit Agricole", ALR.WA="Alior Bank",
  BAER.SW="Julius Baer", BAMI.MI="Banco BPM", BARC.L="Barclays",
  BBVA.MC="BBVA", BCGE.SW="BCG\\`{e}neve", BCP.LS="Millennium BCP",
  BCVN.SW="BCV Vaudoise", BIRG.IR="Bank of Ireland", BKT.MC="Bankinter",
  BMED.MI="Bca Mediolanum", BMPS.MI="Monte dei Paschi", BNP.PA="BNP Paribas",
  BPE.MI="BPER Banca", BPSO.MI="Bca Pop. Sondrio", BRD.RO="BRD-SocGen",
  CABK.MC="CaixaBank", CBG.L="Close Brothers", CBK.DE="Commerzbank",
  DANSKE.CO="Danske Bank", DBK.DE="Deutsche Bank", DNB.OL="DNB Bank",
  EBS.VI="Erste Group", EFGN.SW="EFG International", ETE.AT="Natl Bk Greece",
  EUROB.AT="Eurobank", GLE.PA="Soci\\'{e}t\\'{e} G\\'{e}n\\'{e}rale",
  HSBA.L="HSBC Holdings", ING.WA="ING Bank Slaski", INGA.AS="ING Group",
  INVP.L="Investec", ISP.MI="Intesa Sanpaolo", JYSK.CO="Jyske Bank",
  KBC.BR="KBC Group", KOMB.PR="Komer\\v{c}n\\'{i} banka", LLOY.L="Lloyds",
  MB.MI="Mediobanca", MBK.WA="mBank", MIL.WA="Bank Millennium",
  MING.OL="SpareBank 1 SR", NDA.SE.ST="Nordea Bank",
  `NDA-SE.ST`="Nordea Bank",
  NONG.OL="Sparebanken Vest", NWG.L="NatWest Group", PEO.WA="Bank Pekao",
  PKO.WA="PKO Bank Polski", PTSB.IR="Permanent TSB",
  RBI.VI="Raiffeisen Bank", SAB.MC="Banco Sabadell", SAN.MC="Santander",
  `SEB-A.ST`="SEB", `SHB-A.ST`="Handelsbanken", SPL.WA="Santander PL",
  STAN.L="Standard Chartered", STB.L="Secure Trust Bk",
  `SWED-A.ST`="Swedbank", TLV.RO="Banca Transilvania",
  TPEIR.AT="Piraeus Bank", UBSG.SW="UBS Group", UCG.MI="UniCredit",
  VLK.AS="Van Lanschot", VONN.SW="Vontobel"
)

make_row <- function(df_row) {
  tk   <- df_row$ticker
  name <- inst_names[[tk]]
  if (is.null(name) || is.na(name)) name <- tk
  ctry <- df_row$country %||% ""
  sprintf("%-14s & %-26s & %2s & %4d & %8.4f & %7.3f & %7.2f & %7.2f & %6.3f & %6.2f \\\\",
    tk, name, ctry,
    df_row$n_obs,
    df_row$mean_pct,
    df_row$sd_pct,
    df_row$min_pct,
    df_row$max_pct,
    df_row$skewness,
    df_row$exc_kurt)
}

# Group by country
countries_present <- unique(stats_df$country)
countries_present <- countries_present[!is.na(countries_present)]

tex_lines <- c(
  "% Auto-generated by 16_summary_statistics.R",
  "\\begin{longtable}{llcrrrrrrr}",
  "\\caption{Descriptive statistics of weekly log-returns (\\%). All figures are in \\%.}",
  "\\label{tab:summary_stats} \\\\",
  "\\hline",
  "\\textbf{Ticker} & \\textbf{Institution} & \\textbf{Ctry} & \\textbf{$T$} &",
  "\\textbf{Mean} & \\textbf{Std} & \\textbf{Min} & \\textbf{Max} &",
  "\\textbf{Skew} & \\textbf{Kurt$^{\\dagger}$} \\\\",
  "\\hline",
  "\\endfirsthead",
  "\\multicolumn{10}{l}{\\small\\textit{(continued from previous page)}} \\\\",
  "\\hline",
  "\\textbf{Ticker} & \\textbf{Institution} & \\textbf{Ctry} & \\textbf{$T$} &",
  "\\textbf{Mean} & \\textbf{Std} & \\textbf{Min} & \\textbf{Max} &",
  "\\textbf{Skew} & \\textbf{Kurt$^{\\dagger}$} \\\\",
  "\\hline",
  "\\endhead",
  "\\hline \\multicolumn{10}{r}{\\small\\textit{(continued on next page)}} \\\\",
  "\\endfoot",
  "\\hline",
  paste0("\\multicolumn{10}{l}{\\footnotesize $^{\\dagger}$Excess kurtosis.",
         " Mean and Std in \\%. Panel covers ",
         format(min(ret$date), "%B %Y"), "--",
         format(max(ret$date), "%B %Y"), ".} \\\\"),
  "\\endlastfoot"
)

current_ctry <- ""
for (i in seq_len(nrow(stats_df))) {
  row <- stats_df[i, ]
  ctry_code <- row$country %||% ""
  if (!is.na(ctry_code) && ctry_code != current_ctry) {
    current_ctry <- ctry_code
    ctry_label   <- country_names[ctry_code]
    if (is.na(ctry_label)) ctry_label <- ctry_code
    tex_lines <- c(tex_lines,
      paste0("\\multicolumn{10}{l}{\\textit{", ctry_label, "}} \\\\"))
  }
  tex_lines <- c(tex_lines, make_row(row))
}

# Panel totals row
tex_lines <- c(tex_lines,
  "\\hline",
  sprintf("\\multicolumn{3}{l}{\\textbf{Panel average}} & %d & %8.4f & %7.3f & & & %6.3f & %6.2f \\\\",
    round(mean(stats_df$n_obs)),
    mean(stats_df$mean_pct),
    mean(stats_df$sd_pct),
    mean(stats_df$skewness, na.rm=TRUE),
    mean(stats_df$exc_kurt,  na.rm=TRUE)),
  "\\end{longtable}"
)

writeLines(tex_lines, file.path(out_tex, "tab_summary_statistics.tex"))
msg("tab_summary_statistics.tex written.")

# =============================================================================
# LaTeX TABLE 2: COMPACT PANEL SUMMARY (for data section in main text)
# =============================================================================
compact_lines <- c(
  "% Auto-generated by 16_summary_statistics.R",
  "\\begin{table}[htbp]",
  "  \\centering",
  "  \\caption{Panel summary of weekly log-returns. Statistics are computed",
  "  over the full sample period and then averaged across institutions.",
  "  $N=63$ institutions, 16 countries, weekly frequency.}",
  "  \\label{tab:panel_summary}",
  "  \\begin{tabular}{lrr}",
  "  \\hline",
  "  \\textbf{Statistic} & \\textbf{Cross-section mean} & \\textbf{Cross-section range} \\\\",
  "  \\hline",
  sprintf("  Observations per series & %d & [%d, %d] \\\\",
    round(mean(stats_df$n_obs)), min(stats_df$n_obs), max(stats_df$n_obs)),
  sprintf("  Mean return (\\%%) & %.4f & [%.4f, %.4f] \\\\",
    mean(stats_df$mean_pct), min(stats_df$mean_pct), max(stats_df$mean_pct)),
  sprintf("  Std deviation (\\%%) & %.3f & [%.3f, %.3f] \\\\",
    mean(stats_df$sd_pct), min(stats_df$sd_pct), max(stats_df$sd_pct)),
  sprintf("  Minimum weekly return (\\%%) & %.2f & [%.2f, %.2f] \\\\",
    mean(stats_df$min_pct), min(stats_df$min_pct), max(stats_df$min_pct)),
  sprintf("  Maximum weekly return (\\%%) & %.2f & [%.2f, %.2f] \\\\",
    mean(stats_df$max_pct), min(stats_df$max_pct), max(stats_df$max_pct)),
  sprintf("  Skewness & %.3f & [%.3f, %.3f] \\\\",
    mean(stats_df$skewness, na.rm=TRUE),
    min(stats_df$skewness, na.rm=TRUE),
    max(stats_df$skewness, na.rm=TRUE)),
  sprintf("  Excess kurtosis & %.2f & [%.2f, %.2f] \\\\",
    mean(stats_df$exc_kurt, na.rm=TRUE),
    min(stats_df$exc_kurt, na.rm=TRUE),
    max(stats_df$exc_kurt, na.rm=TRUE)),
  "  \\hline",
  "  \\end{tabular}",
  "\\end{table}"
)

writeLines(compact_lines, file.path(out_tex, "tab_summary_panel.tex"))
msg("tab_summary_panel.tex written.")

msg("Done. All outputs in ", out_base)
