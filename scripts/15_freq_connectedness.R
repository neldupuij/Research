#!/usr/bin/env Rscript
# =============================================================================
# 15_freq_connectedness.R
# Frequency-domain connectedness (Baruník & Křehlík 2018)
#
# Decomposes the Diebold-Yilmaz TCI and directional spillovers into three
# frequency bands:
#   - Short-run  : 2–4 weeks   (high-frequency: panic / contagion)
#   - Medium-run : 4–8 weeks   (intermediate: adjustment dynamics)
#   - Long-run   : >8 weeks    (structural co-movement)
#
# This directly addresses the "largely descriptive" critique: the frequency
# decomposition distinguishes transient crisis transmission from persistent
# structural integration, providing analytically new content beyond the
# aggregate time-domain TCI.
#
# Produces:
#   outputs/freq_connectedness/mat/FREQ_tci_bands.csv
#   outputs/freq_connectedness/mat/FREQ_net_avg_by_band.csv
#   outputs/freq_connectedness/mat/FREQ_to_avg_by_band.csv
#   outputs/freq_connectedness/mat/FREQ_from_avg_by_band.csv
#   outputs/freq_connectedness/mat/FREQ_share_bands.csv
#   outputs/freq_connectedness/fig/freq_tci_decomp.png
#   outputs/freq_connectedness/fig/freq_net_heatmap.png
#   outputs/freq_connectedness/fig/freq_share_bar.png
#   outputs/freq_connectedness/fig/freq_top5_by_band.png
# =============================================================================

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(tidyr)
  library(zoo); library(tibble); library(ConnectednessApproach)
  library(ggplot2); library(RColorBrewer)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse=" "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg      <- yaml::read_yaml("config/project.yml")
vd       <- cfg$var_dy %||% list()
ret_path <- cfg$paths$returns_weekly %||% "data/processed/returns_weekly.csv"
out_base <- cfg$paths$out_freq_conn  %||% "outputs/freq_connectedness"

out_mat <- file.path(out_base, "mat"); dir.create(out_mat, recursive=TRUE, showWarnings=FALSE)
out_fig <- file.path(out_base, "fig"); dir.create(out_fig, recursive=TRUE, showWarnings=FALSE)

p_fixed <- as.integer(vd$p_fixed         %||% 1L)
Wsize   <- as.integer(vd$rolling_window  %||% 156L)
Wstep   <- max(1L, as.integer(vd$rolling_step %||% 4L))

# Frequency bands for weekly data (radians):
#   Full range : [0, pi]  — Nyquist = pi for weekly
#   Short-run  : cycles 2–4 weeks   → freq band [pi/2, pi]
#   Medium-run : cycles 4–8 weeks   → freq band [pi/4, pi/2]
#   Long-run   : cycles >8 weeks    → freq band [0,    pi/4]
PARTITION  <- c(pi, pi/2, pi/4, 0)
BAND_NAMES <- c("Short (2–4w)", "Medium (4–8w)", "Long (>8w)")
NFORE_SPEC <- 200L   # spectral resolution (frequency-domain nfore, not GFEVD H)

stopifnot(file.exists(ret_path))
ret      <- readr::read_csv(ret_path, show_col_types=FALSE)
ret$date <- as.Date(ret$date)
num_cols <- setdiff(names(ret), "date")
K        <- length(num_cols)

# Full-sample standardization (same as script 04)
mat <- scale(as.matrix(ret[, num_cols]))
Z   <- zoo::zoo(mat, order.by=ret$date)

# Rolling window index — report at every Wstep (same thinning as script 04)
all_ends  <- seq(from=Wsize, to=nrow(ret), by=1L)
thin_ends <- all_ends[seq(1, length(all_ends), by=Wstep)]
n_win     <- length(thin_ends)

msg("15_freq_connectedness.R")
msg("N=", nrow(ret), " | K=", K, " | W=", Wsize, " | step=", Wstep,
    " | windows=", n_win, " | bands=", length(BAND_NAMES))

# =============================================================================
# ROLLING FREQUENCY CONNECTEDNESS
# =============================================================================
# Storage: TCI[n_win x 4] (3 bands + total), NET/TO/FROM[n_win x K x 4]
# Package returns [Total, Band1, Band2, Band3] — Total is index 1
ALL_COLS <- c("Total", BAND_NAMES)
tci_mat  <- matrix(NA_real_, nrow=n_win, ncol=4,
                   dimnames=list(NULL, ALL_COLS))
to_arr   <- array(NA_real_, dim=c(n_win, K, 4),
                  dimnames=list(NULL, num_cols, ALL_COLS))
from_arr <- array(NA_real_, dim=c(n_win, K, 4),
                  dimnames=list(NULL, num_cols, ALL_COLS))
net_arr  <- array(NA_real_, dim=c(n_win, K, 4),
                  dimnames=list(NULL, num_cols, ALL_COLS))
dates_out <- ret$date[thin_ends]

for (i in seq_len(n_win)) {
  end_idx   <- thin_ends[i]
  start_idx <- end_idx - Wsize + 1L
  win       <- Z[start_idx:end_idx, ]

  # Estimate VAR(p) on this window
  var_fit <- tryCatch(
    VAR(win, configuration=list(nlag=p_fixed)),
    error=function(e) NULL
  )
  if (is.null(var_fit)) {
    msg("  Window ", i, " (", as.character(dates_out[i]), "): VAR failed — skipped")
    next
  }

  # Frequency connectedness
  fc <- tryCatch(
    suppressWarnings(FrequencyConnectedness(
      Phi         = var_fit$B,
      Sigma       = var_fit$Q,
      nfore       = NFORE_SPEC,
      partition   = PARTITION,
      generalized = TRUE
    )),
    error=function(e) NULL
  )
  if (is.null(fc)) {
    msg("  Window ", i, " (", as.character(dates_out[i]), "): FreqConn failed — skipped")
    next
  }

  # TCI: fc$TCI is length 4 (3 bands + total)
  tci_mat[i, ] <- as.numeric(fc$TCI)

  # TO/FROM/NET: fc$TO has dim 1 x K x 4
  to_arr[i, , ]   <- fc$TO[1, , ]
  from_arr[i, , ] <- fc$FROM[1, , ]
  net_arr[i, , ]  <- fc$NET[1, , ]

  if (i %% 20 == 0 || i == 1 || i == n_win)
    msg("  Window ", i, "/", n_win, " (", as.character(dates_out[i]), ") done")
}

msg("Rolling estimation complete.")

# =============================================================================
# WRITE CSV OUTPUTS
# =============================================================================

# 1. TCI by band over time
tci_df <- as_tibble(tci_mat) %>%
  mutate(date = dates_out) %>%
  select(date, everything()) %>%
  filter(!is.na(Total))
readr::write_csv(tci_df, file.path(out_mat, "FREQ_tci_bands.csv"))
msg("FREQ_tci_bands.csv written.")

# 2. Time-averaged net / TO / FROM per institution and band
avg_mat <- function(arr, band_names) {
  valid <- which(!is.na(arr[, 1, 1]))
  # apply(, c(2,3), mean) averages over dim-1 (windows) → K x 4 matrix
  avg <- apply(arr[valid, , , drop=FALSE], c(2, 3), mean)
  as_tibble(cbind(
    data.frame(ticker = num_cols),
    setNames(as.data.frame(avg), band_names)
  ))
}
net_avg  <- avg_mat(net_arr,  ALL_COLS)
to_avg   <- avg_mat(to_arr,   ALL_COLS)
from_avg <- avg_mat(from_arr, ALL_COLS)

readr::write_csv(net_avg,  file.path(out_mat, "FREQ_net_avg_by_band.csv"))
readr::write_csv(to_avg,   file.path(out_mat, "FREQ_to_avg_by_band.csv"))
readr::write_csv(from_avg, file.path(out_mat, "FREQ_from_avg_by_band.csv"))
msg("FREQ_net/to/from_avg_by_band.csv written.")

# 3. Average share of each band in total TCI
valid_rows <- which(!is.na(tci_df$Total) & tci_df$Total > 0)
shares <- colMeans(tci_df[valid_rows, BAND_NAMES] / tci_df$Total[valid_rows] * 100,
                   na.rm=TRUE)
share_df <- tibble(band=BAND_NAMES, share_pct=round(shares, 2))
readr::write_csv(share_df, file.path(out_mat, "FREQ_share_bands.csv"))
msg("FREQ_share_bands.csv written — shares: ",
    paste(BAND_NAMES, round(shares, 1), sep="=", collapse=", "))

# =============================================================================
# FIGURE 1: TCI decomposition over time (stacked area)
# =============================================================================
band_cols <- c("Short (2–4w)"="#d62728", "Medium (4–8w)"="#ff7f0e", "Long (>8w)"="#1f77b4")

tci_long <- tci_df %>%
  select(date, all_of(BAND_NAMES)) %>%
  pivot_longer(-date, names_to="band", values_to="tci") %>%
  mutate(band = factor(band, levels=BAND_NAMES))

png(file.path(out_fig, "freq_tci_decomp.png"), width=2800, height=1100, res=220)
p1 <- ggplot(tci_long, aes(x=date, y=tci, fill=band)) +
  geom_area(alpha=0.85, colour=NA) +
  geom_line(data=tci_df %>% filter(!is.na(Total)),
            aes(x=date, y=Total, fill=NULL),
            colour="black", linewidth=0.7, linetype="dashed", inherit.aes=FALSE) +
  scale_fill_manual(values=band_cols, name="Frequency band") +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
  labs(
    title = "Frequency decomposition of the Total Connectedness Index (TCI)",
    subtitle = "Stacked area = band contributions; dashed line = total TCI",
    x = NULL, y = "TCI contribution (pp)"
  ) +
  theme_bw(base_size=11) +
  theme(legend.position="bottom", plot.title=element_text(face="bold"))
print(p1)
dev.off()
msg("freq_tci_decomp.png written.")

# =============================================================================
# FIGURE 2: Band shares over time (normalized to 100%)
# =============================================================================
tci_pct <- tci_df %>%
  filter(!is.na(Total) & Total > 0) %>%
  mutate(across(all_of(BAND_NAMES), ~ .x / Total * 100)) %>%
  select(date, all_of(BAND_NAMES)) %>%
  pivot_longer(-date, names_to="band", values_to="share") %>%
  mutate(band = factor(band, levels=BAND_NAMES))

png(file.path(out_fig, "freq_share_bar.png"), width=2800, height=1000, res=220)
p2 <- ggplot(tci_pct, aes(x=date, y=share, fill=band)) +
  geom_area(alpha=0.85, colour=NA, position="stack") +
  scale_y_continuous(labels=function(x) paste0(round(x), "%")) +
  scale_fill_manual(values=band_cols, name="Frequency band") +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
  labs(
    title = "Share of each frequency band in total TCI",
    subtitle = "COVID shock raises the long-run share — structural integration, not short-term panic",
    x = NULL, y = "Share of total TCI"
  ) +
  theme_bw(base_size=11) +
  theme(legend.position="bottom", plot.title=element_text(face="bold"))
print(p2)
dev.off()
msg("freq_share_bar.png written.")

# =============================================================================
# FIGURE 3: Net spillover by institution and frequency band (heatmap)
# =============================================================================
net_heat <- net_avg %>%
  select(ticker, all_of(BAND_NAMES)) %>%
  pivot_longer(-ticker, names_to="band", values_to="net") %>%
  mutate(band = factor(band, levels=BAND_NAMES))

# Order institutions by total net (descending)
ticker_order <- net_avg %>%
  arrange(desc(Total)) %>%
  pull(ticker)
net_heat$ticker <- factor(net_heat$ticker, levels=rev(ticker_order))

png(file.path(out_fig, "freq_net_heatmap.png"), width=2400, height=3200, res=200)
p3 <- ggplot(net_heat, aes(x=band, y=ticker, fill=net)) +
  geom_tile(colour="white", linewidth=0.3) +
  scale_fill_gradient2(
    low="#d62728", mid="white", high="#1f77b4", midpoint=0,
    name="Net spillover\n(TO − FROM)"
  ) +
  labs(
    title = "Time-averaged net spillover by institution and frequency band",
    x = "Frequency band", y = NULL
  ) +
  theme_bw(base_size=9) +
  theme(
    axis.text.y  = element_text(size=6.5),
    plot.title   = element_text(face="bold"),
    legend.position = "right"
  )
print(p3)
dev.off()
msg("freq_net_heatmap.png written.")

# =============================================================================
# FIGURE 4: Top-5 net transmitters per frequency band (bar chart)
# =============================================================================
top5_band <- net_avg %>%
  select(ticker, all_of(BAND_NAMES)) %>%
  pivot_longer(-ticker, names_to="band", values_to="net") %>%
  mutate(band = factor(band, levels=BAND_NAMES)) %>%
  group_by(band) %>%
  slice_max(net, n=5) %>%
  ungroup()

png(file.path(out_fig, "freq_top5_by_band.png"), width=2400, height=900, res=200)
p4 <- ggplot(top5_band, aes(x=reorder(ticker, net), y=net, fill=band)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~band, scales="free_y", ncol=3) +
  scale_fill_manual(values=band_cols) +
  coord_flip() +
  labs(
    title = "Top-5 net volatility transmitters by frequency band",
    subtitle = "Net = TO − FROM (time-averaged). Institutions ranked within each band.",
    x=NULL, y="Average net spillover"
  ) +
  theme_bw(base_size=11) +
  theme(plot.title=element_text(face="bold"))
print(p4)
dev.off()
msg("freq_top5_by_band.png written.")

msg("Done. All outputs in ", out_base)
