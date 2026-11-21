#!/usr/bin/env Rscript
# =============================================================================
# 07_cluster_tci_by_country.R
# Calcule le TCI rolling intra-pays en relançant un VAR-DY sur le sous-panel
# de chaque pays. Les dates sont alignées sur celles du TCI global (04_var_dy).
# Produit :
#   outputs/cluster_regimes/mat/cluster_tci_rolling.csv
# =============================================================================

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(tidyr)
  library(lubridate); library(zoo); library(ConnectednessApproach)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse = " "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg      <- yaml::read_yaml("config/project.yml")
vd       <- cfg$var_dy %||% list()
ret_path <- cfg$paths$returns_weekly %||% "data/processed/returns_weekly.csv"
out_base <- cfg$paths$out_cluster    %||% "outputs/cluster_regimes"

MAP_PATH <- "data/processed/ticker_countries.csv"
DY_TCI   <- file.path(cfg$paths$out_var_dy %||% "outputs/var_dy", "mat", "DY_tci.csv")
out_dir  <- file.path(out_base, "mat")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

H       <- as.integer(vd$forecast_horizon %||% 10L)
p_fixed <- as.integer(vd$p_fixed          %||% 1L)
Wsize   <- as.integer(vd$rolling_window   %||% 156L)

stopifnot(file.exists(ret_path), file.exists(MAP_PATH), file.exists(DY_TCI))
msg("07_cluster_tci_by_country.R")
msg("W=", Wsize, " | H=", H, " | p=", p_fixed)

# =============================================================================
# CHARGEMENT
# =============================================================================
ret      <- readr::read_csv(ret_path, show_col_types = FALSE)
ret$date <- as.Date(ret$date)
tickers  <- setdiff(names(ret), "date")
Zfull    <- zoo::zoo(ret[, tickers, drop = FALSE], order.by = ret$date)

map <- readr::read_csv(MAP_PATH, show_col_types = FALSE) %>%
  rename(ticker = 1, cluster = 2) %>%
  mutate(ticker = as.character(ticker), cluster = as.character(cluster))

ref_dates <- readr::read_csv(DY_TCI, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(window_id = row_number()) %>%
  select(window_id, date)

msg("N=", nrow(ret), " | K=", length(tickers),
    " | fenêtres de référence=", nrow(ref_dates))

# =============================================================================
# TCI INTRA-PAYS PAR CLUSTER
# =============================================================================
clusters <- sort(unique(map$cluster))
out_list <- list()

for (cl in clusters) {
  tick_cl <- intersect(map %>% filter(cluster == cl) %>% pull(ticker), tickers)
  Kc <- length(tick_cl)
  if (Kc < 2) next

  Zc <- Zfull[, tick_cl, drop = FALSE]
  Wc <- Wsize
  if (nrow(Zc) <= Wc) Wc <- max(5L, min(Wc, nrow(Zc) - 1L))
  if (Wc < 5L) next

  msg("Cluster ", cl, " : K=", Kc, " | W=", Wc)

  res <- tryCatch({
    CA <- ConnectednessApproach(
      x = Zc, nlag = p_fixed, nfore = H,
      window.size = Wc, model = "VAR",
      connectedness = "Time", corrected = FALSE
    )

    TT <- NULL
    if (!is.null(CA$TCI)) TT <- CA$TCI
    if (is.null(TT) && !is.null(CA$CT) && !is.null(CA$CT$TCI)) TT <- CA$CT$TCI
    if (is.null(TT)) TT <- ConnectednessApproach::TimeConnectedness(CA)$TCI

    df <- if (inherits(TT, "zoo")) {
      tibble(date = as.Date(zoo::index(TT)), TCI = as.numeric(zoo::coredata(TT)))
    } else {
      tibble(date = tail(ret$date, length(TT)), TCI = as.numeric(TT))
    }

    df %>%
      inner_join(ref_dates, by = "date") %>%
      arrange(window_id) %>%
      mutate(cluster = cl, n_banks = Kc) %>%
      select(cluster, n_banks, window_id, date, TCI)

  }, error = function(e) {
    msg("  [SKIP] ", cl, " : ", e$message); NULL
  })

  if (!is.null(res) && nrow(res) > 0) out_list[[cl]] <- res
}

# =============================================================================
# EXPORT
# =============================================================================
out <- bind_rows(out_list) %>% arrange(cluster, window_id)
out_path <- file.path(out_dir, "cluster_tci_rolling.csv")
readr::write_csv(out, out_path)
msg("Ecrit : ", out_path,
    " | clusters=", length(unique(out$cluster)),
    " | lignes=", nrow(out))
msg("Done.")
