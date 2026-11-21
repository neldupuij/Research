#!/usr/bin/env Rscript
# =============================================================================
# 01_download.R
# Télécharge les prix ajustés depuis Yahoo Finance pour tous les tickers
# définis dans config/project.yml et écrit data/processed/prices_raw.csv
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(readr)
  library(dplyr)
  library(lubridate)
  library(quantmod)
  library(purrr)
})

`%||%` <- function(a, b) if (is.null(a)) b else a
logi   <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse = " "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg_path <- file.path("config", "project.yml")
stopifnot(file.exists(cfg_path))
cfg <- yaml::read_yaml(cfg_path)

out_path   <- cfg$paths$prices_raw %||% "data/processed/prices_raw.csv"
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)

tickers    <- cfg$data$tickers
start_date <- as.Date(cfg$data$start_date %||% "2010-01-01")
end_date   <- cfg$data$end_date
if (is.null(end_date)) end_date <- Sys.Date()

blacklist <- cfg$data$blacklist %||% character(0)
if (length(blacklist)) {
  logi("Blacklist (", length(blacklist), "): ", paste(blacklist, collapse = ", "))
  tickers <- setdiff(tickers, blacklist)
}

options(getSymbols.yahoo.warning = FALSE, getSymbols.warning4.0 = FALSE)

logi("01_download.R | wd=", getwd())
logi("Fenêtre : ", start_date, " -> ", end_date)
logi("Tickers  : ", length(tickers), " actifs")

# =============================================================================
# HELPERS
# =============================================================================
fetch_one <- function(sym, from, to) {
  x <- tryCatch(
    suppressWarnings(getSymbols(sym, src = "yahoo", from = from, to = to, auto.assign = FALSE)),
    error = function(e) NULL
  )
  if (is.null(x) || !inherits(x, "xts") || NROW(x) == 0) return(NULL)

  df <- tryCatch(
    data.frame(
      symbol   = sym,
      date     = as.Date(index(x)),
      open     = as.numeric(x[, 1]),
      high     = as.numeric(x[, 2]),
      low      = as.numeric(x[, 3]),
      close    = as.numeric(x[, 4]),
      volume   = as.numeric(x[, 5]),
      adjusted = as.numeric(x[, 6]),
      check.names = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) return(NULL)

  df <- df[df$date >= from & df$date <= to, , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  df
}

# =============================================================================
# TÉLÉCHARGEMENT PAR BATCH (checkpoint par batch pour éviter les pertes)
# =============================================================================
batch_size <- 10
parts_dir  <- dirname(out_path)
failed     <- character(0)
part_idx   <- 0L
accu       <- list()

for (i in seq(1, length(tickers), by = batch_size)) {
  syms <- tickers[i:min(i + batch_size - 1, length(tickers))]
  logi("Batch ", ceiling(i / batch_size), "/", ceiling(length(tickers) / batch_size),
       " (", length(syms), " tickers)")

  for (sym in syms) {
    cat("  •", sym, " ")
    df <- tryCatch(fetch_one(sym, start_date, end_date), error = function(e) {
      cat("-> ERROR:", conditionMessage(e), "\n"); NULL
    })

    if (is.null(df)) {
      cat("-> SKIP\n")
      failed <- c(failed, sym)
    } else {
      cat("-> OK | rows=", nrow(df),
          " (", as.character(min(df$date)), " -> ", as.character(max(df$date)), ")\n", sep = "")
      accu[[length(accu) + 1]] <- df
    }
    flush.console()
  }

  # checkpoint par batch
  if (length(accu) > 0) {
    part_idx  <- part_idx + 1L
    part_df   <- bind_rows(accu)
    part_path <- file.path(parts_dir, sprintf("prices_raw.part%03d.csv", part_idx))
    write_csv(part_df, part_path)
    logi("Checkpoint : ", normalizePath(part_path),
         " (rows=", nrow(part_df), ", syms=", length(unique(part_df$symbol)), ")")
    accu <- list()
  }
}

# =============================================================================
# AGRÉGATION FINALE
# =============================================================================
part_files <- list.files(parts_dir, pattern = "^prices_raw\\.part\\d+\\.csv$", full.names = TRUE)

all_df <- if (length(part_files) > 0) {
  bind_rows(lapply(part_files, readr::read_csv, show_col_types = FALSE))
} else if (length(accu) > 0) {
  bind_rows(accu)
} else {
  NULL
}

if (is.null(all_df) || nrow(all_df) == 0)
  stop("Aucun prix téléchargé. Vérifie la connectivité / les tickers.")

all_df <- all_df %>%
  arrange(symbol, date) %>%
  select(symbol, date, open, high, low, close, adjusted, volume)

write_csv(all_df, out_path)
logi("Ecrit : ", normalizePath(out_path),
     " | rows=", nrow(all_df), " | symbols=", length(unique(all_df$symbol)))

# nettoyage des fichiers .part après agrégation réussie
file.remove(part_files)
logi("Fichiers .part supprimés (", length(part_files), ")")

# =============================================================================
# LOG DES TICKERS MANQUANTS
# =============================================================================
if (length(failed) > 0) {
  failed    <- sort(unique(failed))
  log_dir   <- file.path("outputs", "logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  miss_path <- file.path(log_dir, "missing_tickers.txt")
  writeLines(failed, miss_path)
  logi("Tickers manquants (", length(failed), ") : ", paste(failed, collapse = ", "))
  logi("Log : ", normalizePath(miss_path))
}

logi("Done.")
