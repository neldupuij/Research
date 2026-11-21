#!/usr/bin/env Rscript
# =============================================================================
# 02_process_returns.R
# Lit prices_raw.csv, applique les filtres qualité, calcule les log-returns
# journaliers puis les agrège en log-returns hebdomadaires.
# Produit :
#   data/processed/returns.csv         (daily, balanced panel)
#   data/processed/returns_weekly.csv  (weekly, balanced panel)
# =============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(purrr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse = " "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg   <- yaml::read_yaml("config/project.yml")
paths <- cfg$paths %||% list()
proc  <- cfg$process %||% list()

prices_path  <- paths$prices_raw     %||% "data/processed/prices_raw.csv"
ret_daily    <- paths$returns_daily  %||% "data/processed/returns.csv"
ret_weekly   <- paths$returns_weekly %||% "data/processed/returns_weekly.csv"

min_obs           <- as.integer(proc$min_obs           %||% 252L)
drop_na_ratio_max <- as.numeric(proc$drop_na_ratio_max %||% 0.20)
keep_min_symbols  <- as.integer(proc$keep_min_symbols  %||% 3L)
min_start         <- as.Date(proc$min_start            %||% "2010-01-01")

msg("02_process_returns.R")
msg("Source  : ", prices_path)
msg("Sorties : ", ret_daily, " | ", ret_weekly)

if (!file.exists(prices_path))
  stop("Introuvable : ", prices_path, " — lance 01_download.R d'abord.")

# =============================================================================
# LECTURE ET FILTRES INITIAUX
# =============================================================================
df <- read_csv(prices_path, show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(symbol, date) %>%
  filter(date >= min_start)

msg("Lignes brutes après filtre date : ", nrow(df))

# Supprimer les symboles avec prix non positifs
bad_nonpos <- df %>%
  group_by(symbol) %>%
  summarise(flag = any(is.finite(adjusted) & adjusted <= 0, na.rm = TRUE), .groups = "drop") %>%
  filter(flag) %>% pull(symbol)

if (length(bad_nonpos)) {
  msg("Drop (prix <= 0) : ", paste(bad_nonpos, collapse = ", "))
  df <- df %>% filter(!symbol %in% bad_nonpos)
}

# =============================================================================
# PIVOT LARGE + FILTRES QUALITÉ
# =============================================================================
wide <- df %>%
  select(symbol, date, adjusted) %>%
  distinct(symbol, date, .keep_all = TRUE) %>%
  pivot_wider(names_from = symbol, values_from = adjusted) %>%
  arrange(date)

cols    <- setdiff(names(wide), "date")
n_total <- nrow(wide)
msg("Fenêtre prix : ", as.character(min(wide$date)), " -> ", as.character(max(wide$date)))
msg("N dates = ", n_total, " | K bruts = ", length(cols))

na_stats <- cols %>% map_df(function(cn) {
  v <- wide[[cn]]
  tibble(symbol   = cn,
         n_na     = sum(is.na(v)),
         na_ratio = mean(is.na(v)),
         n_non_na = sum(!is.na(v)))
})

drop_high_na  <- na_stats %>% filter(na_ratio > drop_na_ratio_max) %>% pull(symbol)
drop_low_obs  <- na_stats %>% filter(n_non_na < min_obs)            %>% pull(symbol)
keep_symbols  <- setdiff(cols, union(drop_high_na, drop_low_obs))

msg("K gardés = ", length(keep_symbols),
    " | Drop >NA% : ", length(drop_high_na),
    " | Drop <min_obs : ", length(drop_low_obs))

if (length(keep_symbols) < keep_min_symbols)
  stop("Qualité insuffisante : seulement ", length(keep_symbols), " symboles gardés.")

# =============================================================================
# LOG-RETURNS JOURNALIERS (panel équilibré)
# =============================================================================
rets_daily <- wide %>%
  select(date, all_of(keep_symbols)) %>%
  arrange(date) %>%
  mutate(across(-date, ~ log(.x) - dplyr::lag(log(.x)))) %>%
  filter(if_all(-date, is.finite))

if (nrow(rets_daily) < (min_obs - 1L))
  stop("Trop peu d'obs daily après équilibrage : ", nrow(rets_daily))

msg("Daily : ", as.character(min(rets_daily$date)), " -> ", as.character(max(rets_daily$date)),
    " | N=", nrow(rets_daily), " | K=", ncol(rets_daily) - 1)

dir.create(dirname(ret_daily), recursive = TRUE, showWarnings = FALSE)
write_csv(rets_daily, ret_daily)
msg("Ecrit : ", ret_daily)

# =============================================================================
# LOG-RETURNS HEBDOMADAIRES (somme des log-returns journaliers)
# =============================================================================
val_cols    <- setdiff(names(rets_daily), "date")
rets_weekly <- rets_daily %>%
  mutate(week = floor_date(date, "week", week_start = 1)) %>%
  group_by(date = week) %>%
  summarise(across(all_of(val_cols), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>%
  arrange(date)

msg("Weekly : ", as.character(min(rets_weekly$date)), " -> ", as.character(max(rets_weekly$date)),
    " | N=", nrow(rets_weekly), " | K=", ncol(rets_weekly) - 1)

dir.create(dirname(ret_weekly), recursive = TRUE, showWarnings = FALSE)
write_csv(rets_weekly, ret_weekly)
msg("Ecrit : ", ret_weekly)

msg("Done.")
