#!/usr/bin/env Rscript
# =============================================================================
# 06_cluster_regimes.R
# Décomposition de la connectedness VAR-DY par cluster (pays) à partir
# de la matrice GFEVD rolling (DY_theta_long.csv).
# Produit :
#   outputs/cluster_regimes/mat/system_connectedness.csv
#   outputs/cluster_regimes/mat/cluster_components.csv
#   outputs/cluster_regimes/mat/cluster_spillover_c2c.csv
#   outputs/cluster_regimes/mat/cluster_net.csv
# =============================================================================

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(tidyr); library(stringr)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse = " "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg      <- yaml::read_yaml("config/project.yml")
out_base <- cfg$paths$out_cluster %||% "outputs/cluster_regimes"

THETA_PATH <- file.path(cfg$paths$out_var_dy %||% "outputs/var_dy", "mat", "DY_theta_long.csv")
MAP_PATH   <- "data/processed/ticker_countries.csv"
out_dir    <- file.path(out_base, "mat")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

stopifnot(file.exists(THETA_PATH), file.exists(MAP_PATH))
msg("06_cluster_regimes.R")
msg("Theta : ", THETA_PATH)

# =============================================================================
# CHARGEMENT
# =============================================================================
theta <- readr::read_csv(THETA_PATH, show_col_types = FALSE) %>%
  mutate(date  = as.Date(date),
         i     = as.character(i),
         j     = as.character(j),
         theta = as.numeric(theta)) %>%
  filter(!is.na(date), !is.na(i), !is.na(j), is.finite(theta))

if (!all(c("date", "window_id", "i", "j", "theta") %in% names(theta)))
  stop("DY_theta_long.csv doit contenir : date, window_id, i, j, theta")

map <- readr::read_csv(MAP_PATH, show_col_types = FALSE) %>%
  rename(ticker = 1, cluster = 2) %>%
  mutate(ticker = as.character(ticker), cluster = as.character(cluster)) %>%
  filter(!is.na(ticker), !is.na(cluster)) %>%
  distinct(ticker, .keep_all = TRUE)

ticks <- intersect(unique(theta$i), unique(theta$j))
map2  <- map %>% filter(ticker %in% ticks)

missing <- setdiff(ticks, map2$ticker)
if (length(missing))
  msg("ATTENTION : tickers sans mapping cluster : ", paste(missing, collapse = ", "))

# =============================================================================
# NORMALISATION LIGNE + TYPES
# =============================================================================
theta2 <- theta %>%
  filter(i %in% map2$ticker, j %in% map2$ticker) %>%
  left_join(map2 %>% rename(i = ticker, cl_i = cluster), by = "i") %>%
  left_join(map2 %>% rename(j = ticker, cl_j = cluster), by = "j") %>%
  group_by(date, i) %>%
  mutate(row_sum = sum(theta, na.rm = TRUE),
         theta_n = ifelse(row_sum > 0, theta / row_sum, NA_real_)) %>%
  ungroup() %>%
  filter(is.finite(theta_n)) %>%
  mutate(same_cluster = (cl_i == cl_j),
         is_own       = (i == j),
         type         = case_when(is_own ~ "own",
                                  same_cluster ~ "within",
                                  TRUE ~ "cross"))

# =============================================================================
# (A) MÉTRIQUES SYSTÈME
# =============================================================================
system_metrics <- theta2 %>%
  group_by(date) %>%
  summarise(
    N             = n_distinct(i),
    total_offdiag = 100 * mean(1 - theta_n[is_own], na.rm = TRUE),
    .groups = "drop"
  )

within_cross <- theta2 %>%
  filter(!is_own) %>%
  group_by(date, i) %>%
  summarise(within_i = sum(theta_n[same_cluster],  na.rm = TRUE),
            cross_i  = sum(theta_n[!same_cluster], na.rm = TRUE),
            .groups = "drop") %>%
  group_by(date) %>%
  summarise(within_exact = 100 * mean(within_i, na.rm = TRUE),
            cross_exact  = 100 * mean(cross_i,  na.rm = TRUE),
            .groups = "drop")

system_metrics <- left_join(system_metrics, within_cross, by = "date")

# =============================================================================
# (B) COMPOSANTES PAR CLUSTER
# =============================================================================
cluster_components <- theta2 %>%
  group_by(date, cl_i, i) %>%
  summarise(
    own_i    = theta_n[i == j][1],
    comove_i = sum(theta_n[(cl_j == cl_i) & (j != i)], na.rm = TRUE),
    contag_i = sum(theta_n[(cl_j != cl_i)],             na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  group_by(date, cluster = cl_i) %>%
  summarise(
    n_assets = n_distinct(i),
    own      = 100 * mean(own_i,    na.rm = TRUE),
    comove   = 100 * mean(comove_i, na.rm = TRUE),
    contag   = 100 * mean(contag_i, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  arrange(cluster, date)

# =============================================================================
# (C) SPILLOVERS CLUSTER -> CLUSTER
# =============================================================================
cluster_sizes <- theta2 %>%
  distinct(date, cl_i, i) %>%
  group_by(date, cluster = cl_i) %>%
  summarise(Nc = n_distinct(i), .groups = "drop")

cluster_c2c <- theta2 %>%
  group_by(date, cl_i, cl_j) %>%
  summarise(sum_theta = sum(theta_n, na.rm = TRUE), .groups = "drop") %>%
  left_join(cluster_sizes %>% rename(cl_i = cluster, Nc = Nc), by = c("date", "cl_i")) %>%
  left_join(cluster_sizes %>% rename(cl_j = cluster, Nk = Nc), by = c("date", "cl_j")) %>%
  mutate(spill = ifelse(Nc > 0 & Nk > 0, 100 * (sum_theta / (Nc * Nk)), NA_real_)) %>%
  select(date, from_cluster = cl_j, to_cluster = cl_i, Nc, Nk, spill) %>%
  arrange(date, to_cluster, from_cluster)

cluster_net <- cluster_c2c %>%
  filter(from_cluster != to_cluster) %>%
  group_by(date, cluster = from_cluster) %>%
  summarise(to_others = sum(spill, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    cluster_c2c %>%
      filter(from_cluster != to_cluster) %>%
      group_by(date, cluster = to_cluster) %>%
      summarise(from_others = sum(spill, na.rm = TRUE), .groups = "drop"),
    by = c("date", "cluster")
  ) %>%
  mutate(net = to_others - from_others) %>%
  arrange(cluster, date)

# =============================================================================
# EXPORTS
# =============================================================================
readr::write_csv(system_metrics,    file.path(out_dir, "system_connectedness.csv"))
readr::write_csv(cluster_components, file.path(out_dir, "cluster_components.csv"))
readr::write_csv(cluster_c2c,       file.path(out_dir, "cluster_spillover_c2c.csv"))
readr::write_csv(cluster_net,       file.path(out_dir, "cluster_net.csv"))

msg("Ecrit : system_connectedness.csv")
msg("Ecrit : cluster_components.csv")
msg("Ecrit : cluster_spillover_c2c.csv")
msg("Ecrit : cluster_net.csv")
msg("Done.")
