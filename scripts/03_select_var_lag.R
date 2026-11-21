#!/usr/bin/env Rscript
# ==============================================================================
# 03_select_var_lag.R
# Sélection formelle de l'ordre p du VAR via AIC, HQ, SC (BIC)
# sur le panel complet des log-returns hebdomadaires.
# Résultat utilisé pour valider p_fixed dans project.yml.
#
# Sortie :
#   outputs/var_dy/mat/lag_selection.csv
# ==============================================================================

suppressPackageStartupMessages({
  library(yaml)
  library(readr)
  library(vars)
  library(tibble)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse = " "), "\n")

# ==============================================================================
# CONFIG
# ==============================================================================
cfg      <- yaml::read_yaml("config/project.yml")
ret_path <- cfg$paths$returns_weekly %||% "data/processed/returns_weekly.csv"
out_dir  <- file.path(cfg$paths$out_var_dy %||% "outputs/var_dy", "mat")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

p_fixed <- as.integer(cfg$var_dy$p_fixed %||% 1L)
p_max   <- 5L

stopifnot(file.exists(ret_path))

msg("03_select_var_lag.R")
msg("  Source  : ", ret_path)
msg("  p_fixed configuré dans project.yml : ", p_fixed)

# ==============================================================================
# CORE — sélection du lag
# ==============================================================================
ret      <- read_csv(ret_path, show_col_types = FALSE)
ret$date <- as.Date(ret$date)
X        <- as.matrix(ret[, setdiff(names(ret), "date")])

msg("  Données : N=", nrow(X), " | K=", ncol(X))
msg("  Calcul VARselect (p_max=", p_max, ")...")

lag_sel <- VARselect(X, lag.max = p_max, type = "const")
crit    <- lag_sel$criteria

# Détection robuste des noms de lignes (varient selon version du package vars)
find_row <- function(mat, pattern) {
  idx <- grep(pattern, rownames(mat))
  if (length(idx) == 0) return(NA_character_)
  rownames(mat)[idx[1]]
}

aic_row <- find_row(crit, "AIC")
hq_row  <- find_row(crit, "HQ")
sc_row  <- find_row(crit, "SC")
fpe_row <- find_row(crit, "FPE")

# Construire le tableau des p* sélectionnés
rows <- list()
if (!is.na(aic_row)) rows[["AIC"]]     <- which.min(crit[aic_row, ])
if (!is.na(hq_row))  rows[["HQ"]]      <- which.min(crit[hq_row,  ])
if (!is.na(sc_row))  rows[["SC (BIC)"]] <- which.min(crit[sc_row,  ])
if (!is.na(fpe_row)) rows[["FPE"]]     <- which.min(crit[fpe_row, ])

best_df <- tibble(
  criterion = names(rows),
  p_star    = unlist(rows)
)

# ==============================================================================
# RÉSUMÉ
# ==============================================================================
msg("  --- Critères par ordre p ---")
print(round(crit, 4))
msg("  --- Ordres sélectionnés ---")
print(best_df)

if (all(best_df$p_star == p_fixed)) {
  msg("  OK : tous les critères confirment p=", p_fixed,
      " (conforme à project.yml)")
} else {
  msg("  ATTENTION : résultats mixtes — p_fixed=", p_fixed,
      " mais certains critères suggèrent p=",
      paste(sort(unique(best_df$p_star)), collapse = "/"),
      ". Voir lag_selection.csv.")
}

# ==============================================================================
# EXPORT
# ==============================================================================
out_path <- file.path(out_dir, "lag_selection.csv")
write_csv(best_df, out_path)
msg("  Écrit : ", out_path)
msg("Done.")
