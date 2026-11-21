#!/usr/bin/env Rscript
# =============================================================================
# 04_var_dy_connectedness.R
# VAR-DY rolling connectedness (Diebold-Yilmaz) sur les log-returns
# hebdomadaires standardisés. Utilise le package ConnectednessApproach.
# Produit :
#   outputs/var_dy/mat/DY_tci.csv          (TCI rolling)
#   outputs/var_dy/mat/DY_to.csv           (TO directional)
#   outputs/var_dy/mat/DY_from.csv         (FROM directional)
#   outputs/var_dy/mat/DY_net.csv          (NET = TO - FROM)
#   outputs/var_dy/mat/DY_CT_last.csv      (matrice GFEVD dernière fenêtre)
#   outputs/var_dy/mat/DY_theta_long.csv   (GFEVD rolling format long)
#   outputs/var_dy/obj/CA_vardy_weekly.rds (objet CA complet)
# =============================================================================

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(tidyr)
  library(lubridate); library(zoo); library(tibble)
  library(ConnectednessApproach)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse = " "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg      <- yaml::read_yaml("config/project.yml")
vd       <- cfg$var_dy %||% list()
ret_path <- cfg$paths$returns_weekly %||% "data/processed/returns_weekly.csv"
out_base <- cfg$paths$out_var_dy     %||% "outputs/var_dy"

out_dir <- file.path(out_base, "mat"); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
obj_dir <- file.path(out_base, "obj"); dir.create(obj_dir, recursive = TRUE, showWarnings = FALSE)

H       <- as.integer(vd$forecast_horizon %||% 10L)
p_fixed <- as.integer(vd$p_fixed          %||% 1L)
Wsize   <- as.integer(vd$rolling_window   %||% 156L)
Wstep   <- max(1L, as.integer(vd$rolling_step %||% 4L))
Wmax    <- vd$rolling_max_windows
if (!is.null(Wmax)) Wmax <- as.integer(Wmax)

stopifnot(file.exists(ret_path))

ret      <- readr::read_csv(ret_path, show_col_types = FALSE)
ret$date <- as.Date(ret$date)
num_cols <- setdiff(names(ret), "date")

if (!is.finite(Wsize) || Wsize < 5L) Wsize <- 100L
if (nrow(ret) <= Wsize) Wsize <- max(5L, min(Wsize, nrow(ret) - 1L))

ends_idx_full   <- seq(from = Wsize, to = nrow(ret), by = 1L)
roll_dates_full <- ret$date[ends_idx_full]

Z <- zoo::zoo(ret[, num_cols, drop = FALSE], order.by = ret$date)

# Explicit full-sample standardization (zero mean, unit variance per series).
# Matches the paper description and makes preprocessing independent of package internals.
scaled_mat <- scale(zoo::coredata(Z))
Z          <- zoo::zoo(scaled_mat, order.by = zoo::index(Z))

msg("04_var_dy_connectedness.R")
msg("N=", nrow(ret), " | K=", length(num_cols),
    " | W=", Wsize, " | step=", Wstep, " | H=", H, " | p=", p_fixed)

# =============================================================================
# ESTIMATION VAR-DY ROLLING
# =============================================================================
CA <- ConnectednessApproach(
  x            = Z,
  nlag         = p_fixed,
  nfore        = H,
  window.size  = Wsize,
  model        = "VAR",
  connectedness = "Time",
  corrected    = FALSE
)
saveRDS(CA, file.path(obj_dir, "CA_vardy_weekly.rds"))
msg("Objet CA sauvegardé.")

# =============================================================================
# EXTRACTION DES SÉRIES TEMPORELLES
# =============================================================================
get_time <- function(CA) {
  if (!is.null(CA$TCI) && !is.null(CA$TO) && !is.null(CA$FROM) && !is.null(CA$NET))
    return(list(TCI = CA$TCI, TO = CA$TO, FROM = CA$FROM, NET = CA$NET))
  if (!is.null(CA$CT) && is.list(CA$CT) &&
      all(c("TCI", "TO", "FROM", "NET") %in% names(CA$CT)))
    return(list(TCI = CA$CT$TCI, TO = CA$CT$TO, FROM = CA$CT$FROM, NET = CA$CT$NET))
  ConnectednessApproach::TimeConnectedness(CA)
}

as_df <- function(z) {
  if (inherits(z, "zoo")) {
    tibble(date = as.Date(zoo::index(z))) |>
      dplyr::bind_cols(as.data.frame(zoo::coredata(z)))
  } else if (is.data.frame(z) || is.matrix(z)) {
    as_tibble(as.data.frame(z))
  } else tibble()
}

thin_by <- function(df, step, wmax = NULL) {
  if (!nrow(df)) return(df)
  out <- df[seq(1, nrow(df), by = max(1L, step)), , drop = FALSE]
  if (!is.null(wmax) && nrow(out) > wmax) out <- tail(out, wmax)
  out
}

TT  <- get_time(CA)
tci <- as_df(TT$TCI)

if (ncol(tci) >= 2 && !"TCI" %in% names(tci)) names(tci)[2] <- "TCI"
if (!"date" %in% names(tci) ||
    any(!is.finite(as.numeric(tci$date))) ||
    nrow(tci) != length(ends_idx_full)) {
  tci <- tci[, setdiff(names(tci), "date"), drop = FALSE]
  tci <- tibble(date = roll_dates_full[seq_len(nrow(tci))]) |> dplyr::bind_cols(tci)
}

tci     <- thin_by(tci,              Wstep, Wmax) |> arrange(date)
to_df   <- thin_by(as_df(TT$TO),   Wstep, Wmax)
from_df <- thin_by(as_df(TT$FROM), Wstep, Wmax)
net_df  <- thin_by(as_df(TT$NET),  Wstep, Wmax)

readr::write_csv(tci,     file.path(out_dir, "DY_tci.csv"))
readr::write_csv(to_df,   file.path(out_dir, "DY_to.csv"))
readr::write_csv(from_df, file.path(out_dir, "DY_from.csv"))
readr::write_csv(net_df,  file.path(out_dir, "DY_net.csv"))
msg("TCI / TO / FROM / NET écrits.")

# =============================================================================
# MATRICE GFEVD DERNIÈRE FENÊTRE
# =============================================================================
CT <- NULL
if (!is.null(CA$TABLE) && (is.matrix(CA$TABLE) || is.data.frame(CA$TABLE)))
  CT <- CA$TABLE
if (is.null(CT) && !is.null(CA$CT) && is.list(CA$CT)) {
  if (!is.null(CA$CT$TABLE)) CT <- CA$CT$TABLE
  if (is.null(CT) && !is.null(CA$CT$tables) && length(CA$CT$tables))
    CT <- CA$CT$tables[[length(CA$CT$tables)]]
}
if (!is.null(CT)) {
  M <- if (is.list(CT) && !is.null(CT$TO)) CT$TO else CT
  # GFEVD row-sum check: normalized shares must sum to ~1 per row.
  M_mat <- as.matrix(M)
  if (is.numeric(M_mat)) {
    row_sums <- rowSums(M_mat, na.rm = TRUE)
    if (any(abs(row_sums - 1) > 0.01, na.rm = TRUE)) {
      warning("GFEVD row sums deviate from 1 — normalization may not hold in this package version.")
    } else {
      msg("GFEVD row-sum check passed (all rows sum to ~1).")
    }
  } else {
    msg("GFEVD row-sum check skipped (TABLE contains non-numeric summary rows).")
  }
  M <- tibble::rownames_to_column(as.data.frame(M), "from")
  readr::write_csv(M, file.path(out_dir, "DY_CT_last.csv"))
  msg("DY_CT_last.csv écrit.")
}

# =============================================================================
# GFEVD ROLLING FORMAT LONG (theta_ij pour chaque fenêtre)
# =============================================================================
find_fevd_candidates <- function(x, path = "CA", out = list()) {
  if (is.array(x) && length(dim(x)) == 3) {
    d <- dim(x)
    if (!any(is.na(d)) && d[1] == d[2] && d[1] >= 3 && d[3] >= 5)
      out[[length(out) + 1]] <- list(path = path, type = "array3d", dims = d, obj = x)
  }
  if (is.list(x) && length(x) >= 5) {
    mats <- x[sapply(x, is.matrix)]
    if (length(mats) >= 5) {
      dims <- lapply(mats, dim); d0 <- dims[[1]]
      same_dim <- all(sapply(dims, function(di) length(di) == 2 && all(di == d0)))
      if (same_dim && d0[1] == d0[2] && d0[1] >= 3)
        out[[length(out) + 1]] <- list(path = path, type = "list_mats",
                                       dims = c(d0, length(mats)), obj = mats)
    }
  }
  if (is.list(x)) {
    nms <- names(x)
    for (i in seq_along(x)) {
      nm  <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("[[", i, "]]")
      out <- find_fevd_candidates(x[[i]], paste0(path, "$", nm), out)
    }
  }
  out
}

write_theta_long <- function(theta_3d, dates, Wstep, Wmax, out_path) {
  Tn    <- dim(theta_3d)[3]
  stopifnot(length(dates) >= Tn)
  dates <- dates[seq_len(Tn)]

  pick_idx <- seq(1, Tn, by = max(1L, Wstep))
  if (!is.null(Wmax) && length(pick_idx) > Wmax)
    pick_idx <- tail(pick_idx, Wmax)

  long_rows <- vector("list", length(pick_idx))
  for (k in seq_along(pick_idx)) {
    t <- pick_idx[k]
    M <- as.matrix(theta_3d[, , t, drop = TRUE])
    if (is.null(rownames(M))) rownames(M) <- paste0("i", seq_len(nrow(M)))
    if (is.null(colnames(M))) colnames(M) <- paste0("j", seq_len(ncol(M)))

    df_long           <- as.data.frame(as.table(M), stringsAsFactors = FALSE)
    names(df_long)    <- c("i", "j", "theta")
    df_long$date      <- as.Date(dates[t])
    df_long$window_id <- k
    long_rows[[k]]    <- df_long[, c("date", "window_id", "i", "j", "theta")]
  }

  theta_long <- dplyr::bind_rows(long_rows) %>%
    dplyr::mutate(date = as.Date(date), theta = as.numeric(theta)) %>%
    dplyr::arrange(date, i, j)

  readr::write_csv(theta_long, out_path)
}

tryCatch({
  cands <- find_fevd_candidates(CA)
  if (length(cands) == 0) {
    msg("ATTENTION : aucun candidat GFEVD rolling trouvé dans l'objet CA.")
  } else {
    Ts   <- sapply(cands, function(ca) ca$dims[3])
    best <- cands[[which.max(Ts)]]
    msg("Candidat GFEVD : ", best$path, " | type=", best$type, " | dims=", paste(best$dims, collapse = "x"))

    theta_3d <- if (best$type == "array3d") {
      best$obj
    } else {
      mats <- best$obj; K <- nrow(mats[[1]]); Tn <- length(mats)
      arr  <- array(NA_real_, dim = c(K, K, Tn))
      for (t in seq_len(Tn)) arr[, , t] <- mats[[t]]
      dimnames(arr) <- list(rownames(mats[[1]]), colnames(mats[[1]]), NULL)
      arr
    }

    Tn       <- dim(theta_3d)[3]
    L        <- min(Tn, length(roll_dates_full))
    theta_3d <- theta_3d[, , seq_len(L), drop = FALSE]
    out_theta <- file.path(out_dir, "DY_theta_long.csv")
    write_theta_long(theta_3d, roll_dates_full[seq_len(L)], Wstep, Wmax, out_theta)
    msg("DY_theta_long.csv écrit.")
  }
}, error = function(e) {
  msg("ATTENTION : DY_theta_long.csv non généré : ", e$message)
  msg("  TCI / TO / FROM / NET restent valides.")
})

# Self-documenting parameters log.
params_log <- tibble::tibble(
  parameter = c("p", "H", "W", "Wstep", "N_series", "N_rows", "date_run"),
  value     = c(as.character(p_fixed), as.character(H),
                as.character(Wsize), as.character(Wstep),
                as.character(length(num_cols)), as.character(nrow(ret)),
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
)
readr::write_csv(params_log, file.path(out_dir, "parameters_used.csv"))
msg("parameters_used.csv écrit.")

msg("Done.")
