#!/usr/bin/env Rscript
# Patch: load saved CA object and write DY_CT_last.csv, DY_theta_long.csv,
# parameters_used.csv — used after script 04 crashed past the estimation step.

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(tibble); library(zoo)
  library(ConnectednessApproach)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse = " "), "\n")

cfg      <- yaml::read_yaml("config/project.yml")
vd       <- cfg$var_dy %||% list()
ret_path <- cfg$paths$returns_weekly %||% "data/processed/returns_weekly.csv"
out_base <- cfg$paths$out_var_dy     %||% "outputs/var_dy"
out_dir  <- file.path(out_base, "mat")
obj_dir  <- file.path(out_base, "obj")

H       <- as.integer(vd$forecast_horizon %||% 10L)
p_fixed <- as.integer(vd$p_fixed          %||% 1L)
Wsize   <- as.integer(vd$rolling_window   %||% 156L)
Wstep   <- max(1L, as.integer(vd$rolling_step %||% 4L))
Wmax    <- vd$rolling_max_windows
if (!is.null(Wmax)) Wmax <- as.integer(Wmax)

ret      <- readr::read_csv(ret_path, show_col_types = FALSE)
ret$date <- as.Date(ret$date)
num_cols <- setdiff(names(ret), "date")

ends_idx_full   <- seq(from = Wsize, to = nrow(ret), by = 1L)
roll_dates_full <- ret$date[ends_idx_full]

rds_path <- file.path(obj_dir, "CA_vardy_weekly.rds")
stopifnot(file.exists(rds_path))
msg("Loading saved CA object from ", rds_path)
CA <- readRDS(rds_path)

# --- DY_CT_last.csv ---
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
  M_mat <- as.matrix(M)
  if (is.numeric(M_mat)) {
    row_sums <- rowSums(M_mat, na.rm = TRUE)
    if (any(abs(row_sums - 1) > 0.01, na.rm = TRUE))
      warning("GFEVD row sums deviate from 1.")
    else
      msg("GFEVD row-sum check passed.")
  } else {
    msg("GFEVD row-sum check skipped (TABLE contains non-numeric summary rows).")
  }
  M_df <- tibble::rownames_to_column(as.data.frame(M), "from")
  readr::write_csv(M_df, file.path(out_dir, "DY_CT_last.csv"))
  msg("DY_CT_last.csv written.")
} else {
  msg("No CT table found in CA object.")
}

# --- DY_theta_long.csv ---
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
  Tn <- dim(theta_3d)[3]; stopifnot(length(dates) >= Tn); dates <- dates[seq_len(Tn)]
  pick_idx <- seq(1, Tn, by = max(1L, Wstep))
  if (!is.null(Wmax) && length(pick_idx) > Wmax) pick_idx <- tail(pick_idx, Wmax)
  long_rows <- vector("list", length(pick_idx))
  for (k in seq_along(pick_idx)) {
    t <- pick_idx[k]
    M <- as.matrix(theta_3d[, , t, drop = TRUE])
    if (is.null(rownames(M))) rownames(M) <- paste0("i", seq_len(nrow(M)))
    if (is.null(colnames(M))) colnames(M) <- paste0("j", seq_len(ncol(M)))
    df_long <- as.data.frame(as.table(M), stringsAsFactors = FALSE)
    names(df_long) <- c("i", "j", "theta")
    df_long$date <- as.Date(dates[t]); df_long$window_id <- k
    long_rows[[k]] <- df_long[, c("date", "window_id", "i", "j", "theta")]
  }
  theta_long <- dplyr::bind_rows(long_rows) %>%
    dplyr::mutate(date = as.Date(date), theta = as.numeric(theta)) %>%
    dplyr::arrange(date, i, j)
  readr::write_csv(theta_long, out_path)
}

tryCatch({
  cands <- find_fevd_candidates(CA)
  if (length(cands) == 0) {
    msg("No rolling GFEVD candidate found in CA.")
  } else {
    Ts   <- sapply(cands, function(ca) ca$dims[3])
    best <- cands[[which.max(Ts)]]
    msg("GFEVD candidate: ", best$path, " | type=", best$type, " | dims=", paste(best$dims, collapse = "x"))
    theta_3d <- if (best$type == "array3d") {
      best$obj
    } else {
      mats <- best$obj; K <- nrow(mats[[1]]); Tn <- length(mats)
      arr  <- array(NA_real_, dim = c(K, K, Tn))
      for (t in seq_len(Tn)) arr[, , t] <- mats[[t]]
      dimnames(arr) <- list(rownames(mats[[1]]), colnames(mats[[1]]), NULL)
      arr
    }
    Tn <- dim(theta_3d)[3]; L <- min(Tn, length(roll_dates_full))
    theta_3d <- theta_3d[, , seq_len(L), drop = FALSE]
    out_theta <- file.path(out_dir, "DY_theta_long.csv")
    write_theta_long(theta_3d, roll_dates_full[seq_len(L)], Wstep, Wmax, out_theta)
    msg("DY_theta_long.csv written.")
  }
}, error = function(e) {
  msg("DY_theta_long.csv not generated: ", e$message)
})

# --- parameters_used.csv ---
params_log <- tibble::tibble(
  parameter = c("p", "H", "W", "Wstep", "N_series", "N_rows", "date_run"),
  value     = c(as.character(p_fixed), as.character(H),
                as.character(Wsize), as.character(Wstep),
                as.character(length(num_cols)), as.character(nrow(ret)),
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
)
readr::write_csv(params_log, file.path(out_dir, "parameters_used.csv"))
msg("parameters_used.csv written.")
msg("Patch complete.")
