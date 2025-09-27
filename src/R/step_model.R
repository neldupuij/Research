# src/R/step_model.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(vars)
})

log_msg <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", sprintf(...), "\n")
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------- MA builder ----------
.build_phis <- function(A_list, p, H) {
  k <- nrow(A_list[[1]])
  Phis <- array(0, dim = c(k, k, H))
  Phis[,,1] <- diag(k)
  if (H >= 2) {
    for (h in 2:H) {
      acc <- matrix(0, k, k)
      for (m in 1:min(p, h-1)) acc <- acc + A_list[[m]] %*% Phis[,,h-m]
      Phis[,,h] <- acc
    }
  }
  Phis
}

# ---------- GFEVD (generalized, Pesaran–Shin) ----------
.gfevd_generalized <- function(Phis, Sigma) {
  H <- dim(Phis)[3]; k <- dim(Phis)[1]
  sdiag <- diag(Sigma)
  Theta <- matrix(0, k, k)
  for (i in 1:k) {
    den <- 0
    for (h in 1:H) {
      v <- Phis[,,h][i,, drop=FALSE]
      den <- den + v %*% Sigma %*% t(v)
    }
    den <- as.numeric(den)
    for (j in 1:k) {
      num <- 0
      for (h in 1:H) {
        vij <- Phis[,,h][i,, drop=FALSE] %*% Sigma[,j, drop=FALSE]
        num <- num + as.numeric(vij)^2
      }
      Theta[i,j] <- num / den / sdiag[j]
    }
  }
  # row-normalize to %
  rs <- rowSums(Theta)
  for (i in 1:k) Theta[i,] <- 100 * Theta[i,] / rs[i]
  Theta
}

# ---------- indices ----------
.compute_indices <- function(Theta_pct) {
  k <- nrow(Theta_pct)
  TCI  <- 100 - mean(diag(Theta_pct))
  FROM <- rowSums(Theta_pct) - diag(Theta_pct)
  TO   <- colSums(Theta_pct) - diag(Theta_pct)
  NET  <- TO - FROM
  list(TCI=TCI, FROM=FROM, TO=TO, NET=NET)
}

.choose_p_bic <- function(X, lag_max) {
  sel <- vars::VARselect(X, lag.max = lag_max, type = "const")
  p <- as.integer(sel$selection["SC(n)"])
  if (!is.finite(p) || p < 1) p <- 1
  p
}

# ---------- run once ----------
.run_spillover_once <- function(X, H, lag_max, p_override = NULL) {
  p <- if (is.null(p_override)) .choose_p_bic(X, lag_max) else as.integer(p_override)
  V <- vars::VAR(X, p = p, type = "const")
  A <- vars::Acoef(V); if (p == 1 && is.matrix(A)) A <- list(A)
  U <- residuals(V); Sigma <- crossprod(U) / nrow(U)
  Phis <- .build_phis(A, p, H)
  TH   <- .gfevd_generalized(Phis, Sigma)
  list(Theta_pct = TH, idx = .compute_indices(TH), p = p)
}

# ---------- writer helpers (explicit row labels) ----------
.write_kxk <- function(TH, path) {
  # TH is a numeric matrix with rownames/colnames = symbols
  sym <- colnames(TH)
  stopifnot(identical(sym, rownames(TH)))
  out <- tibble::tibble(symbol = sym) |>
         dplyr::bind_cols(as.data.frame(TH, check.names = FALSE))
  readr::write_csv(out, path)
}

# ---------- STATIC ----------
step_model_static <- function(cfg) {
  H       <- cfg$forecast_horizon %||% 10
  lag_max <- cfg$var_max_lag %||% 5

  wide <- readr::read_csv("data/processed/returns_panel.csv", show_col_types = FALSE) %>%
          dplyr::select(symbol, date, return) %>%
          tidyr::pivot_wider(names_from = symbol, values_from = return) %>%
          tidyr::drop_na() %>%              # statique : intersection globale OK
          dplyr::arrange(date)

  dates <- wide$date
  X <- as.matrix(dplyr::select(wide, -date))
  k <- ncol(X); n <- nrow(X)

  log_msg("Model: start (H=%d)", H)
  res <- .run_spillover_once(X, H, lag_max, p_override = cfg$p_fixed %||% NULL)
  TH  <- res$Theta_pct; idx <- res$idx; p <- res$p

  dir.create("reports/exports", showWarnings = FALSE, recursive = TRUE)
  rownames(TH) <- colnames(X); colnames(TH) <- colnames(X)

  # k×k + table avec étiquettes explicites
  .write_kxk(TH, "reports/exports/spillover_matrix_kxk.csv")
  .write_kxk(TH, "reports/exports/spillover_table.csv")

  tibble::tibble(
    metric="TCI", value=as.numeric(idx$TCI), k=k, n_obs=n, p=p, H=H,
    date_start=as.character(min(dates)), date_end=as.character(max(dates))
  ) %>% readr::write_csv("reports/exports/total_connectedness.csv")

  tibble::tibble(
    symbol=colnames(X), FROM=as.numeric(idx$FROM), TO=as.numeric(idx$TO), NET=as.numeric(idx$NET)
  ) %>% readr::write_csv("reports/exports/net_spillovers.csv")

  log_msg("Saved spillover_table.csv, spillover_matrix_kxk.csv, total_connectedness.csv (TCI=%.2f), net_spillovers.csv", idx$TCI)
  invisible(list(TCI=idx$TCI, p=p))
}

# ---------- ROLLING ----------
step_model_rolling <- function(cfg) {
  H        <- cfg$forecast_horizon %||% 10
  lag_max  <- cfg$var_max_lag %||% 5
  W        <- as.integer(cfg$rolling_window)
  step     <- as.integer(cfg$rolling_step %||% 5)
  log_every<- as.integer(cfg$log_every %||% 5)
  max_win  <- cfg$rolling_max_windows %||% NA
  mode_p   <- (cfg$select_p_mode %||% "per_window")  # "per_window" | "global" | "fixed"
  p_fixed  <- cfg$p_fixed %||% NULL

  if (!is.finite(W) || W <= 0) stop("rolling_window must be > 0")

  # IMPORTANT : pas d'intersection globale ici (pas de drop_na)
  wide <- readr::read_csv("data/processed/returns_panel.csv", show_col_types = FALSE) %>%
          dplyr::select(symbol, date, return) %>%
          tidyr::pivot_wider(names_from = symbol, values_from = return) %>%
          dplyr::arrange(date)

  dates <- wide$date
  k <- ncol(wide) - 1
  n <- nrow(wide)
  if (n < W + 20) stop(sprintf("Not enough observations (%d) for W=%d", n, W))

  # choose p once if mode = global
  p_global <- NULL
  if (is.null(p_fixed) && mode_p == "global") {
    X_all <- as.matrix(stats::na.omit(dplyr::select(wide, -date)))  # pour choisir p sur les lignes complètes
    p_global <- .choose_p_bic(X_all, lag_max)
    log_msg("Global BIC-selected p=%d (will be reused in all windows)", p_global)
  }

  win_ends <- seq(from = W, to = n, by = step)
  if (is.finite(max_win)) win_ends <- head(win_ends, max_win)
  nW <- length(win_ends)

  log_msg("Rolling model: k=%d, n=%d, W=%d, H=%d, step=%d, lag_max=%d, windows=%d, mode_p=%s%s",
          k, n, W, H, step, lag_max, nW, mode_p,
          if (!is.null(p_fixed)) sprintf(" (p_fixed=%d)", as.integer(p_fixed)) else
            if (!is.null(p_global)) sprintf(" (p_global=%d)", as.integer(p_global)) else "")

  dir.create("reports/exports", showWarnings = FALSE, recursive = TRUE)
  out_tci <- NULL
  out_net <- NULL

  for (ix in seq_along(win_ends)) {
    t_end <- win_ends[ix]; t_start <- t_end - W + 1

    sub <- wide[t_start:t_end, , drop = FALSE]
    # intersection PAR FENETRE (on exige des lignes complètes dans la fenêtre seulement)
    comp <- stats::complete.cases(dplyr::select(sub, -date))
    sub  <- sub[comp, , drop = FALSE]

    Xw <- as.matrix(dplyr::select(sub, -date))
    if (nrow(Xw) < max(20, (cfg$p_fixed %||% 1) + 5)) {
      # fenêtre trop creuse → on saute proprement
      next
    }

    # decide p for this window
    p_override <- if (!is.null(p_fixed)) as.integer(p_fixed) else
                    if (!is.null(p_global)) as.integer(p_global) else NULL

    res <- .run_spillover_once(Xw, H, lag_max, p_override = p_override)
    idx <- res$idx; p <- res$p

    # accumulate (date de fin = date de sub, pas wide)
    row_tci <- tibble::tibble(
      date_end = max(sub$date), window=W, step=step, k=k, n_win=nrow(Xw), p=p, H=H, TCI=as.numeric(idx$TCI)
    )
    row_net <- tibble::tibble(
      date_end = max(sub$date), symbol = colnames(Xw),
      TO=as.numeric(idx$TO), FROM=as.numeric(idx$FROM), NET=as.numeric(idx$NET)
    )
    out_tci <- dplyr::bind_rows(out_tci, row_tci)
    out_net <- dplyr::bind_rows(out_net, row_net)

    if (ix %% log_every == 0 || ix == nW) {
      log_msg("Window %d/%d (end %s): p=%d, TCI=%.2f", ix, nW, as.character(max(sub$date)), p, idx$TCI)
      readr::write_csv(out_tci, "reports/exports/tci_rolling.csv")
      readr::write_csv(out_net, "reports/exports/net_spillovers_rolling.csv")
    }
  }

  # also export "static-style" for last window (sur la dernière fenêtre valide)
  if (!is.null(out_tci) && nrow(out_tci) > 0) {
    last_date <- max(out_tci$date_end)
    # recompute on that exact last subwindow to export k×k proprement
    end_idx <- max(which(dates <= last_date))
    t_start <- end_idx - W + 1
    sub <- wide[t_start:end_idx, , drop = FALSE]
    sub <- sub[stats::complete.cases(dplyr::select(sub, -date)), , drop = FALSE]
    Xlast <- as.matrix(dplyr::select(sub, -date))

    res_last <- .run_spillover_once(Xlast, H, lag_max,
                    p_override = if (!is.null(cfg$p_fixed)) cfg$p_fixed else p_global)
    TH_last  <- res_last$Theta_pct; idx_last <- res_last$idx; p_last <- res_last$p
    rownames(TH_last) <- colnames(Xlast); colnames(TH_last) <- colnames(Xlast)

    .write_kxk(TH_last, "reports/exports/spillover_matrix_kxk.csv")
    .write_kxk(TH_last, "reports/exports/spillover_table.csv")

    tibble::tibble(
      metric="TCI", value=as.numeric(idx_last$TCI), k=ncol(Xlast), n_obs=nrow(Xlast), p=p_last, H=H,
      date_start=as.character(min(sub$date)), date_end=as.character(max(sub$date))
    ) %>% readr::write_csv("reports/exports/total_connectedness.csv")

    out_net %>% dplyr::filter(date_end == last_date) %>%
      dplyr::select(symbol, FROM, TO, NET) %>%
      readr::write_csv("reports/exports/net_spillovers.csv")
  }

  log_msg("Saved tci_rolling.csv (%d rows), net_spillovers_rolling.csv (%d rows), and static-style exports for last window",
          ifelse(is.null(out_tci), 0L, nrow(out_tci)),
          ifelse(is.null(out_net), 0L, nrow(out_net)))
  invisible(list(tci=out_tci, net=out_net))
}

# ---------- dispatcher ----------
step_model <- function(cfg) {
  W <- cfg$rolling_window %||% NA
  if (is.na(W) || !is.finite(W) || W <= 0) step_model_static(cfg) else step_model_rolling(cfg)
}
