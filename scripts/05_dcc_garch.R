#!/usr/bin/env Rscript
# =============================================================================
# 05_dcc_garch.R
# DCC-GARCH rolling sur les log-returns hebdomadaires.
# Produit :
#   outputs/dcc_garch/mat/dcc_meancorr_rolling.csv  (moyenne |corr| par fenêtre)
#   outputs/dcc_garch/mat/dcc_eigen_centrality.csv  (centralité eigenvector)
#   outputs/dcc_garch/mat/DCC_CT_last.csv           (matrice corr dernière fenêtre)
# =============================================================================

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(lubridate)
  library(zoo); library(xts); library(rmgarch); library(rugarch); library(igraph)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse = " "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg      <- yaml::read_yaml("config/project.yml")
vd       <- cfg$var_dy   %||% list()
gd       <- cfg$garch_dcc %||% list()
ret_path <- cfg$paths$returns_weekly %||% "data/processed/returns_weekly.csv"
out_base <- cfg$paths$out_dcc        %||% "outputs/dcc_garch"

strict_mode <- isTRUE(gd$strict %||% FALSE)
out_dir <- file.path(out_base, "mat"); dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
obj_dir <- file.path(out_base, "obj"); dir.create(obj_dir, recursive = TRUE, showWarnings = FALSE)

Wsize <- as.integer(vd$rolling_window %||% 156L)
Wstep <- max(1L, as.integer(vd$rolling_step %||% 4L))
Wmax  <- vd$rolling_max_windows
if (!is.null(Wmax)) Wmax <- as.integer(Wmax)

stopifnot(file.exists(ret_path))
ret      <- readr::read_csv(ret_path, show_col_types = FALSE)
ret$date <- as.Date(ret$date)
num_cols <- setdiff(names(ret), "date")

if (!is.finite(Wsize) || Wsize < 40L) Wsize <- 156L
if (nrow(ret) <= Wsize) Wsize <- max(40L, min(Wsize, nrow(ret) - 1L))

ends_idx <- seq(from = Wsize, to = nrow(ret), by = Wstep)
if (!is.null(Wmax) && length(ends_idx) > Wmax)
  ends_idx <- tail(ends_idx, Wmax)

msg("05_dcc_garch.R")
msg("N=", nrow(ret), " | K=", length(num_cols),
    " | W=", Wsize, " | step=", Wstep,
    " | fenêtres=", length(ends_idx), " | strict=", strict_mode)

# =============================================================================
# SPÉCIFICATION DCC
# =============================================================================
ugarch_spec_of <- function(use_t = FALSE) {
  ugarchspec(
    mean.model      = list(armaOrder = c(0, 0), include.mean = FALSE),
    variance.model  = list(model = "sGARCH", garchOrder = c(1, 1),
                           variance.targeting = FALSE),
    distribution.model = if (use_t) "std" else "norm"
  )
}

fit_dcc_multi <- function(Xts) {
  attempts <- list(
    list(use_t = FALSE, solver = "solnp"),
    list(use_t = FALSE, solver = "nlminb"),
    list(use_t = FALSE, solver = "hybrid"),
    list(use_t = TRUE,  solver = "solnp"),
    list(use_t = TRUE,  solver = "nlminb"),
    list(use_t = TRUE,  solver = "hybrid")
  )
  for (a in attempts) {
    mspec <- multispec(replicate(ncol(Xts), ugarch_spec_of(a$use_t)))
    dspec <- dccspec(uspec = mspec, dccOrder = c(1, 1),
                     distribution = if (a$use_t) "mvt" else "mvnorm")
    fit <- tryCatch(
      dccfit(dspec, data = Xts,
             fit.control = list(eval.se = FALSE, scale = TRUE, stationarity = 1),
             solver = a$solver, solver.control = list(trace = 0)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      R <- tryCatch(rcor(fit)[, , nrow(Xts)], error = function(e) NULL)
      if (!is.null(R))
        return(list(R   = R,
                    tag = paste0(if (a$use_t) "t" else "gauss", "/", a$solver)))
    }
  }
  NULL
}

# =============================================================================
# BOUCLE ROLLING
# =============================================================================
rows_mean <- list(); rows_eig <- list()
last_R <- NULL; last_cols <- NULL
cnt_ok <- 0L; cnt_skip <- 0L
cnt_gauss <- 0L; cnt_t <- 0L; cnt_fallback <- 0L
t0 <- proc.time()[["elapsed"]]

for (ix in seq_along(ends_idx)) {
  e   <- ends_idx[ix]; s <- e - Wsize + 1L
  sub <- ret[s:e, , drop = FALSE]
  Xw  <- as.matrix(sub[, num_cols, drop = FALSE])
  rownames(Xw) <- format(sub$date)

  ok_ratio <- colMeans(is.finite(Xw))
  keep <- which(ok_ratio >= 0.80)
  if (length(keep) < 3L) { cnt_skip <- cnt_skip + 1L; next }
  Xw <- Xw[, keep, drop = FALSE]

  Xw <- apply(Xw, 2, function(v) {
    v <- zoo::na.locf(v, na.rm = FALSE)
    v <- zoo::na.locf(v, fromLast = TRUE, na.rm = FALSE)
    v[!is.finite(v)] <- 0; v
  })
  sdv  <- apply(Xw, 2, sd)
  keep2 <- which(is.finite(sdv) & sdv > 1e-10)
  if (length(keep2) < 3L) { cnt_skip <- cnt_skip + 1L; next }
  Xw <- Xw[, keep2, drop = FALSE]

  if (nrow(Xw) < max(60L, floor(0.6 * Wsize))) { cnt_skip <- cnt_skip + 1L; next }

  Xw  <- scale(Xw)
  Xts <- xts::xts(Xw, order.by = as.Date(rownames(Xw)))

  used <- NA_character_; R <- NULL
  res  <- fit_dcc_multi(Xts)
  if (!is.null(res)) { R <- res$R; used <- res$tag }

  if (is.null(R) && !strict_mode) {
    R <- tryCatch(cor(Xw), error = function(e) NULL)
    if (!is.null(R)) used <- "corr_fallback"
  }
  if (is.null(R)) { cnt_skip <- cnt_skip + 1L; next }

  if (grepl("^gauss/", used)) cnt_gauss   <- cnt_gauss   + 1L
  if (grepl("^t/",     used)) cnt_t       <- cnt_t       + 1L
  if (used == "corr_fallback") cnt_fallback <- cnt_fallback + 1L

  cols <- colnames(Xw); dimnames(R) <- list(cols, cols)
  last_R <- R; last_cols <- cols; cnt_ok <- cnt_ok + 1L

  Ru <- R; diag(Ru) <- 0
  iu <- upper.tri(Ru, diag = FALSE)
  meancorr <- if (any(iu)) mean(abs(Ru[iu])) else NA_real_
  rows_mean[[length(rows_mean) + 1L]] <-
    tibble::tibble(end = max(as.Date(rownames(Xw))), dcc_meancorr = meancorr)

  A  <- abs(R); diag(A) <- 0
  g  <- igraph::graph_from_adjacency_matrix(A, mode = "undirected",
                                             diag = FALSE, weighted = TRUE)
  ec <- suppressWarnings(
    igraph::eigen_centrality(g, directed = FALSE,
                             weights = E(g)$weight, scale = TRUE)$vector)
  if (!is.null(ec) && length(ec) == length(cols))
    rows_eig[[length(rows_eig) + 1L]] <-
      tibble::tibble(end = max(as.Date(rownames(Xw))), ticker = cols, eigen = as.numeric(ec))

  if (ix %% 5L == 0L || ix == length(ends_idx))
    msg("  fenêtre ", ix, "/", length(ends_idx),
        " end=", as.character(max(as.Date(rownames(Xw)))),
        " k=", length(cols), " [", if (is.na(used)) "skip" else paste("OK", used), "]")
}

# =============================================================================
# EXPORTS
# =============================================================================
stopifnot(length(rows_mean) > 0)

dplyr::bind_rows(rows_mean) |> arrange(end) |>
  readr::write_csv(file.path(out_dir, "dcc_meancorr_rolling.csv"))

if (length(rows_eig)) {
  dplyr::bind_rows(rows_eig) |> arrange(end, ticker) |>
    readr::write_csv(file.path(out_dir, "dcc_eigen_centrality.csv"))
}

if (!is.null(last_R)) {
  out_ct <- tibble::tibble(from = rownames(last_R)) |>
    dplyr::bind_cols(as_tibble(as.data.frame(last_R)))
  readr::write_csv(out_ct, file.path(out_dir, "DCC_CT_last.csv"))
}

elapsed <- proc.time()[["elapsed"]] - t0
msg("ok=", cnt_ok, " | skip=", cnt_skip,
    " | gauss=", cnt_gauss, " | t=", cnt_t, " | fallback=", cnt_fallback,
    " | elapsed=", round(elapsed, 1), "s")
msg("Done.")
