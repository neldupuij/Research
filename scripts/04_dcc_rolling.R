suppressPackageStartupMessages({
  library(tidyverse); library(yaml); library(xts)
  library(rmgarch);   library(igraph)
})

`%||%` <- function(x,y) if (is.null(x)) y else x

run_dcc_rolling <- function() {
  cfg  <- if (file.exists("config/project.yml")) yaml::read_yaml("config/project.yml") else list()
  W    <- as.integer(cfg$rolling_window %||% 100L)
  STEP <- as.integer(cfg$rolling_step   %||% 22L)
  message(sprintf("DCC config: W=%d, step=%d", W, STEP))

  panel <- readr::read_csv("data/processed/returns_panel.csv", show_col_types = FALSE) |>
    dplyr::select(symbol, date, return) |>
    tidyr::pivot_wider(names_from = symbol, values_from = return) |>
    dplyr::arrange(date)

  stopifnot(nrow(panel) > W)
  ends_idx <- seq(from = W, to = nrow(panel), by = STEP)

  uspec <- ugarchspec(
    mean.model     = list(armaOrder = c(0,0), include.mean = FALSE),
    variance.model = list(model = "sGARCH", garchOrder = c(1,1), variance.targeting = TRUE),
    distribution.model = "std"
  )

  rows_mean <- list(); rows_eig <- list()
  message(sprintf("Rolling %d windows ...", length(ends_idx)))
  for (ix in seq_along(ends_idx)) {
    e <- ends_idx[ix]; s <- e - W + 1L
    sub <- panel[s:e, , drop = FALSE]
    sub <- sub[stats::complete.cases(dplyr::select(sub, -date)), , drop = FALSE]
    if (nrow(sub) < 50L) next

    X <- as.matrix(dplyr::select(sub, -date))
    sdv <- apply(X, 2, sd, na.rm=TRUE)
    keep <- which(is.finite(sdv) & sdv > 1e-8)
    if (length(keep) < 3L) next
    X <- X[, keep, drop = FALSE]
    X <- scale(X)

    end_date <- max(sub$date)
    Xts <- xts::xts(X, order.by = sub$date)

    mspec <- multispec(replicate(ncol(X), uspec))
    dspec <- dccspec(uspec = mspec, dccOrder = c(1,1), distribution = "mvt")

    fit <- tryCatch(
      dccfit(dspec, data = Xts, fit.control = list(eval.se = FALSE, scale = TRUE)),
      error = function(e) e
    )
    if (inherits(fit, "error") || !inherits(fit, "DCCfit")) {
      if (ix %% 5L == 0L || ix == length(ends_idx))
        message(sprintf("  window %d/%d end=%s  -> skipped (no convergence)",
                        ix, length(ends_idx), as.character(end_date)))
      next
    }

    R <- rcor(fit)[,,nrow(Xts)]
    diag(R) <- 0
    iu <- upper.tri(R, diag = FALSE)
    meancorr <- if (any(iu)) mean(abs(R[iu])) else NA_real_
    rows_mean[[length(rows_mean)+1L]] <- tibble::tibble(end = end_date, meancorr = meancorr)

    A <- abs(R)
    g <- igraph::graph_from_adjacency_matrix(A, mode = "undirected", diag = FALSE, weighted = TRUE)
    ec <- igraph::eigen_centrality(g, directed = FALSE, weights = E(g)$weight, scale = TRUE)$vector
    rows_eig[[length(rows_eig)+1L]] <- tibble::tibble(end = end_date,
                                                      ticker = colnames(X), eigen = as.numeric(ec))

    if (ix %% 5L == 0L || ix == length(ends_idx)) {
      message(sprintf("  window %d/%d end=%s  k=%d  (ok)", ix, length(ends_idx),
                      as.character(end_date), ncol(X)))
    }
  }

  d_meancorr <- dplyr::bind_rows(rows_mean) |> dplyr::arrange(end)
  d_eigen    <- dplyr::bind_rows(rows_eig)  |> dplyr::arrange(end, ticker)

  dir.create("reports/exports_garch", recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(d_meancorr, "reports/exports_garch/dcc_meancorr_rolling.csv")
  readr::write_csv(d_eigen,    "reports/exports_garch/dcc_eigen_centrality.csv")

  message("[OK] Wrote:",
          "\n - reports/exports_garch/dcc_meancorr_rolling.csv (", nrow(d_meancorr), " rows)",
          "\n - reports/exports_garch/dcc_eigen_centrality.csv (", nrow(d_eigen), " rows)")
}

run_dcc_rolling()
