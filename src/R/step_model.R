# src/R/step_model.R
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(vars)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- GFEVD généralisée (Pesaran–Shin) -----------------------------------------

.build_phis <- function(A_list, H) {
  k <- nrow(A_list[[1]]); p <- length(A_list)
  Phi <- vector("list", H)
  Phi[[1]] <- diag(k)                       # h = 0
  for (h in 2:H) {                          # h = 1..H-1 (index R)
    acc <- matrix(0, k, k)
    for (m in 1:min(p, h - 1)) acc <- acc + A_list[[m]] %*% Phi[[h - m]]
    Phi[[h]] <- acc
  }
  Phi
}

.gfevd_generalized <- function(A_list, Sigma, H) {
  k <- nrow(A_list[[1]])
  Phi <- .build_phis(A_list, H)
  S_num <- matrix(0, k, k)                  # numérateurs (somme des carrés)
  den   <- rep(0, k)                        # dénominateurs par i

  for (h in 1:H) {
    M  <- Phi[[h]] %*% Sigma                # k x k
    S_num <- S_num + (M * M)                # (e_i' Phi Σ e_j)^2 par éléments
    den <- den + diag(Phi[[h]] %*% Sigma %*% t(Phi[[h]]))
  }

  inv_sigma_jj <- 1 / diag(Sigma)           # division par σ_jj (par colonne)
  S_scaled <- sweep(S_num, 2, inv_sigma_jj, `*`)
  row_sums <- rowSums(S_scaled)
  TH <- sweep(S_scaled, 1, row_sums, `/`) * 100
  TH[!is.finite(TH)] <- 0
  TH
}

# --- Étape MODEL ---------------------------------------------------------------

step_model <- function(cfg) {
  t0 <- Sys.time()
  H  <- as.integer(cfg$forecast_horizon %||% 10)
  message(format(t0, "%Y-%m-%d %H:%M:%S"), " - Model: start (H=", H, ")")

  # 1) Panel aligné -> matrice T x k
  panel <- readr::read_csv("data/processed/returns_panel.csv", show_col_types = FALSE)
  wide  <- panel %>%
    dplyr::select(symbol, date, return) %>%
    tidyr::pivot_wider(names_from = symbol, values_from = return) %>%
    dplyr::arrange(date) %>%
    tidyr::drop_na()

  dates <- wide$date
  X     <- as.matrix(wide %>% dplyr::select(-date))
  k     <- ncol(X);  n <- nrow(X)
  if (k < 2 || n < 50) stop("Not enough data: k=", k, ", n=", n)

  # 2) Choix de p par BIC (SC(n))
  sel <- vars::VARselect(X, lag.max = cfg$var_max_lag %||% 5, type = "const")
  p   <- as.integer(sel$selection["SC(n)"]); if (!is.finite(p) || p < 1) p <- 1
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Selected VAR lag by BIC: p=", p, " (k=", k, ", n=", n, ")")

  # 3) Estimation VAR(p)
  varfit <- vars::VAR(X, p = p, type = "const")
  A_list <- vars::Acoef(varfit)                    # A_1..A_p (k x k)
  U      <- residuals(varfit)                      # T x k
  U      <- as.matrix(U)
  Sigma  <- crossprod(U) / nrow(U)                 # covariance résiduelle (MLE)

  # 4) GFEVD généralisée (k x k, % par ligne)
  TH <- .gfevd_generalized(A_list, Sigma, H)
  rownames(TH) <- colnames(TH) <- colnames(X)

  # 5) Indices
  row_sums <- rowSums(TH)
  r_mean   <- mean(row_sums)
  off_sum  <- sum(TH) - sum(diag(TH))
  tci_val  <- as.numeric((off_sum / (k * r_mean)) * 100)

  FROM <- rowSums(TH) - diag(TH)
  TO   <- colSums(TH) - diag(TH)
  NET  <- TO - FROM

  # 6) Exports
  dir.create("reports/exports", showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(
    tibble::tibble(metric = "TCI", value = tci_val,
                   k = k, n_obs = n, p = p, H = H,
                   start = min(dates), end = max(dates)),
    "reports/exports/total_connectedness.csv"
  )
  readr::write_csv(
    tibble::tibble(symbol = colnames(X),
                   FROM = as.numeric(FROM),
                   TO   = as.numeric(TO),
                   NET  = as.numeric(NET)),
    "reports/exports/net_spillovers.csv"
  )
  readr::write_csv(
    tibble::as_tibble(TH, .name_repair = "minimal") %>%
      dplyr::mutate(`_row` = rownames(TH)) %>% dplyr::relocate(`_row`),
    "reports/exports/spillover_table.csv"
  )
  readr::write_csv(
    tibble::as_tibble(TH, .name_repair = "minimal") %>%
      dplyr::mutate(`_row` = rownames(TH)) %>% dplyr::relocate(`_row`),
    "reports/exports/spillover_matrix_kxk.csv"
  )

  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Saved spillover_table.csv, spillover_matrix_kxk.csv, total_connectedness.csv (TCI=",
          sprintf("%.2f", tci_val), "), net_spillovers.csv")
  invisible(list(TCI = tci_val, p = p, k = k, n = n))
}
