estimate_spillovers <- function(cfg){
  pkgs <- c("tidyverse","vars","Spillover")
  invisible(lapply(pkgs, require, character.only=TRUE))

  ret <- readr::read_csv("data/processed/returns_panel.csv", show_col_types = FALSE) %>%
         dplyr::select(date, symbol, ret) %>%
         tidyr::pivot_wider(names_from = symbol, values_from = ret) %>%
         tidyr::drop_na()
  Xdf <- ret %>% dplyr::select(-date)
  tickers <- colnames(Xdf); k <- ncol(Xdf)

  lag_sel <- as.integer(vars::VARselect(as.matrix(Xdf), lag.max = 10, type = "const")$selection["SC(n)"])
  var_fit <- vars::VAR(Xdf, p = lag_sel, type = "const")

  gtab <- Spillover::G.spillover(var_fit, n.ahead = cfg$forecast_horizon, standardized = TRUE)

  GT <- as.matrix(gtab)
  idx_c <- match(tickers, colnames(GT))
  idx_r <- match(tickers, rownames(GT))
  M <- GT[idx_r, idx_c, drop = FALSE]   # bloc k×k (lignes normalisées à 100)

  off_sum <- sum(M) - sum(diag(M))
  TCI <- off_sum / k                    # pas de *100 ici

  to    <- colSums(M) - diag(M)
  from  <- rowSums(M) - diag(M)
  net   <- to - from

  dir.create("reports/exports", showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(tibble::as_tibble(GT, .name_repair = "unique"), "reports/exports/spillover_table.csv")
  # nouvelle sortie propre k×k avec noms préservés
  write.csv(M, "reports/exports/spillover_matrix_kxk.csv", row.names = TRUE, quote = FALSE)
  readr::write_csv(tibble::tibble(TCI = as.numeric(TCI)), "reports/exports/total_connectedness.csv")
  readr::write_csv(tibble::tibble(symbol = tickers, TO = as.numeric(to), FROM = as.numeric(from), NET = as.numeric(net)),
                   "reports/exports/net_spillovers.csv")
  saveRDS(list(var_fit = var_fit, table_full = GT, M = M, TCI = TCI, net = net),
          file = "reports/exports/spillover_static_result.rds")

  log_msg(sprintf("Saved spillover_table.csv, spillover_matrix_kxk.csv, total_connectedness.csv (TCI=%.2f), net_spillovers.csv", TCI))
}
