estimate_spillovers <- function(cfg){
  pkgs <- c("tidyverse","vars","Spillover")
  invisible(lapply(pkgs, require, character.only=TRUE))

  ret <- readr::read_csv("data/processed/returns_panel.csv", show_col_types = FALSE) %>%
         dplyr::select(date, symbol, ret) %>%
         tidyr::pivot_wider(names_from=symbol, values_from=ret) %>%
         tidyr::drop_na()
  X <- as.matrix(ret %>% dplyr::select(-date))

  lag_sel <- as.integer(VARselect(X, lag.max=10, type="const")$selection["SC(n)"])
  res <- Spillover::spilloverDY12(X, n.ahead = cfg$forecast_horizon,
                                  VAR_config = list(p = lag_sel, type="const"))

  readr::write_csv(tibble::tibble(idx = seq_along(res$TCI), TCI = res$TCI),
                   "reports/exports/total_connectedness.csv")
  saveRDS(res, file="reports/exports/spillover_object.rds")
  log_msg("Saved reports/exports/total_connectedness.csv & spillover_object.rds")
}
