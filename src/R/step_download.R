download_prices <- function(cfg){
  pkgs <- c("tidyverse","tidyquant")
  invisible(lapply(pkgs, require, character.only=TRUE))
  log_msg("Download:", paste(cfg$tickers, collapse=", "))
  prices <- tidyquant::tq_get(cfg$tickers,
                              from = as.Date(cfg$start_date),
                              to   = if (is.null(cfg$end_date)) Sys.Date() else as.Date(cfg$end_date))
  readr::write_csv(prices, "data/processed/prices_raw.csv")
  log_msg("Saved data/processed/prices_raw.csv")
}
