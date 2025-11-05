# src/R/step_download.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(tidyquant)   # tq_get (Yahoo)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# Ã‰tape Download : Yahoo Adj Close -> data/processed/prices_raw.csv
step_download <- function(cfg) {
  t0 <- Sys.time()
  message(format(t0, "%Y-%m-%d %H:%M:%S"), " - Download: ", paste(cfg$tickers, collapse = ", "))

  # Appel direct tq_get sur le vecteur de tickers (gÃ¨re une grande liste)
  prices <- tq_get(cfg$tickers,
                   get  = "stock.prices",
                   from = cfg$start_date %||% "1900-01-01",
                   to   = cfg$end_date   %||% Sys.Date()) %>%
    as_tibble()

  # Nettoyage / colonnes minimum
  if (!all(c("symbol","date","adjusted") %in% names(prices))) {
    stop("tq_get missing required columns; got: ", paste(names(prices), collapse = ", "))
  }

  out <- prices %>%
    select(symbol, date, adjusted) %>%
    mutate(date = as.Date(date)) %>%
    arrange(symbol, date)

  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(out, "data/processed/prices_raw.csv")

  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Saved data/processed/prices_raw.csv (",
          length(unique(out$symbol)), " symbols)")
  invisible(out)
}
