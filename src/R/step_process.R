# src/R/step_process.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(lubridate)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# ÃƒÆ’Ã¢â‚¬Â°tape Process : rendements, filtrage qualitÃƒÆ’Ã‚Â©, panel alignÃƒÆ’Ã‚Â© (dates communes)
# - returns: "log" | "simple"                 (def: "log")
# - min_coverage_ratio in (0,1]               (def: 0.90)
# - min_history_years >= 0                    (def: 5)
# - drop_reason_log                           (def: "logs/exclusions.csv")
step_process <- function(cfg) {
  t0 <- Sys.time()
  message(format(t0, "%Y-%m-%d %H:%M:%S"), " - Process: start")

  returns_type <- cfg$returns %||% "log"
  min_cov      <- cfg$min_coverage_ratio %||% 0.90
  min_years    <- cfg$min_history_years %||% 5
  drop_log     <- cfg$drop_reason_log %||% "logs/exclusions.csv"

  # 1) Charger prix ajustÃƒÆ’Ã‚Â©s
  prices_path <- "data/processed/prices_raw.csv"
  if (!file.exists(prices_path)) stop("Missing ", prices_path, " (run 01_download first).")
  prices <- readr::read_csv(prices_path, show_col_types = FALSE) %>%
    dplyr::mutate(date = as.Date(date))

  if (!is.null(cfg$start_date)) prices <- dplyr::filter(prices, date >= as.Date(cfg$start_date))
  if (!is.null(cfg$end_date))   prices <- dplyr::filter(prices, date <= as.Date(cfg$end_date))

  # 2) Rendements
  compute_ret <- function(x) if (identical(returns_type, "simple")) x/lag(x)-1 else log(x/lag(x))
  rets <- prices %>%
    dplyr::arrange(symbol, date) %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(return = compute_ret(adjusted)) %>%
    ungroup() %>%
    dplyr::select(symbol, date, return)

  # 3) Large (union) pour ÃƒÆ’Ã‚Â©valuer la couverture
  wide_all <- rets %>%
    tidyr::pivot_wider(names_from = symbol, values_from = return) %>%
    dplyr::arrange(date)
  if (nrow(wide_all) == 0) stop("No observations after return computation.")

  # Historique par titre (sur les rendements)
  span_tbl <- rets %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise(
      first_date = suppressWarnings(min(date[is.finite(return)], na.rm = TRUE)),
      last_date  = suppressWarnings(max(date[is.finite(return)], na.rm = TRUE)),
      span_years = as.numeric(difftime(last_date, first_date, units = "days"))/365.25,
      .groups = "drop"
    )

  # Couverture (proportion de jours non-NA sur l'univers de dates)
  cov_vec <- sapply(wide_all %>% dplyr::select(-date), function(col) mean(!is.na(col)))
  cov_tbl <- tibble::tibble(symbol = names(cov_vec), coverage = as.numeric(cov_vec))

  qual_tbl <- cov_tbl %>%
    left_join(span_tbl, by = "symbol") %>%
    dplyr::arrange(symbol)

  # 4) Filtres
  bad_hist <- qual_tbl %>% dplyr::filter(is.finite(span_years) & span_years < min_years)
  bad_cov  <- qual_tbl %>% dplyr::filter(is.finite(coverage)  & coverage  < min_cov)

  drop_syms <- union(bad_hist$symbol, bad_cov$symbol)
  keep_syms <- setdiff(colnames(wide_all)[-1], drop_syms)

  # 5) Log + PRINT des exclusions
  if (length(drop_syms)) {
    dir.create("logs", showWarnings = FALSE)
    log_now <- tibble::tibble(
      timestamp  = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      symbol     = drop_syms,
      reason     = dplyr::case_when(
        drop_syms %in% bad_hist$symbol & drop_syms %in% bad_cov$symbol ~
          sprintf("history<%.1fy & coverage<%.0f%%", min_years, 100*min_cov),
        drop_syms %in% bad_hist$symbol ~ sprintf("history<%.1fy", min_years),
        drop_syms %in% bad_cov$symbol  ~ sprintf("coverage<%.0f%%", 100*min_cov),
        TRUE ~ "excluded"
      ),
      coverage   = round(100 * (cov_tbl$coverage[match(drop_syms, cov_tbl$symbol)]), 2),
      span_years = round(span_tbl$span_years[match(drop_syms, span_tbl$symbol)], 2)
    )
    readr::write_csv(log_now, file = drop_log, append = file.exists(drop_log))
    message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " - Excluded ", length(drop_syms), " symbol(s): ",
            paste(drop_syms, collapse = ", "))
    print(log_now %>% dplyr::select(symbol, reason, coverage, span_years) %>% dplyr::arrange(symbol), n = Inf)
  } else {
    message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            " - No exclusions by quality filters (coverage/history).")
  }

  # 6) Panel alignÃƒÆ’Ã‚Â© (dates communes)
  if (length(keep_syms) < 2) {
    stop("Fewer than 2 symbols after filtering (keep=", length(keep_syms), ").")
  }
  wide_keep <- wide_all %>%
    dplyr::select(date, all_of(keep_syms)) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(date)
  panel <- wide_keep %>%
    tidyr::pivot_longer(-date, names_to = "symbol", values_to = "return") %>%
    dplyr::arrange(symbol, date)

  # 7) Sauvegarde
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(panel, "data/processed/returns_panel.csv")
  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Saved data/processed/returns_panel.csv (",
          length(keep_syms), " symbols kept; ",
          n_distinct(panel$date), " common trading days)")
  invisible(panel)
}
