# src/R/utils_config.R
suppressPackageStartupMessages({
  library(yaml)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

# Lit la config YAML et fixe des valeurs par défaut raisonnables
read_config <- function(path = "config/project.yml") {
  if (!file.exists(path)) stop("Config file not found: ", path)
  cfg <- yaml::read_yaml(path)

  cfg$project            <- cfg$project            %||% "Volatility Spillovers — EU Financials"
  cfg$tickers            <- cfg$tickers            %||% cfg$symbols
  if (is.null(cfg$tickers)) stop("No 'tickers' defined in config/project.yml")

  cfg$start_date         <- cfg$start_date         %||% cfg$from %||% NULL
  cfg$end_date           <- cfg$end_date           %||% cfg$to   %||% NULL
  cfg$returns            <- cfg$returns            %||% "log"

  cfg$forecast_horizon   <- cfg$forecast_horizon   %||% cfg$horizon_H %||% 10
  cfg$var_max_lag        <- cfg$var_max_lag        %||% 5

  # Rolling (optionnel ; non utilisé ici)
  cfg$rolling_window     <- cfg$rolling_window     %||% NULL

  # Filtres qualité (pour 30+ banques)
  cfg$min_coverage_ratio <- cfg$min_coverage_ratio %||% 0.90
  cfg$min_history_years  <- cfg$min_history_years  %||% 5
  cfg$drop_reason_log    <- cfg$drop_reason_log    %||% "logs/exclusions.csv"

  cfg
}