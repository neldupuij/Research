suppressPackageStartupMessages({
  library(yaml)
})

source("src/R/step_process.R")
source("src/R/step_model.R")

cfg <- yaml::read_yaml("config/project.yml")

cfg$min_coverage_ratio <- if (!is.null(cfg$min_coverage_pct)) as.numeric(cfg$min_coverage_pct)/100 else 0.90

cat(sprintf("Config: W=%s, step=%s, H=%s, select_p_mode=%s, p_fixed=%s\n",
            as.character(cfg$rolling_window),
            as.character(cfg$rolling_step),
            as.character(cfg$forecast_horizon),
            as.character(cfg$select_p_mode %||% "per_window"),
            as.character(cfg$p_fixed %||% NA)))

step_process(cfg)
step_model(cfg)
