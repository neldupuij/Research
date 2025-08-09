source("src/R/utils_config.R"); source("src/R/step_download.R")
cfg <- read_cfg(); ensure_dirs(); download_prices(cfg)
