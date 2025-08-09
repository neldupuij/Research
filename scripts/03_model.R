source("src/R/utils_config.R"); source("src/R/step_model.R")
cfg <- read_cfg(); ensure_dirs(); estimate_spillovers(cfg)
