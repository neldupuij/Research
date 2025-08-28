#!/usr/bin/env Rscript
# scripts/03_model.R
suppressPackageStartupMessages({
  library(tidyverse)
})
source("src/R/utils_config.R")
source("src/R/step_model.R")

cfg <- read_config()
step_model(cfg)