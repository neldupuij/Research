#!/usr/bin/env Rscript
# scripts/02_process.R
suppressPackageStartupMessages({
  library(tidyverse)
})
source("src/R/utils_config.R")
source("src/R/step_process.R")

cfg <- read_config()
step_process(cfg)
