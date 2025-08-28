#!/usr/bin/env Rscript
# scripts/01_download.R
suppressPackageStartupMessages({
  library(tidyverse)
})
source("src/R/utils_config.R")
source("src/R/step_download.R")

cfg <- read_config()
step_download(cfg)