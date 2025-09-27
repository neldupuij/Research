#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(yaml)
})
source("src/R/step_model.R")

cfg <- yaml::read_yaml("config/project.yml")
invisible(step_model(cfg))