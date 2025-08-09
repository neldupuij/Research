suppressPackageStartupMessages({library(yaml); library(tools)})
ok <- TRUE
req_dirs <- c("config","src/R","scripts","data","data/processed","reports/exports","figures")
for (d in req_dirs) if (!dir.exists(d)) {cat("❌ Missing dir:", d, "\n"); ok <- FALSE}

cfg_path <- "config/project.yml"
if (!file.exists(cfg_path)) {cat("❌ Missing config/project.yml\n"); ok <- FALSE} else {
  cfg <- yaml::read_yaml(cfg_path)
  if (is.null(cfg$tickers) || length(cfg$tickers)<2) {cat("❌ cfg$tickers manquants/faibles\n"); ok <- FALSE}
  if (is.null(cfg$rolling_window)) cat("⚠️ cfg$rolling_window absent (ok pour l’instant)\n")
  cat("✅ Config OK —", paste(cfg$tickers, collapse=", "), "\n")
}

req_funcs <- c("src/R/utils_config.R","src/R/step_download.R","src/R/step_process.R","src/R/step_model.R","src/R/step_render.R")
for (f in req_funcs) if (!file.exists(f) || file.size(f)==0) {cat("❌ Missing/empty:", f, "\n"); ok <- FALSE}

req_scripts <- c("scripts/01_download.R","scripts/02_process.R","scripts/03_model.R","scripts/04_render.R","scripts/run_pipeline.R")
for (s in req_scripts) if (!file.exists(s) || file.size(s)==0) {cat("❌ Missing/empty:", s, "\n"); ok <- FALSE}

# si outputs présents, faire 2 sanity checks
if (file.exists("reports/exports/spillover_matrix_kxk.csv")) {
  X <- as.matrix(read.csv("reports/exports/spillover_matrix_kxk.csv", row.names=1, check.names=FALSE))
  rs <- round(rowSums(X), 2)
  cat("✅ Row sums (should be ≈100 or 100/k):", paste(rs, collapse=", "), "\n")
}
if (file.exists("reports/exports/net_spillovers.csv")) {
  d <- read.csv("reports/exports/net_spillovers.csv")
  cat("✅ Sum(NET) =", round(sum(d$NET), 6), "(should be 0)\n")
}

cat(if (ok) "🎯 Structure OK\n" else "⛔ À corriger\n")
