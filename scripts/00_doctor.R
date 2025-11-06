#!/usr/bin/env Rscript

ok <- TRUE

req_funcs <- c(
  "src/R/utils_config.R",
  "src/R/step_download.R",
  "src/R/step_process.R",
  "src/R/step_model.R"
)

req_scripts <- c(
  "scripts/01_download.R",
  "scripts/02_process.R",
  "scripts/03_model.R"
)

# Check presence
for (f in c(req_funcs, req_scripts)) {
  if (!file.exists(f)) {
    message("Missing: ", f)
    ok <- FALSE
  }
}

# Check python plotter exists
if (!file.exists("src/python/plot_connectedness.py")) {
  message("Missing: src/python/plot_connectedness.py")
  ok <- FALSE
}

if (ok) {
  message("âœ… Doctor: structure OK (R CSV exports + Python plots).")
} else {
  quit(status = 1)
}
