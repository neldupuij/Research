#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(strucchange); library(tibble)
})
`%||%` <- function(x, y) if (is.null(x)) y else x
msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse=" "), "\n")
cfg      <- yaml::read_yaml("config/project.yml")
out_var  <- cfg$paths$out_var_dy %||% "outputs/var_dy"
tci_path <- file.path(out_var, "mat", "DY_tci.csv")
out_mat  <- file.path(out_var, "mat")
out_fig  <- file.path(out_var, "fig")
dir.create(out_mat, recursive=TRUE, showWarnings=FALSE)
dir.create(out_fig, recursive=TRUE, showWarnings=FALSE)
stopifnot(file.exists(tci_path))
msg("12_structural_break_test.R | Source : ", tci_path)
tci_raw  <- read_csv(tci_path, show_col_types=FALSE)
tci_col  <- names(tci_raw)[grep("tci|TCI", names(tci_raw))[1]]
date_col <- names(tci_raw)[grep("date|Date", names(tci_raw))[1]]
tci_df   <- tci_raw |>
  rename(date=all_of(date_col), tci=all_of(tci_col)) |>
  mutate(date=as.Date(date), tci=as.numeric(tci)) |>
  filter(!is.na(date), is.finite(tci)) |>
  arrange(date)
msg("  N=", nrow(tci_df), " (", as.character(min(tci_df$date)), " -> ", as.character(max(tci_df$date)), ")")
tci_ts <- ts(tci_df$tci)
msg("  Bai-Perron...")
bp <- tryCatch(breakpoints(tci_ts~1, h=0.10, breaks=5),
               error=function(e){ msg("  WARN: ",e$message); NULL })
bp_dates <- as.Date(character(0))
all_rows <- list()
if (!is.null(bp) && !all(is.na(bp$breakpoints))) {
  bp_idx   <- bp$breakpoints[!is.na(bp$breakpoints)]
  bp_dates <- tci_df$date[bp_idx]
  ci       <- tryCatch(confint(bp), error=function(e) NULL)
  for (k in seq_along(bp_idx)) {
    all_rows[[k]] <- data.frame(
      method="Bai-Perron",
      break_date=as.character(bp_dates[k]),
      break_index=bp_idx[k],
      ci_lower=if (!is.null(ci)) as.character(tci_df$date[ci$confint[k,1]]) else NA,
      ci_upper=if (!is.null(ci)) as.character(tci_df$date[ci$confint[k,3]]) else NA,
      statistic=NA_real_, p_value=NA_real_, stringsAsFactors=FALSE)
  }
  msg("  Breakpoints: ", paste(format(bp_dates, "%Y-%m"), collapse=", "))
} else { msg("  No breakpoints found.") }
chow_date  <- as.Date("2020-03-01")
chow_idx   <- which.min(abs(tci_df$date - chow_date))
chow_exact <- tci_df$date[chow_idx]
chow_row   <- tryCatch({
  sc <- sctest(tci_ts~1, type="Chow", point=chow_idx)
  msg("  Chow (", format(chow_exact, "%Y-%m-%d"), ") : F=", round(sc$statistic,3),
      " | p=", round(sc$p.value,4), if(sc$p.value<0.05) " ** SIGNIFICANT **" else "")
  data.frame(method="Chow", break_date=as.character(chow_exact), break_index=chow_idx,
             ci_lower=NA, ci_upper=NA, statistic=round(sc$statistic,4),
             p_value=round(sc$p.value,4), stringsAsFactors=FALSE)
}, error=function(e){ msg("  WARN Chow: ",e$message); NULL })
all_rows <- c(all_rows, list(chow_row))
all_rows <- all_rows[!sapply(all_rows, is.null)]
if (length(all_rows)>0) {
  write_csv(do.call(rbind, all_rows), file.path(out_mat,"structural_breaks.csv"))
  msg("  Written: structural_breaks.csv")
}
png(file.path(out_fig,"tci_with_breakpoints.png"), width=2400, height=1000, res=220)
par(mar=c(4,4,2,2))
y_range <- range(tci_df$tci, na.rm=TRUE)
plot(tci_df$date, tci_df$tci, type="l", lwd=1.8, col="#1f77b4",
     xlab="Date", ylab="TCI (%)", main="VAR-DY TCI — Structural Breaks (Bai-Perron + Chow)")
grid(lty=3, col="grey80")
if (length(bp_dates)>0) {
  abline(v=bp_dates, col="firebrick", lwd=2, lty=2)
  text(x=bp_dates, y=y_range[2] - 0.02*(y_range[2]-y_range[1]),
       labels=format(bp_dates,"%Y-%m"), col="firebrick",
       cex=0.65, srt=90, adj=c(1, 0.5))
}
abline(v=chow_date, col="darkorange", lwd=1.5, lty=3)
legend("topleft", legend=c("TCI","Bai-Perron","Chow 2020-03"),
       col=c("#1f77b4","firebrick","darkorange"), lwd=c(1.8,2,1.5), lty=c(1,2,3), bty="n", cex=0.8)
dev.off()
msg("  Written: tci_with_breakpoints.png")
cusum_res <- tryCatch(efp(tci_ts~1, type="OLS-CUSUM"),
                      error=function(e){ msg("  WARN CUSUM: ",e$message); NULL })
if (!is.null(cusum_res)) {
  png(file.path(out_fig,"tci_cusum.png"), width=2400, height=900, res=220)
  plot(cusum_res, main="CUSUM — TCI (VAR-DY)")
  dev.off()
  msg("  Written: tci_cusum.png")
}
msg("Done.")
