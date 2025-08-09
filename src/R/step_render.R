render_outputs <- function(cfg){
  if (!requireNamespace("ggplot2", quietly=TRUE)) stop("Installer 'ggplot2'")
  tci <- readr::read_csv("reports/exports/total_connectedness.csv", show_col_types = FALSE)
  p <- ggplot2::ggplot(tci, ggplot2::aes(idx, TCI)) +
       ggplot2::geom_line() +
       ggplot2::labs(title="Diebold–Yilmaz Total Connectedness", x="Rolling index", y="TCI")
  ggplot2::ggsave("figures/total_connectedness.png", p, width=10, height=6, dpi=140)
  log_msg("Saved figures/total_connectedness.png")
}
