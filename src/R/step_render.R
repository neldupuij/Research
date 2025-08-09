render_outputs <- function(cfg){
  if (!requireNamespace("ggplot2", quietly=TRUE)) stop("Installer 'ggplot2'")
  library(ggplot2); library(readr); library(dplyr); dir.create("figures", showWarnings = FALSE)

  # 1) TCI (statique ici)
  tci <- readr::read_csv("reports/exports/total_connectedness.csv", show_col_types = FALSE)
  p1 <- ggplot(tci, aes(x = 1, y = TCI)) + geom_col() +
        coord_cartesian(ylim = c(0, 100)) +
        labs(title = "Diebold–Yilmaz Total Connectedness (static)", x = NULL, y = "TCI (%)") +
        theme_minimal()
  ggsave("figures/total_connectedness.png", p1, width = 8, height = 5, dpi = 140)

  # 2) Net spillovers (TO - FROM)
  if (file.exists("reports/exports/net_spillovers.csv")){
    nets <- readr::read_csv("reports/exports/net_spillovers.csv", show_col_types = FALSE)
    p2 <- ggplot(nets, aes(x = reorder(symbol, NET), y = NET)) +
          geom_col() + coord_flip() +
          labs(title = "Net spillovers (TO - FROM)", x = NULL, y = "Net") +
          theme_minimal()
    ggsave("figures/net_spillovers.png", p2, width = 8, height = 5, dpi = 140)
  }
  log_msg("Saved figures/total_connectedness.png and (if available) figures/net_spillovers.png")
}
