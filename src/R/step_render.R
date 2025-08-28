# src/R/step_render.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(ggplot2)
})

step_render <- function() {
  dir.create("figures", showWarnings = FALSE, recursive = TRUE)

  # 1) TCI
  if (file.exists("reports/exports/total_connectedness.csv")) {
    tci <- readr::read_csv("reports/exports/total_connectedness.csv", show_col_types = FALSE)
    p <- ggplot(tci, aes(x = "TCI", y = value)) +
      geom_col() +
      coord_cartesian(ylim = c(0, 100)) +
      labs(title = "Total Connectedness Index (0–100)",
           x = NULL, y = "Percent") +
      theme_minimal(base_size = 12)
    ggsave("figures/total_connectedness.png", p, width = 6, height = 4, dpi = 150)
  }

  # 2) NET
  if (file.exists("reports/exports/net_spillovers.csv")) {
    net <- readr::read_csv("reports/exports/net_spillovers.csv", show_col_types = FALSE)
    net <- net %>% arrange(NET) %>%
      mutate(symbol = factor(symbol, levels = symbol))
    p2 <- ggplot(net, aes(x = symbol, y = NET)) +
      geom_col() +
      coord_flip() +
      labs(title = "Net Spillovers (TO − FROM)",
           x = NULL, y = "Percent points (FEVD)") +
      theme_minimal(base_size = 12)
    ggsave("figures/net_spillovers.png", p2, width = 7, height = 6, dpi = 150)
  }

  message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          " - Saved figures/total_connectedness.png and (if available) figures/net_spillovers.png")
}