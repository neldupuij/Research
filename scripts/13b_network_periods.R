#!/usr/bin/env Rscript
# 13b_network_periods.R
# Three-period network visualisation (pre-COVID / COVID era / post-COVID)
# 9 PNG figures: 3 network types x 3 periods
#
# Colour encoding (aids interpretation, no community detection):
#   VAR-DY    : net spillover — red = net emitter, blue = net receiver
#   Correlation: mean absolute correlation — gradient light→dark blue
#   MST        : MST degree — gradient light→dark teal
#
# Node size = centrality. Top 8 also show ticker label.
# Legend (number → ticker) is in paper Appendix E, NOT in figures.
#
# Output: outputs/network_periods/fig/

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(igraph)
  library(ggraph); library(ggplot2)
})

msg <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|", paste0(..., collapse=" "), "\n")

cfg     <- yaml::read_yaml("config/project.yml")
out_dir <- "outputs/network_periods"
dir.create(file.path(out_dir, "fig"), recursive=TRUE, showWarnings=FALSE)
dir.create(file.path(out_dir, "mat"), recursive=TRUE, showWarnings=FALSE)

TOP_N <- 8

# ── PERIOD DEFINITIONS ────────────────────────────────────────────────────
periods <- list(
  pre_covid  = list(label = "Pre-COVID (2010–2019)",       start = "2010-01-01", end = "2019-12-31"),
  covid      = list(label = "COVID Era (2020–mid-2023)",   start = "2020-01-01", end = "2023-06-30"),
  post_covid = list(label = "Post-COVID (Jul 2023–2024)",  start = "2023-07-01", end = "2026-12-31")
)
period_keys <- names(periods)

# ── LOAD DATA ─────────────────────────────────────────────────────────────
msg("Loading data...")
theta_long   <- read_csv("outputs/var_dy/mat/DY_theta_long.csv", show_col_types = FALSE)
returns_wide <- read_csv("data/processed/returns_weekly.csv",    show_col_types = FALSE)

# ── HELPERS ───────────────────────────────────────────────────────────────

# Alphabetical node numbers + top-N ticker label
annotate_graph <- function(g, centrality_attr) {
  sorted_names  <- sort(V(g)$name)
  num_map       <- setNames(seq_along(sorted_names), sorted_names)
  V(g)$node_num <- num_map[V(g)$name]
  cent          <- vertex_attr(g, centrality_attr)
  top_idx       <- order(cent, decreasing = TRUE)[seq_len(min(TOP_N, length(cent)))]
  V(g)$top_label <- ifelse(seq_len(vcount(g)) %in% top_idx, V(g)$name, "")
  g
}

export_legend_csv <- function(tickers, prefix) {
  df  <- data.frame(num = seq_along(sort(tickers)), ticker = sort(tickers))
  out <- file.path(out_dir, "mat", paste0("legend_", prefix, ".csv"))
  write.csv(df, out, row.names = FALSE)
}

net_theme <- function(title, subtitle) {
  list(
    labs(title = title, subtitle = subtitle),
    theme_graph(base_family = "sans", base_size = 10),
    theme(
      legend.position   = "right",
      plot.title        = element_text(size = 12, face = "bold"),
      plot.subtitle     = element_text(size = 8,  color = "grey40"),
      plot.background   = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    )
  )
}


# ══════════════════════════════════════════════════════════════════════════
# 1.  VAR-DY DIRECTED SPILLOVER NETWORKS
#     Colour = net spillover (red = emitter, blue = receiver)
# ══════════════════════════════════════════════════════════════════════════
msg("--- VAR-DY networks ---")

for (pk in period_keys) {
  per <- periods[[pk]]
  msg("  Period: ", per$label)

  sub <- theta_long |>
    filter(date >= as.Date(per$start), date <= as.Date(per$end), i != j) |>
    group_by(i, j) |>
    summarise(weight = mean(theta, na.rm = TRUE), .groups = "drop")

  # Period-average net spillover per node (TO - FROM)
  to_i   <- sub |> group_by(i) |> summarise(to_sum   = sum(weight), .groups = "drop")
  from_i <- sub |> group_by(j) |> summarise(from_sum = sum(weight), .groups = "drop") |>
    rename(i = j)
  net_df <- full_join(to_i, from_i, by = "i") |>
    mutate(to_sum   = ifelse(is.na(to_sum),   0, to_sum),
           from_sum = ifelse(is.na(from_sum), 0, from_sum),
           net      = to_sum - from_sum)

  # Keep top 15 % of edges by weight
  thresh <- quantile(sub$weight, 0.85, na.rm = TRUE)
  sub    <- filter(sub, weight >= thresh)

  tickers <- sort(unique(c(sub$i, sub$j)))
  nodes   <- data.frame(name = tickers) |>
    left_join(net_df, by = c("name" = "i"))

  g <- graph_from_data_frame(sub, directed = TRUE, vertices = nodes)
  V(g)$centrality <- strength(g, mode = "all")
  g <- annotate_graph(g, "centrality")

  export_legend_csv(tickers, paste0("vardy_", pk))

  set.seed(42)
  p_net <- ggraph(g, layout = "fr", niter = 800) +
    geom_edge_arc(
      aes(width = weight, alpha = weight),
      color    = "#607D8B",
      arrow    = arrow(length = unit(2, "mm"), type = "closed"),
      end_cap  = circle(4, "mm"), strength = 0.18
    ) +
    scale_edge_width(range = c(0.2, 1.8),  guide = "none") +
    scale_edge_alpha(range = c(0.15, 0.75), guide = "none") +
    geom_node_point(aes(size = centrality, color = net), alpha = 0.95) +
    scale_color_gradient2(
      low      = "#2980B9",   # blue  = net receiver
      mid      = "#ECF0F1",   # white = neutral
      high     = "#E74C3C",   # red   = net emitter
      midpoint = 0,
      name     = "Net spillover\n(+ emitter / − receiver)"
    ) +
    geom_node_text(aes(label = node_num), size = 2.0, color = "grey20", fontface = "bold") +
    geom_node_label(aes(label = top_label), size = 2.5, fontface = "bold",
                    color = "#1A3A6B", fill = alpha("white", 0.85),
                    label.padding = unit(0.12, "lines"), label.size = 0.2,
                    nudge_y = 0.10, repel = TRUE, max.overlaps = 20) +
    scale_size(range = c(4, 12), name = "Strength") +
    net_theme(
      title    = paste0("VAR–DY Spillover Network — ", per$label),
      subtitle = paste0("Top 15 % directed spillovers. ",
                        "Colour: red = net emitter, blue = net receiver. ",
                        "Size = total strength. Bold = top ", TOP_N, " (see Appendix for node index).")
    )

  out_path <- file.path(out_dir, "fig", paste0("network_vardy_", pk, ".png"))
  ggsave(out_path, p_net, width = 13, height = 8.5, dpi = 200)
  msg("    Saved: ", basename(out_path))
}


# ══════════════════════════════════════════════════════════════════════════
# 2.  PEARSON CORRELATION NETWORKS
#     Colour = mean absolute correlation (how connected a node is on average)
# ══════════════════════════════════════════════════════════════════════════
msg("--- Pearson correlation networks ---")
CORR_THRESH <- 0.50

for (pk in period_keys) {
  per <- periods[[pk]]
  msg("  Period: ", per$label)

  ret <- returns_wide |>
    filter(date >= as.Date(per$start), date <= as.Date(per$end)) |>
    select(-date)
  ret <- ret[, colSums(!is.na(ret) & ret != 0) >= 30, drop = FALSE]

  corr_mat <- cor(ret, use = "pairwise.complete.obs")
  tickers  <- rownames(corr_mat)

  # Mean absolute correlation per node (off-diagonal)
  diag(corr_mat) <- NA
  mean_corr <- rowMeans(abs(corr_mat), na.rm = TRUE)
  diag(corr_mat) <- 1

  edges <- data.frame(
    from   = rep(tickers, each  = length(tickers)),
    to     = rep(tickers, times = length(tickers)),
    weight = as.vector(corr_mat)
  ) |> filter(from < to, abs(weight) >= CORR_THRESH)

  nodes <- data.frame(name = tickers, mean_corr = mean_corr)
  g     <- graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  V(g)$centrality <- degree(g)
  g <- annotate_graph(g, "centrality")

  export_legend_csv(tickers, paste0("corr_", pk))

  set.seed(42)
  p_net <- ggraph(g, layout = "fr", niter = 800) +
    geom_edge_link(aes(width = abs(weight), alpha = abs(weight)), color = "#90A4AE") +
    scale_edge_width(range = c(0.2, 1.8),  guide = "none") +
    scale_edge_alpha(range = c(0.10, 0.65), guide = "none") +
    geom_node_point(aes(size = centrality, color = mean_corr), alpha = 0.95) +
    scale_color_gradient(
      low  = "#AED6F1",   # light blue = weakly connected on average
      high = "#1A5276",   # dark blue  = strongly connected on average
      name = "Mean |ρ|\n(avg correlation)"
    ) +
    geom_node_text(aes(label = node_num), size = 2.0, color = "white", fontface = "bold") +
    geom_node_label(aes(label = top_label), size = 2.5, fontface = "bold",
                    color = "#1A3A6B", fill = alpha("white", 0.85),
                    label.padding = unit(0.12, "lines"), label.size = 0.2,
                    nudge_y = 0.10, repel = TRUE, max.overlaps = 20) +
    scale_size(range = c(4, 12), name = "Degree") +
    net_theme(
      title    = paste0("Pearson Correlation Network — ", per$label),
      subtitle = paste0("Edges: |ρ| ≥ ", CORR_THRESH,
                        ". Colour: dark = high avg correlation. ",
                        "Size = degree. Bold = top ", TOP_N, " (see Appendix for node index).")
    )

  out_path <- file.path(out_dir, "fig", paste0("network_corr_", pk, ".png"))
  ggsave(out_path, p_net, width = 13, height = 8.5, dpi = 200)
  msg("    Saved: ", basename(out_path))
}


# ══════════════════════════════════════════════════════════════════════════
# 3.  MINIMUM SPANNING TREE NETWORKS  (Mantegna distance)
#     Colour = MST degree (hub nodes stand out)
# ══════════════════════════════════════════════════════════════════════════
msg("--- MST networks ---")

for (pk in period_keys) {
  per <- periods[[pk]]
  msg("  Period: ", per$label)

  ret <- returns_wide |>
    filter(date >= as.Date(per$start), date <= as.Date(per$end)) |>
    select(-date)
  ret <- ret[, colSums(!is.na(ret) & ret != 0) >= 30, drop = FALSE]

  corr_mat <- cor(ret, use = "pairwise.complete.obs")
  dist_mat <- sqrt(2 * pmax(1 - corr_mat, 0))
  diag(dist_mat) <- 0
  tickers  <- rownames(corr_mat)

  g_full <- graph_from_adjacency_matrix(dist_mat, mode = "undirected",
                                        weighted = TRUE, diag = FALSE)
  g_mst  <- mst(g_full)
  E(g_mst)$corr <- 1 - (E(g_mst)$weight^2) / 2

  V(g_mst)$centrality <- degree(g_mst)
  g_mst <- annotate_graph(g_mst, "centrality")

  export_legend_csv(tickers, paste0("mst_", pk))

  set.seed(42)
  p_net <- ggraph(g_mst, layout = "fr", niter = 800) +
    geom_edge_link(aes(width = corr, alpha = corr), color = "#546E7A") +
    scale_edge_width(range = c(0.3, 2.2),  guide = "none") +
    scale_edge_alpha(range = c(0.25, 0.90), guide = "none") +
    geom_node_point(aes(size = centrality, color = centrality), alpha = 0.95) +
    scale_color_gradient(
      low  = "#A2D9CE",   # light teal = leaf node (degree 1)
      high = "#0E6655",   # dark teal  = hub node (high degree)
      name = "MST degree\n(hub = dark)"
    ) +
    geom_node_text(aes(label = node_num), size = 2.0, color = "white", fontface = "bold") +
    geom_node_label(aes(label = top_label), size = 2.5, fontface = "bold",
                    color = "#1A3A6B", fill = alpha("white", 0.85),
                    label.padding = unit(0.12, "lines"), label.size = 0.2,
                    nudge_y = 0.10, repel = TRUE, max.overlaps = 20) +
    scale_size(range = c(4, 12), name = "MST degree") +
    net_theme(
      title    = paste0("Minimum Spanning Tree — ", per$label),
      subtitle = paste0("Mantegna distance / Kruskal's algorithm. ",
                        "Colour: dark teal = hub, light = leaf. ",
                        "Bold = top ", TOP_N, " (see Appendix for node index).")
    )

  out_path <- file.path(out_dir, "fig", paste0("network_mst_", pk, ".png"))
  ggsave(out_path, p_net, width = 13, height = 8.5, dpi = 200)
  msg("    Saved: ", basename(out_path))
}

msg("Done. 9 figures in outputs/network_periods/fig/")
