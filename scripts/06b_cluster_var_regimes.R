#!/usr/bin/env Rscript
# =============================================================================
# 06b_cluster_var_regimes.R
# Connectedness regime identification via k-means clustering on rolling GFEVD
# feature vectors (Diebold-Yilmaz).
#
# Feature vector per window (5 interpretable summary statistics):
#   (1) TCI          — system-wide connectedness level
#   (2) sd_NET       — cross-sectional dispersion of net spillovers
#   (3) max_NET      — largest single-institution net spillover
#   (4) top5_to_share— share of TO connectedness in top-5 institutions
#   (5) skew_NET     — skewness of net spillover distribution
#
# K selection: silhouette coefficient + within-cluster SS (elbow), K=2..6.
# Optimal K is taken as argmax(mean silhouette).
#
# Produces:
#   outputs/cluster_regimes/mat/REGIME_assignments.csv
#   outputs/cluster_regimes/mat/REGIME_profiles.csv
#   outputs/cluster_regimes/mat/REGIME_silhouettes.csv
#   outputs/cluster_regimes/fig/regime_timeline.png
#   outputs/cluster_regimes/fig/regime_silhouette.png
#   outputs/cluster_regimes/fig/regime_profiles.png
# =============================================================================

suppressPackageStartupMessages({
  library(yaml); library(readr); library(dplyr); library(tidyr)
  library(tibble); library(cluster); library(ggplot2)
})

`%||%` <- function(x, y) if (is.null(x)) y else x
msg    <- function(...) cat(format(Sys.time(), "%H:%M:%S"), "|",
                            paste0(..., collapse = " "), "\n")

# =============================================================================
# CONFIG
# =============================================================================
cfg      <- yaml::read_yaml("config/project.yml")
out_base <- cfg$paths$out_cluster %||% "outputs/cluster_regimes"

out_mat <- file.path(out_base, "mat"); dir.create(out_mat, recursive=TRUE, showWarnings=FALSE)
out_fig <- file.path(out_base, "fig"); dir.create(out_fig, recursive=TRUE, showWarnings=FALSE)

var_mat_dir <- cfg$paths$out_var_dy %||% "outputs/var_dy"
var_mat_dir <- file.path(var_mat_dir, "mat")

# =============================================================================
# LOAD DATA
# =============================================================================
tci_df <- readr::read_csv(file.path(var_mat_dir, "DY_tci.csv"), show_col_types=FALSE)
net_df <- readr::read_csv(file.path(var_mat_dir, "DY_net.csv"), show_col_types=FALSE)
to_df  <- readr::read_csv(file.path(var_mat_dir, "DY_to.csv"),  show_col_types=FALSE)

stopifnot(nrow(tci_df) == nrow(net_df), nrow(tci_df) == nrow(to_df))

dates  <- as.Date(tci_df$date)
tci    <- tci_df$TCI
n_win  <- length(dates)

# NET and TO matrices: rows = windows, cols = institutions
net_mat <- as.matrix(net_df)
to_mat  <- as.matrix(to_df)

msg("Loaded ", n_win, " windows x ", ncol(net_mat), " institutions")

# =============================================================================
# BUILD FEATURE VECTORS
# =============================================================================
skewness <- function(x) {
  n <- length(x[!is.na(x)])
  m <- mean(x, na.rm=TRUE)
  s <- sd(x, na.rm=TRUE)
  if (is.na(s) || s < 1e-12) return(0)
  mean(((x - m) / s)^3, na.rm=TRUE)
}

features <- tibble(
  date         = dates,
  TCI          = tci,
  sd_NET       = apply(net_mat, 1, sd,      na.rm=TRUE),
  max_NET      = apply(net_mat, 1, max,     na.rm=TRUE),
  top5_to_share = apply(to_mat,  1, function(r) {
    r <- r[!is.na(r)]
    sum(sort(r, decreasing=TRUE)[1:min(5, length(r))]) / sum(r)
  }),
  skew_NET     = apply(net_mat, 1, skewness)
)

msg("Feature matrix: ", nrow(features), " x ", ncol(features) - 1L)

# Scale (zero mean, unit variance per feature)
feat_names <- c("TCI","sd_NET","max_NET","top5_to_share","skew_NET")
X_raw <- as.matrix(features[, feat_names])
X     <- scale(X_raw)

# =============================================================================
# K SELECTION: silhouette + within-cluster SS
# =============================================================================
set.seed(42L)
K_RANGE <- 2:6

sil_scores <- numeric(length(K_RANGE))
wss_scores <- numeric(length(K_RANGE))

for (i in seq_along(K_RANGE)) {
  k   <- K_RANGE[i]
  km  <- kmeans(X, centers=k, nstart=50L, iter.max=200L)
  sil <- cluster::silhouette(km$cluster, dist(X))
  sil_scores[i] <- mean(sil[, "sil_width"])
  wss_scores[i] <- km$tot.withinss
  msg("  k=", k, " | mean silhouette=", round(sil_scores[i], 4),
      " | WSS=", round(wss_scores[i], 2))
}

k_opt <- K_RANGE[which.max(sil_scores)]
msg("Optimal K = ", k_opt, " (max silhouette = ", round(max(sil_scores), 4), ")")

# Save silhouette table
sil_df <- tibble(k=K_RANGE, mean_silhouette=round(sil_scores, 4),
                 wss=round(wss_scores, 2))
readr::write_csv(sil_df, file.path(out_mat, "REGIME_silhouettes.csv"))
msg("REGIME_silhouettes.csv written.")

# =============================================================================
# FINAL CLUSTERING WITH k_opt
# =============================================================================
set.seed(42L)
km_final <- kmeans(X, centers=k_opt, nstart=100L, iter.max=500L)

# Order regimes by mean TCI (regime 1 = lowest TCI, etc.)
regime_tci <- tapply(features$TCI, km_final$cluster, mean)
rank_order  <- order(regime_tci)
relabel     <- setNames(seq_len(k_opt), rank_order)
regime_lbl  <- paste0("Regime ", relabel[as.character(km_final$cluster)])

features$regime <- regime_lbl

# Assign meaningful names based on TCI level
regime_means <- features %>%
  group_by(regime) %>%
  summarise(mean_TCI = mean(TCI), .groups="drop") %>%
  arrange(mean_TCI)

# =============================================================================
# WRITE CSV OUTPUTS
# =============================================================================
# 1. Window-level assignments
readr::write_csv(
  features %>% select(date, TCI, sd_NET, max_NET, top5_to_share, skew_NET, regime),
  file.path(out_mat, "REGIME_assignments.csv")
)
msg("REGIME_assignments.csv written.")

# 2. Cluster mean profiles (unscaled features + silhouette)
sil_final <- cluster::silhouette(km_final$cluster, dist(X))
sil_by_regime <- tapply(sil_final[, "sil_width"],
                         regime_lbl, mean)

sil_regime_df <- tibble(
  regime           = names(sil_by_regime),
  mean_silhouette  = round(as.numeric(sil_by_regime), 4)
)

profiles <- features %>%
  group_by(regime) %>%
  summarise(
    n_windows    = n(),
    mean_TCI     = round(mean(TCI), 2),
    mean_sd_NET  = round(mean(sd_NET), 2),
    mean_max_NET = round(mean(max_NET), 2),
    mean_top5_to = round(mean(top5_to_share), 3),
    mean_skew    = round(mean(skew_NET), 3),
    .groups="drop"
  ) %>%
  left_join(sil_regime_df, by="regime")

readr::write_csv(profiles, file.path(out_mat, "REGIME_profiles.csv"))
msg("REGIME_profiles.csv written.")
print(profiles)

# =============================================================================
# FIGURE 1: TCI timeline coloured by regime
# =============================================================================
regime_cols <- c(
  "Regime 1" = "#1f77b4",
  "Regime 2" = "#2ca02c",
  "Regime 3" = "#d62728",
  "Regime 4" = "#ff7f0e",
  "Regime 5" = "#9467bd"
)[unique(sort(features$regime))]

png(file.path(out_fig, "regime_timeline.png"), width=2800, height=900, res=220)
p1 <- ggplot(features, aes(x=date, y=TCI, colour=regime, group=1)) +
  geom_line(linewidth=0.6, alpha=0.3, colour="grey60") +
  geom_point(aes(colour=regime), size=1.4, alpha=0.9) +
  scale_colour_manual(values=regime_cols, name="Regime") +
  scale_x_date(date_breaks="2 years", date_labels="%Y") +
  labs(
    title    = paste0("Connectedness regimes identified by k-means (k=", k_opt, ")"),
    subtitle = "Each point is a rolling window; colour encodes the assigned regime",
    x=NULL, y="TCI (%)"
  ) +
  theme_bw(base_size=11) +
  theme(legend.position="bottom", plot.title=element_text(face="bold"))
print(p1)
dev.off()
msg("regime_timeline.png written.")

# =============================================================================
# FIGURE 2: Silhouette + elbow plot
# =============================================================================
png(file.path(out_fig, "regime_silhouette.png"), width=2200, height=850, res=200)
par(mfrow=c(1,2), mar=c(4,4,3,1))

plot(sil_df$k, sil_df$mean_silhouette, type="b", pch=19,
     col=ifelse(sil_df$k == k_opt, "#d62728", "#1f77b4"),
     xlab="Number of clusters K", ylab="Mean silhouette width",
     main="K selection — silhouette")
abline(v=k_opt, lty=2, col="#d62728")
text(k_opt, max(sil_df$mean_silhouette) * 0.98,
     paste0("K*=", k_opt), col="#d62728", cex=0.9, pos=4)

plot(sil_df$k, sil_df$wss, type="b", pch=19,
     col=ifelse(sil_df$k == k_opt, "#d62728", "#1f77b4"),
     xlab="Number of clusters K", ylab="Within-cluster SS",
     main="K selection — elbow (WSS)")
abline(v=k_opt, lty=2, col="#d62728")

dev.off()
msg("regime_silhouette.png written.")

# =============================================================================
# FIGURE 3: Regime profiles (bar chart of mean features)
# =============================================================================
profiles_long <- profiles %>%
  select(regime, mean_TCI, mean_sd_NET, mean_max_NET, mean_top5_to) %>%
  pivot_longer(-regime, names_to="feature", values_to="value") %>%
  mutate(feature = recode(feature,
    "mean_TCI"     = "TCI (%)",
    "mean_sd_NET"  = "σ(NET)",
    "mean_max_NET" = "max NET",
    "mean_top5_to" = "Top-5 TO share"
  ))

png(file.path(out_fig, "regime_profiles.png"), width=2400, height=900, res=200)
p3 <- ggplot(profiles_long, aes(x=regime, y=value, fill=regime)) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~feature, scales="free_y", ncol=4) +
  scale_fill_manual(values=regime_cols) +
  labs(
    title    = paste0("Mean feature profiles by connectedness regime (k=", k_opt, ")"),
    subtitle = "Each panel shows one feature; bars show the within-regime time-average",
    x=NULL, y=NULL
  ) +
  theme_bw(base_size=11) +
  theme(
    axis.text.x = element_text(angle=30, hjust=1),
    plot.title  = element_text(face="bold")
  )
print(p3)
dev.off()
msg("regime_profiles.png written.")

msg("Done. All outputs in ", out_base)
