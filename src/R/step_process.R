compute_returns <- function(cfg){
  library(tidyverse)
  prices <- readr::read_csv("data/processed/prices_raw.csv", show_col_types = FALSE) %>%
            dplyr::select(symbol, date, adjusted)
  returns <- prices %>%
    dplyr::group_by(symbol) %>% dplyr::arrange(date) %>%
    dplyr::mutate(ret = if (cfg$returns=="log") log(adjusted/lag(adjusted)) else adjusted/lag(adjusted)-1) %>%
    tidyr::drop_na()
  readr::write_csv(returns, "data/processed/returns_panel.csv")
  log_msg("Saved data/processed/returns_panel.csv")
}
