read_cfg <- function(path="config/project.yml"){
  if (!requireNamespace("yaml", quietly=TRUE)) stop("Veuillez installer le package 'yaml'")
  yaml::read_yaml(path)
}
ensure_dirs <- function(){
  dirs <- c("data/processed","figures","reports/exports","logs")
  for (d in dirs) if (!dir.exists(d)) dir.create(d, recursive=TRUE)
}
log_msg <- function(...){ cat(format(Sys.time(), "%F %T"), "-", ..., "\n") }
