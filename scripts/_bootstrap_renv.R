options(repos=c(CRAN='https://cloud.r-project.org'))
if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv')
# activer l'env du projet
source('renv/activate.R', local=TRUE)

pkgs <- c('tidyverse','xts','rmgarch','BEKKs','ggplot2','rlang')
ok <- logical(length(pkgs)); names(ok) <- pkgs

for (p in pkgs) {
  # 1) essayer de charger
  if (!suppressWarnings(require(p, character.only=TRUE, quietly=TRUE))) {
    # 2) tenter renv::install
    msg <- tryCatch({ renv::install(p); NULL }, error=function(e) e)
    # 3) re-essayer de charger
    if (!suppressWarnings(require(p, character.only=TRUE, quietly=TRUE))) {
      # 4) fallback install.packages
      try(install.packages(p), silent=TRUE)
      # 5) re-essayer une derni?re fois
      if (!suppressWarnings(require(p, character.only=TRUE, quietly=TRUE))) {
        cat('[ERR] Package introuvable apr?s tentatives:', p, '\n')
      }
    }
  }
  ok[p] <- suppressWarnings(require(p, character.only=TRUE, quietly=TRUE))
  cat('[PKG]', p, '->', if (ok[p]) 'OK' else 'FAIL', '\n')
}

# snapshot si au moins un OK
if (any(ok)) renv::snapshot(prompt=FALSE)

cat('[LIBPATHS]', paste(.libPaths(), collapse=' | '), '\n')
