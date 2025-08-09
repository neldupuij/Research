pkgs <- readLines("r-requirements.txt")
pkgs <- pkgs[!grepl("^\\s*#", pkgs)]            # enlever commentaires
pkgs <- trimws(pkgs)
pkgs <- pkgs[nchar(pkgs) > 0]

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  message("✅ Tous les packages R sont déjà installés.")
}
