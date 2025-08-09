RSC := $(shell command -v Rscript 2>/dev/null || echo Rscript)

.PHONY: bootstrap run clean lock restore doctor py-restore r-restore

bootstrap:  ## installe deps à partir des fichiers non figés
	python3 -m venv .venv
	. .venv/bin/activate && pip install -r requirements.txt
	$(RSC) scripts/install_r_requirements.R

run:  ## exécute la pipeline complète
	$(RSC) scripts/01_download.R
	$(RSC) scripts/02_process.R
	$(RSC) scripts/03_model.R
	$(RSC) scripts/04_render.R

clean: ## supprime outputs
	rm -f data/processed/prices_raw.csv data/processed/returns_panel.csv
	rm -f reports/exports/total_connectedness.csv reports/exports/net_spillovers.csv reports/exports/spillover_table.csv reports/exports/spillover_matrix_kxk.csv reports/exports/spillover_static_result.rds
	rm -f figures/total_connectedness.png figures/net_spillovers.png

lock: ## met à jour requirements-lock.txt et renv.lock
	. .venv/bin/activate && pip freeze > requirements-lock.txt
	$(RSC) -e 'renv::snapshot()'

restore: py-restore r-restore  ## restaure à partir des fichiers figés

py-restore:
	python3 -m venv .venv
	. .venv/bin/activate && pip install -r requirements-lock.txt

r-restore:
	$(RSC) -e 'if (!requireNamespace("renv", quietly=TRUE)) install.packages("renv", repos="https://cloud.r-project.org"); renv::consent(provided=TRUE); renv::restore()'

doctor:
	$(RSC) scripts/00_doctor.R
