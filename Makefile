# Detect binaries
RSC := $(shell command -v Rscript 2>/dev/null || echo Rscript)
PY  := $(shell command -v python3 2>/dev/null || echo python3)

.PHONY: bootstrap run r-run plot-py clean lock restore doctor py-restore r-restore

## ---------------------------------------------------------------------------
## Setup
## ---------------------------------------------------------------------------

bootstrap:  ## create venv + install Python/R deps (unlocked)
	$(PY) -m venv .venv
	. .venv/bin/activate && pip install -r requirements.txt
	$(RSC) scripts/install_r_requirements.R

## ---------------------------------------------------------------------------
## Pipeline
## ---------------------------------------------------------------------------

run: r-run plot-py  ## full pipeline: R exports CSVs, Python makes figures

r-run:  ## R steps only (no plotting)
	$(RSC) scripts/01_download.R
	$(RSC) scripts/02_process.R
	$(RSC) scripts/03_model.R

plot-py:  ## build all figures with Python from reports/exports/*.csv
	$(PY) -m src.python.plot_connectedness

## ---------------------------------------------------------------------------
## Housekeeping
## ---------------------------------------------------------------------------

clean: ## remove all derived data/figures
	rm -f data/processed/prices_raw.csv data/processed/returns_panel.csv
	rm -f reports/exports/*.csv reports/exports/*.rds
	rm -f figures/*.png figures/*.pdf

lock: ## update Python & R lock files
	. .venv/bin/activate && pip freeze > requirements-lock.txt
	$(RSC) -e 'renv::snapshot()'

restore: py-restore r-restore  ## restore from lock files

py-restore:
	$(PY) -m venv .venv
	. .venv/bin/activate && pip install -r requirements-lock.txt

r-restore:
	$(RSC) -e 'if (!requireNamespace("renv", quietly=TRUE)) install.packages("renv", repos="https://cloud.r-project.org"); renv::consent(provided=TRUE); renv::restore()'

doctor:
	$(RSC) scripts/00_doctor.R
nn:
	".venv\Scripts\python.exe" src\python\nn\prepare_graph_series.py
	".venv\Scripts\python.exe" src\python\nn\pagerank_centralities.py
	".venv\Scripts\python.exe" src\python\nn\train_tci_nn.py
	".venv\Scripts\python.exe" src\python\nn\render_nn.py

nn_clean:
	if exist figures_nn rmdir /S /Q figures_nn
	if exist reports\exports_nn rmdir /S /Q reports\exports_nn
	if exist logs\nn rmdir /S /Q logs\nn
	if exist data\processed_nn rmdir /S /Q data\processed_nn
	mkdir figures_nn
	mkdir reports\exports_nn
	mkdir logs\nn
	mkdir data\processed_nn

nn:
	".venv\Scripts\python.exe" src\python\nn\prepare_graph_series.py
	".venv\Scripts\python.exe" src\python\nn\pagerank_centralities.py
	".venv\Scripts\python.exe" src\python\nn\train_tci_nn.py
	".venv\Scripts\python.exe" src\python\nn\render_nn.py

nn_clean:
	if exist figures_nn rmdir /S /Q figures_nn
	if exist reports\exports_nn rmdir /S /Q reports\exports_nn
	if exist logs\nn rmdir /S /Q logs\nn
	if exist data\processed_nn rmdir /S /Q data\processed_nn
	mkdir figures_nn
	mkdir reports\exports_nn
	mkdir logs\nn
	mkdir data\processed_nn
