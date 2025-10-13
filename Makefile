SHELL := cmd
.SHELLFLAGS := /C

RSC := Rscript
PY  := .venv\Scripts\python.exe

.PHONY: run r-run plot-py restore

run: r-run plot-py

r-run:
	$(RSC) scripts\01_download.R
	$(RSC) scripts\02_process.R
	$(RSC) scripts\03_model.R

plot-py:
	"$(PY)" src\python\plot_connectedness.py

restore:
	py -3.12 -m venv .venv
	"$(PY)" -m pip install -r requirements-lock.txt
	$(RSC) -e "if(!requireNamespace('renv', quietly=TRUE)) install.packages('renv', repos='https://cloud.r-project.org'); renv::restore()"
