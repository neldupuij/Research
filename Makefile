SHELL := cmd.exe
.SHELLFLAGS := /C
PY := ".venv\Scripts\python.exe" -u
POWERSHELL := powershell -NoLogo -NoProfile -ExecutionPolicy Bypass -Command

CFG := config\project.yml

.PHONY: all download process var var_graph garch garch_graph compare_graph clean clean_preview soft_clean hard_clean

PRICES  := data/processed/prices_raw.csv
RETURNS := data/processed/returns_panel.csv

VAR_STAMP := outputs/var/.stamp
VAR_TCI   := outputs/var/tci_rolling.csv
VAR_NET   := outputs/var/net_spillovers_rolling.csv
VAR_CORR  := outputs/var/corr_full.csv

DCC_STAMP := outputs/garch/.stamp
DCC_MEAN  := outputs/garch/dcc_meancorr_rolling.csv
DCC_EIG   := outputs/garch/dcc_eigen_centrality.csv

FIG_TCI        := figures/var/rolling_tci.png
FIG_TCI_MEAN   := figures/compare/rolling_tci_vs_meancorr.png
FIG_RHO_EIGEN  := figures/compare/rolling_rho_varnet_vs_dcc.png

all: download process var garch compare var_graph garch_graph compare_graph

$(PRICES): src/R/step_download.R src/R/utils_config.R scripts/01_download.R $(CFG)
	"Rscript" -e "source('renv/activate.R'); source('scripts/01_download.R')"

download: $(PRICES)

$(RETURNS): src/R/step_process.R $(CFG) $(PRICES)
	"Rscript" -e "source('renv/activate.R'); cfg <- yaml::read_yaml('config/project.yml'); source('src/R/step_process.R'); step_process(cfg)"

process: $(RETURNS)

$(VAR_STAMP): scripts/03b_var_rolling.R $(CFG) $(RETURNS)
	"Rscript" -e "source('renv/activate.R'); source('scripts/03b_var_rolling.R')"
	@if not exist "outputs\var" mkdir "outputs\var"
	@echo done > "$(VAR_STAMP)"

$(VAR_TCI): $(VAR_STAMP)
$(VAR_NET): $(VAR_STAMP)

$(VAR_CORR): src/python/var_make_corr_mats.py $(RETURNS)
	$(PY) src/python/var_make_corr_mats.py outputs\var\corr_full.csv

var: $(VAR_TCI) $(VAR_NET) $(VAR_CORR)

var_graph: src/python/var_graphs.py $(VAR_TCI)
	$(PY) src/python/var_graphs.py figures\var\rolling_tci.png

$(DCC_STAMP): scripts/04_dcc_rolling.R $(CFG) $(RETURNS)
	"Rscript" -e "source('renv/activate.R'); source('scripts/04_dcc_rolling.R')"
	@if not exist "outputs\garch" mkdir "outputs\garch"
	@echo done > "$(DCC_STAMP)"

$(DCC_MEAN): $(DCC_STAMP)
$(DCC_EIG):  $(DCC_STAMP)

garch: $(DCC_MEAN) $(DCC_EIG)

garch_graph: src/python/garch_graphs.py $(DCC_MEAN)
	$(PY) src/python/garch_graphs.py figures\compare\garch_dummy.png

$(FIG_TCI_MEAN) $(FIG_RHO_EIGEN): src/python/compare_rolling_var_garch.py $(VAR_TCI) $(VAR_NET) $(DCC_MEAN)
	$(PY) src/python/compare_rolling_var_garch.py $(FIG_TCI_MEAN) $(FIG_RHO_EIGEN)

compare: $(FIG_TCI_MEAN) $(FIG_RHO_EIGEN)

compare_graph: compare
	@echo Figures :
	@echo  - $(FIG_TCI_MEAN)
	@echo  - $(FIG_RHO_EIGEN)

clean_preview:
	@$(POWERSHELL) "$$p=@('outputs\\var\\*.csv','outputs\\var\\.stamp','outputs\\garch\\*.csv','outputs\\garch\\.stamp','figures\\var\\*.png','figures\\compare\\*.png'); foreach($$x in $$p){ Get-ChildItem $$x -EA SilentlyContinue | %%{ Write-Host ($$_.FullName) } }"

soft_clean clean:
	@$(POWERSHELL) "$$dest='archive\\clean_'+(Get-Date -Format 'yyyyMMdd_HHmmss'); New-Item -ItemType Directory $$dest | Out-Null; $$p=@('outputs\\var\\*.csv','outputs\\var\\.stamp','outputs\\garch\\*.csv','outputs\\garch\\.stamp','figures\\var\\*.png','figures\\compare\\*.png'); foreach($$x in $$p){ Get-ChildItem $$x -EA SilentlyContinue | Move-Item -Destination $$dest -Force -EA SilentlyContinue }; Write-Host '[OK] Moved to ' $$dest"

hard_clean:
	@$(POWERSHELL) "$$p=@('outputs\\var\\*.csv','outputs\\var\\.stamp','outputs\\garch\\*.csv','outputs\\garch\\.stamp','figures\\var\\*.png','figures\\compare\\*.png'); foreach($$x in $$p){ Get-ChildItem $$x -EA SilentlyContinue | Remove-Item -Force -EA SilentlyContinue }; Write-Host '[OK] Deleted.'"
