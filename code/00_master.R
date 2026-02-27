# 00_master.R - Setup: packages, paths, pipeline runner
# Siddhant Pagare, King's College London

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
BASE_DIR <- here::here()

# Paths
CODE_DIR   <- file.path(BASE_DIR, "code")
DATA_RAW   <- file.path(BASE_DIR, "data", "raw")
DATA_CLEAN <- file.path(BASE_DIR, "data", "cleaned")
OUTPUT_DIR <- file.path(BASE_DIR, "output")
FIG_DIR    <- file.path(OUTPUT_DIR, "figures")
TAB_DIR    <- file.path(OUTPUT_DIR, "tables")

CPS_DIR  <- file.path(DATA_RAW, "cps")
ONET_DIR <- file.path(DATA_RAW, "onet", "db_29_1_text")
DN_DIR   <- file.path(DATA_RAW, "dingel_neiman")
CW_DIR   <- file.path(DATA_RAW, "crosswalks")
RPP_DIR  <- file.path(DATA_RAW, "rpp")
dir.create(DATA_CLEAN, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR,    showWarnings = FALSE, recursive = TRUE)
dir.create(TAB_DIR,    showWarnings = FALSE, recursive = TRUE)

# Packages
required_packages <- c(
  "data.table", "tidyverse", "haven", "readxl", "ipumsr",
  "fixest", "sandwich", "lmtest", "broom",
  "ggplot2", "patchwork", "scales", "viridis", "kableExtra",
  "modelsummary", "stargazer", "HonestDiD"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
}
invisible(sapply(required_packages, install_if_missing))
invisible(sapply(required_packages, library, character.only = TRUE))

options(scipen = 999)
theme_set(
  theme_minimal(base_size = 13, base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

# CPI-U-RS deflator (base: 2019 = 100)
# 2024-2025 values use CPI-U annual averages as proxy (CPI-U-RS not yet published)
cpi_u_rs <- data.table(
  YEAR = 2017:2025,
  cpi  = c(93.008, 95.350, 97.309, 98.520, 102.777,
           110.670, 114.288, 117.070, 119.500)
)
cpi_u_rs[, cpi_2019 := cpi / cpi_u_rs[YEAR == 2019, cpi] * 100]


# Run pipeline
if (!exists("SOURCED_BY_CHILD") || !SOURCED_BY_CHILD) {
  if (interactive() || identical(Sys.getenv("RUN_PIPELINE"), "1")) {
    source(file.path(CODE_DIR, "01_clean_data.R"))
    source(file.path(CODE_DIR, "02_analysis.R"))
    source(file.path(CODE_DIR, "03_figures.R"))
    source(file.path(CODE_DIR, "04_additional_tables.R"))
    source(file.path(CODE_DIR, "05_reformat_tables.R"))
    source(file.path(CODE_DIR, "06_strip_titles.R"))
    source(file.path(CODE_DIR, "07_paper_stats.R"))
  }
}
