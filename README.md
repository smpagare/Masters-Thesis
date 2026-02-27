# Telework Expansion and the Abstract-Task Wage Premium

Siddhant Pagare, King's College London

## Research Question

Did the post-2020 telework expansion amplify or compress the wage premium for abstract-task intensive occupations?

## Directory Structure

```
├── code/
│   ├── 00_master.R          # Setup: packages, paths, pipeline runner
│   ├── 01_clean_data.R      # Build analytical sample from raw data
│   ├── 02_analysis.R        # DID, event study, heterogeneity, robustness
│   ├── 03_figures.R         # Publication-quality figures
│   ├── 04_additional_tables.R # Summary statistics tables
│   ├── 05_reformat_tables.R # Regenerate tables as standard tabular LaTeX
│   ├── 06_strip_titles.R    # Remove ggplot titles (LaTeX captions used)
│   ├── 07_paper_stats.R     # Generate paper_stats.tex + sessionInfo
│   └── 08_occupation_dotplot.R # Presentation figure: occupation sample composition
├── data/
│   ├── raw/                 # Raw data sources (not included in repo)
│   └── cleaned/             # Processed analytical samples (.rds)
├── output/
│   ├── tables/              # LaTeX tables and saved model objects
│   └── figures/             # PDF/PNG figures
├── paper/
│   ├── manuscript.tex
│   ├── references.bib
│   ├── tables/              # Self-contained copies for Overleaf
│   └── figures/             # Self-contained copies for Overleaf
├── presentation/
│   ├── presentation.tex     # Beamer slides (Economist theme, 16:9)
│   └── figures/             # Presentation figure copies
└── README.md
```

## Instructions

1. Install the `here` R package if not already available.
2. Place raw data files in the appropriate `data/raw/` subdirectories (see Data Availability below).
3. Run `RUN_PIPELINE=1 Rscript code/00_master.R` to execute the full pipeline (scripts 01-07). Alternatively, run individual scripts in order; each sources `00_master.R` for paths and packages. Script 08 generates a presentation-only figure and runs independently.
4. Output is saved to `output/tables/` and `output/figures/`.

## Data Availability

- **CPS-ORG**: Current Population Survey Outgoing Rotation Groups, 2017-2023. Accessed via IPUMS CPS, University of Minnesota. Available at https://cps.ipums.org/. Citation: Flood, Sarah, Miriam King, Renae Rodgers, Steven Ruggles, J. Robert Warren, Daniel Backman, Annie Chen, Grace Cooper, Stephanie Richards, Megan Schouweiler, and Michael Westberry. IPUMS CPS: Version 12.0 [dataset]. Minneapolis, MN: IPUMS, 2024. https://doi.org/10.18128/D030.V12.0.
- **O*NET**: O*NET Database version 29.1, U.S. Department of Labor/Employment and Training Administration. Available at https://www.onetcenter.org/database.html.
- **Dingel-Neiman Telework Classification**: Dingel, Jonathan I., and Brent Neiman. 2020. "How Many Jobs Can Be Done at Home?" Journal of Public Economics 189: 104235. Replication data available at https://github.com/jdingel/DingelNeiman-workathome.
- **Regional Price Parities (RPP)**: Bureau of Economic Analysis, SARPP table, state-level RPPs. Available at https://www.bea.gov/data/prices-inflation/regional-price-parities-state-and-metro-area.
- **CPI-U-RS**: Bureau of Labor Statistics research series. Available at https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm.
- **SOC Crosswalks**: Census OCC to SOC-2018 crosswalks. U.S. Census Bureau and Bureau of Labor Statistics.

The CPS extract (~2.3 GB) is too large for the repository. To reproduce, request IPUMS CPS extract #00031 with the ORG supplement variables for 2017-2023 from https://cps.ipums.org/. All other data sources are publicly available at the URLs listed above.

## Computational Requirements

- **Software**: R 4.3+ with packages listed in `code/00_master.R`
- **Runtime**: Approximately 15-20 minutes on a standard desktop (M-series Mac or equivalent). The most time-consuming step is `01_clean_data.R` (raw CPS processing, ~8 minutes).
- **Memory**: 8 GB RAM recommended for processing the full CPS extract.
