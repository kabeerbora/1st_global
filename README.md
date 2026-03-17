# Replication Package
## "Trade Openness and the Rate of Profit during the First Globalization, 1870–1913"
### *Review of Social Economy* (Resubmission, March 2026)

---

## Overview

This package contains all code and data required to replicate every table and figure in the manuscript. Running the two scripts in order reproduces the full analysis from raw sources to regression output.

## Requirements

- Python 3.9+
- Packages: `pandas`, `numpy`, `openpyxl`, `xlrd`, `linearmodels`, `statsmodels`, `pyfixest`, `scipy`

Install with:
```
pip install pandas numpy openpyxl xlrd linearmodels statsmodels pyfixest scipy
```

## Directory Structure

```
Replication/
├── README.md                      ← This file
├── 01_build_data.py               ← Step 1: construct net rate of profit dataset
├── 02_run_regressions.py          ← Step 2: run all regressions (TWFE, IV, LOO)
├── Data/
│   ├── Common/
│   │   ├── 1st_global.xlsx        ← Main assembled panel (all 7 countries, 1870–1913)
│   │   ├── dyadic_trade_bilateral_pop.csv  ← Bilateral trade flows for IV gravity model
│   │   └── population_data/
│   │       └── 1st_global.csv     ← GDP/population data (Maddison 2023)
│   ├── Germany/
│   │   ├── Germany.xls            ← Piketty-Zucman (2014) German national accounts
│   │   └── SOURCE.md
│   ├── UK/
│   │   ├── a-millennium-of-macroeconomic-data-for-the-uk.xlsx  ← Bank of England (2023)
│   │   └── SOURCE.md
│   ├── Sweden/
│   │   ├── tablesAtoX.xls         ← Edvinsson (2016) Swedish capital stock
│   │   └── SOURCE.md
│   ├── France/
│   │   ├── France.xls             ← Piketty-Zucman (2014) French national accounts
│   │   └── SOURCE.md
│   ├── Netherlands/
│   │   └── SOURCE.md              ← Data hardcoded in 01_build_data.py
│   ├── Spain/
│   │   └── SOURCE.md              ← Data hardcoded in 01_build_data.py
│   └── USA/
│       └── SOURCE.md              ← Duménil & Lévy (2013), via 1st_global.xlsx
```

## How to Replicate

Run from the `Replication/` directory:

```bash
python 01_build_data.py
python 02_run_regressions.py
```

**Step 1** reads raw country-level capital stock data, applies net productive capital corrections for each country, and writes `Data/Common/1st_global_net_rop.xlsx`.

**Step 2** reads that file, constructs the gravity instrument via PPML, runs all TWFE and IV regressions, and prints results corresponding to Tables 2–5 in the manuscript.

## Correspondence Between Outputs and Manuscript Tables

| Script Output                          | Manuscript Location           |
|----------------------------------------|-------------------------------|
| Summary stats printed at end of Step 2 | Table 3 (Summary Statistics)  |
| TWFE Model M4 (full, with lag)         | Table 2, Columns (1)–(2)      |
| TWFE no-controls (M4 no controls)      | Table 2, Column (0)           |
| Alt. trade measures (exp_gdp, imp_gdp) | Table 2 (alt. measures)       |
| IV uncertainty spec (bare + full)      | Table 4 (IV Results)          |
| LOO results                            | Appendix robustness           |
| First-stage F-statistics               | Table 4 notes                 |

## Data Sources Summary

See individual `SOURCE.md` files in each country subfolder. Common sources:

- **Bilateral trade & gravity variables**: CEPII (2016), `dyadic_trade_bilateral_pop.csv`
- **GDP (Maddison)**: Maddison Project Database 2023, `population_data/1st_global.csv`
- **Temperature uncertainty**: Berkeley Earth (berkeleyearth.org/data/), merged into `dyadic_trade_bilateral_pop.csv`
- **Trade openness, tariffs, ToT, NFA**: Jordà-Schularick-Taylor Macrohistory Database; Clemens & Williamson (2004); Piketty & Zucman (2014)
