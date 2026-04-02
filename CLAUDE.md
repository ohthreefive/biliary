# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Biliary drain and stent project. A dataset of ~500 biliary procedures performed over the last decade, analysed in R with Quarto output. Goals:

1. Descriptive statistics (procedures per year/diagnosis/hospital, patient demographics)
2. Traditional statistical analysis including survival analysis (`survminer`, `ggsurvplot`, Cox proportional hazards) and multiple linear regression
3. Machine learning prediction tools, ideally deployed as Shiny apps (e.g. survival in days, 30-day mortality)

Secondary goal: abstract/paper creation, so generate publication-ready outputs (e.g. Table 1) throughout.

Work through steps one at a time in manageable chunks.

## Directory Structure

```
biliary/
├── biliary.qmd          # Main Quarto document (HTML output first, docx later)
├── data/
│   └── biliary.xlsx     # Single data file
├── sources/
│   ├── libraries.R      # All library() calls
│   ├── dataWrangle.R    # Data cleaning and transformation
│   ├── descriptiveStats.R
│   ├── survival.R
│   ├── regression.R
│   └── machineLearning.R
└── output/              # HTML output, images, etc.
```

## R Style

- Prefer `tidyverse` and `ggplot2` over base R; use `gt` for tables
- Comment generously, especially on long chained `dplyr` pipelines and any functions
- Round most decimals to 0 decimal places (dataset < 1000 entries)
- Write for reproducibility — data will be updated annually and scripts re-run

## Data Dictionary (`data/biliary.xlsx`)

Columns to **remove**: `CHI`, `DOB`, `Code`, `dateDeath`, `daysDeath`

Check `dupCHI` contains only `1` throughout; if so, remove it. If duplicates exist, flag for manual resolution.

**Derived columns to create:**
- `Year` from `eventDate`
- `Decade` from `ageProcedure` (e.g. '20s', '30s', '40s')
- `Site` → abbreviated hospital code: `GRI` / `QEUH`
- `Hospital` → full name: `Glasgow Royal Infirmary` / `Queen Elizabeth University Hospital` (from `G107H` / `QEUH`)
- Rename `Diag` → `Diagnosis`

**Key variables (not outcomes):** `ageProcedure`, `Sex`, `Site`/`Hospital`, `Hb` (g/L), `WCC` (×10⁹/L), `Plts` (×10⁹/L), `PT` (seconds), `Urea` (mmol/L), `Cr` (µmol/L), `Bili` (µmol/L), `Alb` (g/L), `CRP` (mg/L), `Weight` (kg), `Diagnosis` (benign vs malignant), `ERCP` (binary)

**Key outcomes:** `Alive` (Yes/No), `Survival` (days — `daysSince` if alive, `daysDeath` if dead), `Inpatient mortality` (binary; only meaningful when `Alive == "No"`)

`daysSince` = days since procedure — use to filter out patients with insufficient follow-up (e.g. < 42 days) before calculating mortality rates.

## Analysis Plans

### descriptiveStats.R
- **Table 1**: three versions split by hospital (GRI vs QEUH), sex, and diagnosis (benign vs malignant), each with a total column. Continuous variables likely non-normal → use median (IQR). Test distribution first.
- **Procedures per year**: column chart (total), then `position_dodge` by hospital
- In-text: average procedures per month overall and per hospital
- **Age histogram**: binwidth = 10 years (decade buckets on x-axis)

### survival.R
- Survival analysis using `survminer` / `ggsurvplot`
- Cox proportional hazards modelling

### regression.R
- Multiple linear regression to predict outcomes (e.g. death at 30/42/60 days)
- User is new to MLR — provide explanation and guidance as we go

### machineLearning.R
- Prediction models for continuous outcomes (survival days) and binary outcomes (30-day mortality)
- Shiny app(s) for interactive prediction from user-input variables

## Git

This is a git repository. Suggest commits at logical milestones with a recommended commit message, but **do not commit — the user commits manually**.
