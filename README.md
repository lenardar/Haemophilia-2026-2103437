# Haemophilia-2026-2103437

Analysis code for **Machine Learning-Based Classification of Bleeding Risk in Adult Hemophilia Patients: A Multi-regional Cross-Sectional Study**.

## Setup

Use R 4.5.2. Open the repository root in RStudio and restore the package environment:

```r
renv::restore()
```

The source data and supporting variable dictionary required to run the analysis are not included.

## Workflow

Run the scripts in `rscript/` from the repository root:

| Script | Purpose |
|---|---|
| `01_recode.R` | Data preparation and coding |
| `02_eda.R` | Descriptive analysis |
| `03_modeling.R` | Model training and internal validation |
| `04_evaluation.R` | Performance evaluation and sensitivity analyses |
| `05_shap.R` | SHAP explanations |
| `06_tables.R` | Manuscript and supplementary tables |

Select training scenarios at the top of `03_modeling.R`. Run `original_full` and `smotenc_full` together, then run `original_restricted` separately. Set the corresponding run IDs in `report_config.R` before running scripts 04–06.

Training settings are defined in `analysis_config.R`. Generated results are saved under `output/`.
