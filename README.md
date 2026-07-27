# Concentrated Disadvantage Index (CDI) Datasets & Replication Package

[![Website](https://img.shields.io/badge/Website-Live_Interactive_Site-brightgreen.svg)](https://idlhy0218.github.io/Concentrated-Disadvantage-Index/)
[![OSF DOI](https://img.shields.io/badge/OSF-10.17605%2FOSF.IO%2FZEJSW-blue.svg)](https://doi.org/10.17605/OSF.IO/ZEJSW)
[![R](https://img.shields.io/badge/R-%E2%89%A5_4.0-blue)](https://www.r-project.org/)
[![Stata](https://img.shields.io/badge/Stata-16%2B-navy)](https://www.stata.com/)

This repository provides ready-to-use, standardized **Concentrated Disadvantage Index (CDI)** datasets for all U.S. census tracts ($N = 84,208$) and counties ($N = 3,142$), along with complete replication materials for the manuscript *"Misappropriating Vulnerability: Assessing the Utility of the Social Vulnerability Index as a Predictor of Firearm Violence"* (Lee & Vogel, 2026).

---

## 🌐 Live Interactive Website & Explorer

Visit the official project website and interactive map explorer:  
👉 **[https://idlhy0218.github.io/Concentrated-Disadvantage-Index/](https://idlhy0218.github.io/Concentrated-Disadvantage-Index/)**

The website features:
- Interactive dataset navigation & documentation
- Direct data download links
- Complete variable codebooks and index definitions
- Replication workflow guides for R and Stata

---

## 📦 Data Downloads (OSF Repository)

The primary nationwide release datasets are archived and registered on the Open Science Framework (OSF):  
👉 **[Download CDI Datasets on OSF (https://osf.io/zejsw/)](https://osf.io/zejsw/)**

| Dataset | File Format | Geography & Coverage | Key Features |
| :--- | :--- | :--- | :--- |
| **Tract-Level CDI Release** | `.csv` / `.RDS` | $N = 84,208$ Census Tracts (Nationwide) | Baseline components, z-scores, V1/V2/V3 PCA scores scaled at National, State, & County levels |
| **County-Level CDI Release** | `.csv` / `.RDS` | $N = 3,142$ Counties & Equivalents (Nationwide) | Aggregated ACS items, z-scores, V1/V2/V3 PCA scores scaled at National & State levels |

---

## 💡 Overview & Dual Scope

This project serves a **twofold purpose**:

1. **Nationwide CDI Dataset Public Release**  
   Provides ready-to-use, standardized CDI measures constructed from U.S. Census Bureau American Community Survey (ACS 2020 5-year estimates) data across multiple geographic scales (National, State, County) and index formulations (V1, V2, V3).
   
2. **Academic Manuscript Replication Package**  
   Provides complete data processing scripts, merged datasets, and Stata modeling code for Lee & Vogel (2026). The study evaluates the conceptual foundations and empirical performance of the CDC Social Vulnerability Index (SVI) relative to the conventional Concentrated Disadvantage Index (CDI) in predicting neighborhood firearm homicides across 84 U.S. cities.

Unlike emergency-management indices such as the CDC SVI—which gauge general community capacity to absorb natural disaster shocks—the CDI specifically captures structural economic hardship, family structure, public assistance reliance, unemployment, education, and household income.

---

## 📊 Index Formulations & Geographic Scaling

### 1. Index Formulations (PCA Extracted)
All CDI composite scores are constructed via single-factor Principal Component Analysis (PCA) with unrotated factor extraction:

- **CDI V1 (4-Item Baseline)**: Traditional criminological baseline (Sampson et al., 1997) combining poverty, female-headed households, public assistance, and civilian unemployment.
- **CDI V2 (5-Item Index)**: Adds low educational attainment (proportion 25+ with high school education or lower).
- **CDI V3 (6-Item Index)**: Adds reverse-coded median household income to V2.

### 2. Geographic Scaling Levels
- **National Level (`disad_national_v1/v2/v3`)**: Standardized across all U.S. census tracts or counties nationwide.
- **State Level (`disad_state_v1/v2/v3`)**: Standardized within each state.
- **County Level (`disad_county_v1/v2/v3`)**: Standardized within each county (*tract dataset only*).

---

## 📂 Directory Structure

```
Concentrated-Disadvantage-Index/
├── index.html                                        # Project website & web documentation (GitHub Pages)
├── osf_project_description.txt                       # Project description and metadata for OSF archive
├── README.md                                         # Main repository documentation
│
├── R Code/                                           # R processing & extraction scripts
│   ├── 1_tract level concentrated disadvantage items_260510.R  # Pulls ACS items for census tracts
│   ├── 2_American Violence data cleaning_260508.R              # Cleans AVP firearm violence data (84 cities)
│   ├── 3_Other Figures_260508.R                                # Correlation matrices, scree plots & spatial maps
│   ├── 4_county level robustness check.R                       # County-level SVI/CDI robustness check
│   ├── 5_tract level cdi for release.R                         # Builds nationwide tract-level release dataset
│   └── 6_county level cdi for release.R                        # Builds nationwide county-level release dataset
│
├── STATA Code/                                       # Statistical modeling scripts
│   └── analysis_260509.do                                      # Runs regression models (OLS, NBREG, Poisson)
│
├── Data/                                             # Raw data, release files & codebooks
│   ├── codebook_cdi_tract_level_release.txt                    # Codebook for tract release file
│   ├── codebook_cdi_county_level_release.txt                   # Codebook for county release file
│   ├── (created) cdi_tract_level_release.csv / .RDS            # Nationwide Tract CDI dataset (N = 84,208)
│   ├── (created) cdi_county_level_release.csv / .RDS           # Nationwide County CDI dataset (N = 3,142)
│   ├── (created) finaldata_260508.csv / .RDS                   # Merged paper dataset (84 cities sample)
│   ├── (created) finaldata county level_260508.csv / .RDS      # Merged paper dataset (County sample)
│   ├── fatal_shootings_2019-2023.RDS                           # AVP firearm homicide incidents
│   ├── SVI_2020_US_tract.csv                                   # CDC SVI raw data (Tracts)
│   └── SVI_2020_US_county.csv                                  # CDC SVI raw data (Counties)
│
└── Figure/                                           # Figures & interactive MapLibre/Leaflet maps
    ├── interactive_map_county_cdi.html                         # Interactive spatial map (County CDI)
    ├── correlation_plot_260508.png                             # Correlation plot (composite indices)
    ├── correlation_plot_subitems_260508.png                    # Correlation plot (sub-items)
    └── CDI_Scree_*.png                                         # PCA Scree plots across benchmarks
```

---

## 📋 Variable Codebook

Key variables included in `cdi_tract_level_release.csv/.RDS` and `cdi_county_level_release.csv/.RDS`:

| Variable Name | Data Type | Description & ACS Source Construction |
| :--- | :--- | :--- |
| `tract_fips` / `county_fips` | Numeric / String | 11-digit Census Tract FIPS or 5-digit County FIPS code |
| `state_fips` | String | 2-digit State FIPS code |
| `disad_national_v1 / v2 / v3` | Numeric | **National CDI**: PCA score standardized across all U.S. tracts/counties (V1: 4-item, V2: 5-item, V3: 6-item) |
| `disad_state_v1 / v2 / v3` | Numeric | **State CDI**: PCA score standardized within each State |
| `disad_county_v1 / v2 / v3` | Numeric | **County CDI**: PCA score standardized within each County (*tract dataset only*) |
| `pr_female_hh` | Numeric | Proportion of female-headed households with children, no spouse present (ACS B11001) |
| `pr_pov` | Numeric | Proportion of population living below 100% poverty threshold (ACS C17002) |
| `pr_pubassi` | Numeric | Proportion of households receiving public assistance income (ACS B19057) |
| `pr_unemprate` | Numeric | Civilian unemployment rate for population age 16+ (ACS B23025) |
| `pr_hs_or_low` | Numeric | Proportion of population age 25+ with high school education or lower (ACS B06009) |
| `median_income` | Numeric | Median household income in inflation-adjusted dollars (ACS B19013) |
| `pr_std_*` | Numeric | Standardized z-scores ($Mean=0, SD=1$) for each component (`pr_std_income` is reverse-coded) |

---

## 🚀 Execution & Usage Guide

### 1. Generating Nationwide CDI Datasets
To extract Census ACS variables directly via `tidycensus` and generate the nationwide release files:

1. **Build Tract Components**:  
   Run [`R Code/1_tract level concentrated disadvantage items_260510.R`](file:///c:/Users/User/OneDrive/Github%20Desktop/Concentrated-Disadvantage-Index/R%20Code/1_tract%20level%20concentrated%20disadvantage%20items_260510.R)  
   *Outputs:* `(created) concentrated disadvantage items.RDS`

2. **Generate Tract Release Files**:  
   Run [`R Code/5_tract level cdi for release.R`](file:///c:/Users/User/OneDrive/Github%20Desktop/Concentrated-Disadvantage-Index/R%20Code/5_tract%20level%20cdi%20for%20release.R)  
   *Outputs:* `(created) cdi_tract_level_release.csv` and `.RDS`

3. **Generate County Release Files**:  
   Run [`R Code/6_county level cdi for release.R`](file:///c:/Users/User/OneDrive/Github%20Desktop/Concentrated-Disadvantage-Index/R%20Code/6_county%20level%20cdi%20for%20release.R)  
   *Outputs:* `(created) cdi_county_level_release.csv` and `.RDS`

---

### 2. Paper Analysis Replication Workflow
To reproduce all tables, figures, and empirical models from the manuscript (Tables 1–3, Appendices 2–4, Figures 1–2):

1. **Extract Tract Components**: Run Script `01` (`1_tract level concentrated disadvantage items_260510.R`).
2. **Clean AVP Homicide & Merged SVI/CDI Data**: Run Script `02` (`2_American Violence data cleaning_260508.R`).  
   *Outputs:* `(created) finaldata_260508.csv / .RDS` (84 cities sample).
3. **Generate Correlation Plots & Maps**: Run Script `03` (`3_Other Figures_260508.R`).
4. **County-Level Robustness Dataset**: Run Script `04` (`4_county level robustness check.R`).  
   *Outputs:* `(created) finaldata county level_260508.csv / .RDS`.
5. **Statistical Modeling (Stata)**: Execute [`STATA Code/analysis_260509.do`](file:///c:/Users/User/OneDrive/Github%20Desktop/Concentrated-Disadvantage-Index/STATA%20Code/analysis_260509.do). Runs OLS, Negative Binomial, Poisson models, standardized incidence rate ratios (IRRs), leverage diagnostics, V1–V3 comparisons, and county-level sensitivity checks.

---

## 🛠️ Software & Environment Requirements

- **R**: Version $\ge 4.0$
  - Packages: `tidyverse`, `tidycensus`, `sf`, `psych`, `corrplot`, `leaflet`, `htmlwidgets`
- **Stata**: Version $16+$

---

## 📝 Recommended Citation

If you use these datasets or code in your research, please cite:

> Lee, H. & Vogel, M. (2026). *Concentrated Disadvantage Index (CDI) Datasets*. Open Science Framework (OSF). https://doi.org/10.17605/OSF.IO/ZEJSW

```bibtex
@misc{lee_vogel_2026_cdi,
  author       = {Lee, Heeyoung and Vogel, Matt},
  title        = {Concentrated Disadvantage Index (CDI) Datasets},
  year         = {2026},
  publisher    = {Open Science Framework (OSF)},
  doi          = {10.17605/OSF.IO/ZEJSW},
  url          = {https://doi.org/10.17605/OSF.IO/ZEJSW}
}
```

---

## 📬 Contact & Links

- **Project Website**: [https://idlhy0218.github.io/Concentrated-Disadvantage-Index/](https://idlhy0218.github.io/Concentrated-Disadvantage-Index/)
- **OSF Dataset Repository**: [https://osf.io/zejsw/](https://osf.io/zejsw/)
- **GitHub Repository**: [https://github.com/idlhy0218/Concentrated-Disadvantage-Index](https://github.com/idlhy0218/Concentrated-Disadvantage-Index)
