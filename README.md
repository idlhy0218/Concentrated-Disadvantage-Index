# Concentrated Disadvantage Index (CDI) Datasets & Replication Code

[![OSF DOI](https://img.shields.io/badge/OSF-10.17605%2FOSF.IO%2FZEJSW-blue.svg)](https://doi.org/10.17605/OSF.IO/ZEJSW)

This repository provides standardized Concentrated Disadvantage Index (CDI) datasets and paper replication materials.

## 📦 Data Downloads (OSF Repository)
The primary release datasets are registered and archived on OSF:
👉 **[Download CDI Datasets on OSF (https://osf.io/zejsw/)](https://osf.io/zejsw/)**

- **Tract-Level Release Dataset**: `(created) cdi_tract_level_release.csv / .RDS` ($N = 84,208$ census tracts, nationwide coverage).
- **County-Level Release Dataset**: `(created) cdi_county_level_release.csv / .RDS` ($N = 3,142$ counties, nationwide coverage).

---

## 📝 Recommended Citation
If you use these datasets in your research, please cite:

> Lee, H. & Vogel, M. (2026). *Concentrated Disadvantage Index (CDI) Datasets*. Open Science Framework (OSF). https://doi.org/10.17605/OSF.IO/ZEJSW

---

## Directory Structure

```
├── R Code/
│   ├── 1_tract level concentrated disadvantage items_260510.R   # Build CDI component items (tract level)
│   ├── 2_American Violence data cleaning_260508.R               # Clean & merge AVP violence data (84 cities)
│   ├── 3_Other Figures_260508.R                                 # Generate paper correlation plots & map
│   ├── 4_county level robustness check.R                        # County-level robustness check for paper
│   ├── 5_tract level cdi for release.R                          # Build nationwide tract CDI release dataset
│   └── 6_county level cdi for release.R                         # Build nationwide county CDI release dataset
│
├── STATA Code/
│   └── analysis_260509.do                                       # Primary statistical modeling (OLS, NBREG, Poisson)
│
├── Data/
│   ├── codebook_cdi_tract_level_release.txt                     # Plain text codebook for tract dataset
│   ├── codebook_cdi_county_level_release.txt                    # Plain text codebook for county dataset
│   ├── (created) cdi_tract_level_release.csv / .RDS             # Nationwide Tract CDI release (V1, V2, V3)
│   ├── (created) cdi_county_level_release.csv / .RDS            # Nationwide County CDI release (V1, V2, V3)
│   ├── fatal_shootings_2019-2023.RDS                            # Raw: AVP firearm homicide incidents
│   ├── SVI_2020_US_tract.csv                                    # Raw: CDC SVI — tract level
│   └── SVI_2020_US_county.csv                                   # Raw: CDC SVI — county level
│
└── Figure/                                                      # Interactive MapLibre GL map & paper figures
```

---

## Usage & Execution Guide

### 1. Nationwide CDI Dataset Creation
If your primary goal is to construct or replicate the nationwide Concentrated Disadvantage Index (CDI) datasets directly from Census ACS data:

- `1_tract level concentrated disadvantage items_260510.R` — pulls ACS Census variables and builds tract component items.
- `5_tract level cdi for release.R` — constructs the nationwide tract-level CDI release dataset (`cdi_tract_level_release.csv/.RDS`).
- `6_county level cdi for release.R` — extracts county-level ACS variables directly and constructs the nationwide county-level CDI release dataset (`cdi_county_level_release.csv/.RDS`).

### 2. Full Paper Replication
To reproduce all regression models, summary statistics, and paper figures (Tables 1–3, Appendices 2–4, Figures 1–2) from the manuscript:

1. `1_tract level concentrated disadvantage items_260510.R` — generates `(created) concentrated disadvantage items.RDS`
2. `2_American Violence data cleaning_260508.R` — generates `(created) finaldata_260508.RDS/.csv` (84 cities sample)
3. `3_Other Figures_260508.R` — generates figures in `Figure/`
4. `4_county level robustness check.R` — county-level robustness dataset
5. `analysis_260509.do` (Stata) — runs all primary regression models and diagnostic tests
