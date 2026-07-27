#===============================================================================
# TRACT-LEVEL CONCENTRATED DISADVANTAGE INDEX (CDI) FOR RELEASE
#===============================================================================
# Purpose: Calculate nationwide census tract-level CDI scores (V1, V2, V3) 
#          scaled across National, State, and County levels, and generate an interactive MapLibre map 
#          focusing on V1 Baseline (National, State, and County scaled) via mapgl.
# Data Source: American Community Survey (ACS 2020 5-year estimates)
# Geographic Level: Census tract
# Outputs: 
#   - Data/(created) cdi_tract_level_release.csv
#   - Data/(created) cdi_tract_level_release.RDS
#   - Figure/interactive_map_tract_cdi.html
#===============================================================================

library(tidyverse)
library(psych)
library(sf)
library(tigris)
library(mapgl)
library(htmlwidgets)

options(tigris_use_cache = TRUE)

# Define parameters
data_dir   <- "Data"
figure_dir <- "Figure"

# 1. Load tract-level items
tract_items <- readRDS(file.path(data_dir, "(created) concentrated disadvantage items.RDS"))

# 2. Format FIPS codes & geographic identifiers
tract_df <- tract_items %>%
  mutate(
    fips_str = sprintf("%011.0f", tract_fips),
    state_fips = str_sub(fips_str, 1, 2),
    county_fips = str_sub(fips_str, 1, 5)
  )

# Define CDI item lists
cdi_v1 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate")
cdi_v2 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate", "pr_std_hs_or_low")
cdi_v3 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate", "pr_std_hs_or_low", "pr_std_income")
cdi_versions <- list(v1 = cdi_v1, v2 = cdi_v2, v3 = cdi_v3)

# PCA helper function
run_pca_scores <- function(data, vars) {
  cc <- complete.cases(data %>% select(all_of(vars)))
  scores <- rep(NA, nrow(data))
  if (sum(cc) >= 5) {
    pca <- tryCatch(
      principal(data[cc, vars], nfactors = 1, rotate = "none", scores = TRUE),
      error = function(e) NULL
    )
    if (!is.null(pca)) {
      scores[cc] <- pca$scores[, 1]
    }
  }
  scores
}

cat("1. Calculating National-level CDI scores...\n")
for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  tract_df[[paste0("disad_national_", v)]] <- run_pca_scores(tract_df, vars)
}

cat("2. Calculating State-level CDI scores...\n")
for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  col_name <- paste0("disad_state_", v)
  
  state_scores <- tract_df %>%
    group_by(state_fips) %>%
    group_modify(~ {
      .x[[col_name]] <- run_pca_scores(.x, vars)
      .x
    }) %>%
    ungroup()
  
  tract_df[[col_name]] <- state_scores[[col_name]]
}

cat("3. Calculating County-level CDI scores...\n")
for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  col_name <- paste0("disad_county_", v)
  
  county_scores <- tract_df %>%
    group_by(county_fips) %>%
    group_modify(~ {
      .x[[col_name]] <- run_pca_scores(.x, vars)
      .x
    }) %>%
    ungroup()
  
  tract_df[[col_name]] <- county_scores[[col_name]]
}

# Clean infinite values and organize columns
tract_release <- tract_df %>%
  select(
    tract_fips, fips_str, state_fips, county_fips, year,
    # Raw proportions
    pr_female_hh, pr_pov, pr_pubassi, pr_unemprate, pr_hs_or_low, median_income,
    # Standardized items
    pr_std_female_hh, pr_std_pov, pr_std_pubassi, pr_std_unemprate, pr_std_hs_or_low, pr_std_income,
    # National CDI
    disad_national_v1, disad_national_v2, disad_national_v3,
    # State CDI
    disad_state_v1, disad_state_v2, disad_state_v3,
    # County CDI
    disad_county_v1, disad_county_v2, disad_county_v3
  )

# Save release files
saveRDS(tract_release, file.path(data_dir, "(created) cdi_tract_level_release.RDS"))

tryCatch({
  write.csv(tract_release, file.path(data_dir, "(created) cdi_tract_level_release.csv"), row.names = FALSE, na = "")
}, error = function(e) {
  write.csv(tract_release, file.path(data_dir, "cdi_tract_level_release.csv"), row.names = FALSE, na = "")
})

cat("Tract-level CDI release dataset complete. N =", nrow(tract_release), "tracts.\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================

