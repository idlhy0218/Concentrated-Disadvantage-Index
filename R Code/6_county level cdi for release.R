#===============================================================================
# CONCENTRATED DISADVANTAGE INDEX (COUNTY LEVEL) FOR RELEASE
#===============================================================================
# Purpose: Extract county-level ACS components directly via tidycensus (like Script 1),
#          calculate nationwide county-level CDI scores (V1, V2, V3) 
#          scaled across National and State levels, and generate an interactive MapLibre map 
#          focusing on V1 Baseline (National and State scaled) via mapgl.
# Data Source: American Community Survey via tidycensus (ACS 2020 5-year estimates)
# Geographic Level: County
# Outputs: 
#   - Data/(created) cdi_county_level_release.csv
#   - Data/(created) cdi_county_level_release.RDS
#   - Figure/interactive_map_county_cdi.html
#===============================================================================

library(tidycensus)
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
years      <- 2020
names(years) <- years
state      <- state.abb  # All 50 US states

cat("1. Extracting county-level ACS indicators via tidycensus...\n")

#-------------------------------------------------------------------------------
# 1. COMPONENT 1: EDUCATION (HIGH SCHOOL OR LOWER)
#-------------------------------------------------------------------------------
di_educ <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    state = state,
    variables = c(
      po_tot_25plus = "B06009_001",
      po_educ_lh    = "B06009_002",
      po_educ_hc    = "B06009_003"
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

di_educ <- di_educ %>%
  group_by(GEOID, year) %>%
  reframe(
    po_tot_25plus = sum(estimate[variable == "po_tot_25plus"], na.rm = TRUE),
    hs_or_lower   = sum(estimate[variable %in% c("po_educ_lh", "po_educ_hc")], na.rm = TRUE),
    pr_hs_or_low  = hs_or_lower / po_tot_25plus
  ) %>%
  ungroup() %>%
  rename(county_fips = GEOID) %>%
  select(county_fips, year, pr_hs_or_low)

#-------------------------------------------------------------------------------
# 2. COMPONENT 2: FEMALE-HEADED HOUSEHOLDS
#-------------------------------------------------------------------------------
di_fehh <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    state = state,
    variables = c(
      total_hh  = "B11001_001",
      female_hh = "B11001_006"           
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

di_fehh <- di_fehh %>%
  pivot_wider(
    id_cols = c(GEOID, year),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(pr_female_hh = female_hh / total_hh) %>%
  rename(county_fips = GEOID) %>%
  select(county_fips, year, pr_female_hh)

#-------------------------------------------------------------------------------
# 3. COMPONENT 3: POVERTY
#-------------------------------------------------------------------------------
di_pov <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    state = state,
    variables = c(
      n1  = "C17002_002",  # Under 0.50
      n2  = "C17002_003",  # 0.50 to 0.99
      pop = "C17002_001"   # Total population
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

di_pov <- di_pov %>%
  group_by(GEOID, year) %>%
  reframe(
    pr_pov = sum(estimate[variable %in% c("n1", "n2")], na.rm = TRUE) / 
      sum(estimate[variable == "pop"], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(county_fips = GEOID) %>%
  select(county_fips, year, pr_pov)

#-------------------------------------------------------------------------------
# 4. COMPONENT 4: PUBLIC ASSISTANCE INCOME
#-------------------------------------------------------------------------------
di_pubassi <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    state = state,
    variables = c(
      total_hh          = "B19057_001",       
      public_assistance = "B19057_002" 
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

di_pubassi <- di_pubassi %>%
  pivot_wider(
    id_cols = c(GEOID, year),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(pr_pubassi = public_assistance / total_hh) %>%
  rename(county_fips = GEOID) %>%
  select(county_fips, year, pr_pubassi)

#-------------------------------------------------------------------------------
# 5. COMPONENT 5: UNEMPLOYMENT
#-------------------------------------------------------------------------------
di_unemp <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    state = state,
    variables = c(
      total_pop  = "B23001_001",
      unemployed = "B23025_005"
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

di_unemp <- di_unemp %>%
  pivot_wider(
    id_cols = c(GEOID, year),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(pr_unemprate = unemployed / total_pop) %>%
  rename(county_fips = GEOID) %>%
  select(county_fips, year, pr_unemprate)

#-------------------------------------------------------------------------------
# 6. COMPONENT 6: MEDIAN HOUSEHOLD INCOME
#-------------------------------------------------------------------------------
di_income <- map_dfr(years, ~{
  get_acs(
    geography = "county",
    state = state,
    variables = c(
      median_income = "B19013_001"
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

di_income <- di_income %>%
  pivot_wider(
    id_cols = c(GEOID, year),
    names_from = variable,
    values_from = estimate
  ) %>%
  rename(county_fips = GEOID) %>%
  select(county_fips, year, median_income)

#-------------------------------------------------------------------------------
# 7. MERGE ALL COUNTY COMPONENTS & STANDARDIZE
#-------------------------------------------------------------------------------
cat("2. Merging and standardizing county-level components...\n")

county_df <- di_educ %>%
  left_join(di_fehh,    by = c("year", "county_fips")) %>%
  left_join(di_pov,     by = c("year", "county_fips")) %>%
  left_join(di_pubassi, by = c("year", "county_fips")) %>%
  left_join(di_unemp,   by = c("year", "county_fips")) %>%
  left_join(di_income,  by = c("year", "county_fips")) %>%
  mutate(
    year = as.numeric(year),
    state_fips = str_sub(county_fips, 1, 2)
  )

# Replace NaN / Inf with NA
county_df <- county_df %>%
  mutate(across(where(is.numeric), ~replace(., is.nan(.) | is.infinite(.), NA)))

# Standardize components at the county level
county_df <- county_df %>%
  mutate(
    pr_std_female_hh = (pr_female_hh - mean(pr_female_hh, na.rm = TRUE)) / sd(pr_female_hh, na.rm = TRUE),
    pr_std_pov        = (pr_pov - mean(pr_pov, na.rm = TRUE)) / sd(pr_pov, na.rm = TRUE),
    pr_std_pubassi    = (pr_pubassi - mean(pr_pubassi, na.rm = TRUE)) / sd(pr_pubassi, na.rm = TRUE),
    pr_std_unemprate  = (pr_unemprate - mean(pr_unemprate, na.rm = TRUE)) / sd(pr_unemprate, na.rm = TRUE),
    pr_std_hs_or_low  = (pr_hs_or_low - mean(pr_hs_or_low, na.rm = TRUE)) / sd(pr_hs_or_low, na.rm = TRUE),
    # Reverse-coded median income: higher income = lower disadvantage
    pr_std_income     = -(median_income - mean(median_income, na.rm = TRUE)) / sd(median_income, na.rm = TRUE)
  )

#-------------------------------------------------------------------------------
# 8. COMPUTE COUNTY-LEVEL CDI SCORES (V1, V2, V3)
#-------------------------------------------------------------------------------
cat("3. Computing County-level CDI PCA scores (National & State levels)...\n")

cdi_v1 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate")
cdi_v2 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate", "pr_std_hs_or_low")
cdi_v3 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate", "pr_std_hs_or_low", "pr_std_income")
cdi_versions <- list(v1 = cdi_v1, v2 = cdi_v2, v3 = cdi_v3)

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

# National-level PCA
for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  county_df[[paste0("disad_national_", v)]] <- run_pca_scores(county_df, vars)
}

# State-level PCA
for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  col_name <- paste0("disad_state_", v)
  
  state_scores <- county_df %>%
    group_by(state_fips) %>%
    group_modify(~ {
      .x[[col_name]] <- run_pca_scores(.x, vars)
      .x
    }) %>%
    ungroup()
  
  county_df[[col_name]] <- state_scores[[col_name]]
}

# Clean infinite values and select final columns
county_release <- county_df %>%
  select(
    state_fips, county_fips, year,
    # Raw proportions
    pr_female_hh, pr_pov, pr_pubassi, pr_unemprate, pr_hs_or_low, median_income,
    # Standardized items
    pr_std_female_hh, pr_std_pov, pr_std_pubassi, pr_std_unemprate, pr_std_hs_or_low, pr_std_income,
    # National CDI
    disad_national_v1, disad_national_v2, disad_national_v3,
    # State CDI
    disad_state_v1, disad_state_v2, disad_state_v3
  )

#-------------------------------------------------------------------------------
# 9. SAVE OUTPUT DATASETS
#-------------------------------------------------------------------------------
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

saveRDS(county_release, file.path(data_dir, "(created) cdi_county_level_release.RDS"))

tryCatch({
  write.csv(county_release, file.path(data_dir, "(created) cdi_county_level_release.csv"), row.names = FALSE, na = "")
}, error = function(e) {
  write.csv(county_release, file.path(data_dir, "cdi_county_level_release.csv"), row.names = FALSE, na = "")
})

cat("County-level CDI release dataset complete. N =", nrow(county_release), "counties.\n")

#-------------------------------------------------------------------------------
# 10. GENERATE AND SAVE INTERACTIVE COUNTY CDI MAP (V1 ONLY)
#-------------------------------------------------------------------------------
cat("4. Building interactive MapLibre GL map via mapgl for County CDI (V1 Only)...\n")
if (!dir.exists(figure_dir)) dir.create(figure_dir, recursive = TRUE)

us_counties <- tigris::counties(state = NULL, cb = TRUE, year = 2020) %>%
  st_transform(crs = 4326) %>%
  rename(county_fips = GEOID)

map_county_df <- us_counties %>%
  select(county_fips, NAME) %>%
  inner_join(
    county_release %>% select(county_fips, disad_national_v1, disad_state_v1),
    by = "county_fips"
  ) %>%
  mutate(across(starts_with("disad_"), ~round(., 2)))

map_county <- maplibre(
  style = carto_style("positron"),
  center = c(-98.5795, 39.8283),
  zoom = 3.5
) %>%
  add_source(
    id = "county_source",
    data = map_county_df
  ) %>%
  # 1. National Scaled CDI V1
  add_fill_layer(
    id = "National Scaled CDI (V1 Baseline)",
    source = "county_source",
    fill_color = interpolate(
      column = "disad_national_v1",
      values = c(-2, -1, 0, 1, 2),
      stops = c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", "#d73027")
    ),
    fill_opacity = 0.75,
    fill_outline_color = "#ffffff",
    tooltip = "NAME"
  ) %>%
  # 2. State Scaled CDI V1
  add_fill_layer(
    id = "State Scaled CDI (V1 Baseline)",
    source = "county_source",
    fill_color = interpolate(
      column = "disad_state_v1",
      values = c(-2, -1, 0, 1, 2),
      stops = c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", "#d73027")
    ),
    fill_opacity = 0.75,
    fill_outline_color = "#ffffff",
    visibility = "none",
    tooltip = "NAME"
  ) %>%
  add_continuous_legend(
    legend_title = "CDI Score Range (-2 to +2)",
    values = c("-2 (Low)", "-1", "0", "+1", "+2 (High)"),
    colors = c("#4575b4", "#91bfdb", "#e0f3f8", "#fee090", "#d73027"),
    position = "bottom-right"
  ) %>%
  add_layers_control(position = "top-right", collapsible = FALSE)

htmlwidgets::saveWidget(map_county, file.path(figure_dir, "interactive_map_county_cdi.html"), selfcontained = FALSE)
cat("Saved interactive county CDI map (V1 Only) to Figure/interactive_map_county_cdi.html\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================
