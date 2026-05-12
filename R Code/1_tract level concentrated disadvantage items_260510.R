#===============================================================================
# CONCENTRATED DISADVANTAGE INDEX (CENSUS TRACT LEVEL)
#===============================================================================
# Purpose: Calculate concentrated disadvantage index using ACS 5-year estimates
# Data Source: American Community Survey via tidycensus
# Geographic Level: Census tract
# Year: 2020
# Author: Heeyoung
# Date: Feb 16, 2026
#===============================================================================

#-------------------------------------------------------------------------------
# 1. SETUP
#-------------------------------------------------------------------------------

# Load required packages
library(censusapi)
library(tidycensus)
library(tidyverse)
library(magrittr)
library(readxl)
library(corrplot)

# Note: Before running, ensure you have set your Census API key
# census_api_key("YOUR_KEY_HERE", install = TRUE)

# Define parameters
years <- 2020
names(years) <- years
state <- state.abb  # All 50 US states

#-------------------------------------------------------------------------------
# 2. COMPONENT 1: BELOW HIGH SCHOOL DIPLOMA
#-------------------------------------------------------------------------------
# Variables:
# B06009_001: Population age 25 or above
# B06009_002: Less than high school
# B06009_003: High school graduate (includes equivalency)

di_educ <- map_dfr(years, ~{
  get_acs(
    state = state,
    geography = "tract",
    variables = c(
      po_tot_25plus = "B06009_001",
      po_educ_lh = "B06009_002",
      po_educ_hc = "B06009_003"
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

# Calculate proportion with high school or lower education
di_educ <- di_educ %>%
  group_by(GEOID, year) %>%
  reframe(
    po_tot_25plus = sum(estimate[variable == "po_tot_25plus"], na.rm = TRUE),
    hs_or_lower = sum(estimate[variable %in% c("po_educ_lh", "po_educ_hc")], 
                      na.rm = TRUE),
    pr_hs_or_low = hs_or_lower / po_tot_25plus
  ) %>%
  ungroup() %>%
  rename(tract_fips = GEOID) %>%
  select(tract_fips, year, pr_hs_or_low)

print(di_educ)

#-------------------------------------------------------------------------------
# 3. COMPONENT 2: FEMALE-HEADED HOUSEHOLDS
#-------------------------------------------------------------------------------
# Variables:
# B11001_001: Total households
# B11001_006: Female householder, no spouse present

di_fehh <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    state = state,
    variables = c(
      total_hh = "B11001_001",
      female_hh = "B11001_006"           
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

# Calculate proportion of female-headed households
di_fehh <- di_fehh %>%
  pivot_wider(
    id_cols = c(GEOID, year),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(pr_female_hh = female_hh / total_hh) %>%
  rename(tract_fips = GEOID) %>%
  select(tract_fips, year, pr_female_hh)

print(di_fehh)

#-------------------------------------------------------------------------------
# 4. COMPONENT 3: POVERTY
#-------------------------------------------------------------------------------
# Variables:
# C17002_001: Total population for whom poverty status is determined
# C17002_002: Income below 0.50 of poverty threshold
# C17002_003: Income 0.50 to 0.99 of poverty threshold

di_pov <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    state = state,
    variables = c(
      n1 = "C17002_002",  # Under 0.50
      n2 = "C17002_003",  # 0.50 to 0.99
      pop = "C17002_001"  # Total population
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

# Calculate proportion in poverty (below 100% poverty threshold)
di_pov <- di_pov %>%
  group_by(GEOID, year) %>%
  reframe(
    pr_pov = sum(estimate[variable %in% c("n1", "n2")], na.rm = TRUE) / 
      sum(estimate[variable == "pop"], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(tract_fips = GEOID) %>%
  select(tract_fips, year, pr_pov)

print(di_pov)

#-------------------------------------------------------------------------------
# 5. COMPONENT 4: PUBLIC ASSISTANCE INCOME
#-------------------------------------------------------------------------------
# Variables:
# B19057_001: Total households
# B19057_002: Households with public assistance income

di_pubassi <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    state = state,
    variables = c(
      total_hh = "B19057_001",       
      public_assistance = "B19057_002" 
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

# Calculate proportion receiving public assistance
di_pubassi <- di_pubassi %>%
  pivot_wider(
    id_cols = c(GEOID, year),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(pr_pubassi = public_assistance / total_hh) %>%
  rename(tract_fips = GEOID) %>%
  select(tract_fips, year, pr_pubassi)

print(di_pubassi)

#-------------------------------------------------------------------------------
# 6. COMPONENT 5: UNEMPLOYMENT
#-------------------------------------------------------------------------------
# Variables:
# B23001_001: Total population 16 years and over
# B23025_005: Unemployed

di_unemp <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
    state = state,
    variables = c(
      total_pop = "B23001_001",
      unemployed = "B23025_005"
    ),
    survey = "acs5",
    year = .x
  )
}, .id = "year")

# Calculate unemployment rate
di_unemp <- di_unemp %>%
  pivot_wider(
    id_cols = c(GEOID, year),
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(pr_unemprate = unemployed / total_pop) %>%
  rename(tract_fips = GEOID) %>%
  select(tract_fips, year, pr_unemprate)

print(di_unemp)

#-------------------------------------------------------------------------------
# COMPONENT: MEDIAN HOUSEHOLD INCOME
#-------------------------------------------------------------------------------
# Variables:
# B19013_001: Median household income in the past 12 months

di_income <- map_dfr(years, ~{
  get_acs(
    geography = "tract",
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
  rename(tract_fips = GEOID) %>%
  select(tract_fips, year, median_income)

print(di_income)

#-------------------------------------------------------------------------------
# 7. MERGE ALL COMPONENTS
#-------------------------------------------------------------------------------

con_disad <- di_educ %>%
  left_join(di_fehh,   by = c("year", "tract_fips")) %>%
  left_join(di_pov,    by = c("year", "tract_fips")) %>%
  left_join(di_pubassi, by = c("year", "tract_fips")) %>%
  left_join(di_unemp,  by = c("year", "tract_fips")) %>%
  left_join(di_income, by = c("year", "tract_fips"))

# Convert FIPS code and year to numeric
con_disad <- con_disad %>% 
  mutate(
    tract_fips = as.numeric(tract_fips),
    year = as.numeric(year)
  )

# Replace NaN and Inf with NA
con_disad <- con_disad %>%
  mutate(across(everything(), ~replace(., is.nan(.) | is.infinite(.), NA)))

print(con_disad)

#-------------------------------------------------------------------------------
# 8. STANDARDIZE COMPONENTS
#-------------------------------------------------------------------------------
# Create z-scores for each component

con_disad2 <- con_disad %>%
  mutate(
    pr_std_hs_or_low  = (pr_hs_or_low - mean(pr_hs_or_low, na.rm = TRUE)) /
      sd(pr_hs_or_low, na.rm = TRUE),
    pr_std_female_hh  = (pr_female_hh - mean(pr_female_hh, na.rm = TRUE)) /
      sd(pr_female_hh, na.rm = TRUE),
    pr_std_pov        = (pr_pov - mean(pr_pov, na.rm = TRUE)) /
      sd(pr_pov, na.rm = TRUE),
    pr_std_pubassi    = (pr_pubassi - mean(pr_pubassi, na.rm = TRUE)) /
      sd(pr_pubassi, na.rm = TRUE),
    pr_std_unemprate  = (pr_unemprate - mean(pr_unemprate, na.rm = TRUE)) /
      sd(pr_unemprate, na.rm = TRUE),
    # Income is reverse-coded: higher income = lower disadvantage
    pr_std_income     = -(median_income - mean(median_income, na.rm = TRUE)) /
      sd(median_income, na.rm = TRUE)
  )

print(con_disad2)

#-------------------------------------------------------------------------------
# 9. CORRELATION MATRIX
#-------------------------------------------------------------------------------

con_disad_cor <- con_disad2 %>%
  select(pr_std_hs_or_low, pr_std_female_hh, pr_std_pov,
         pr_std_pubassi, pr_std_unemprate, pr_std_income)

# Create correlation plot
corrplot(
  cor(con_disad_cor, use = "pairwise.complete.obs"), 
  method = "ellipse", 
  sig.level = 0.1, 
  insig = "blank", 
  addCoef.col = 'black', 
  tl.col = "black", 
  addrect = 2, 
  number.cex = 0.8
)

#-------------------------------------------------------------------------------
# 10. SAVE OUTPUT
#-------------------------------------------------------------------------------

# Save final dataset
saveRDS(
  con_disad2, 
  "C:/Users/User/OneDrive/Github Desktop/Concentrated-Disadvantage-Index/Data/(created) concentrated disadvantage items.RDS"
)

# Print completion message
cat("\nReplication complete. Output saved successfully.\n")
cat("Number of tracts:", nrow(con_disad2), "\n")
cat("Number of missing values per variable:\n")
print(colSums(is.na(con_disad2)))

#===============================================================================
# END OF SCRIPT
#===============================================================================