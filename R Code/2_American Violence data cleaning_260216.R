#===============================================================================
# AMERICAN VIOLENCE PROJECT: SPATIAL ANALYSIS
#===============================================================================
# Purpose: Merge American Violence Project shooting data with census geography,
#          Social Vulnerability Index (SVI), and Concentrated Disadvantage Index (CDI)
# Geographic Level: Census tract within cities
# Data Sources: 
#   - American Violence Project fatal shootings (2019-2023)
#   - CDC Social Vulnerability Index (2020)
#   - ACS Concentrated Disadvantage Index (2020)
# Author: Heeyoung Lee
# Date: Feb 16, 2026
#===============================================================================

#-------------------------------------------------------------------------------
# 0. SETUP
#-------------------------------------------------------------------------------

# Load required packages
library(tidyverse)
library(sf)
library(janitor)
library(tidycensus)
library(tigris)
library(mapview)
library(tmap)
library(leaflet)
library(RColorBrewer)
library(psych)
library(lubridate)

# Set tigris cache option
options(tigris_use_cache = TRUE)

# Note: Modify file paths below to match your directory structure
data_dir <- "C:/Users/User/OneDrive/Github Desktop/replication-code_misappropriating-vulnerability/Data"
output_dir <- "C:/Users/User/OneDrive/Github Desktop/replication-code_misappropriating-vulnerability"

#-------------------------------------------------------------------------------
# 1. LOAD AND PREPARE BASE GEOGRAPHIC DATA
#-------------------------------------------------------------------------------

# Download census tract boundaries (2020)
# All 50 states, cartographic boundary files (cb = TRUE for generalized geometry)
tract <- tigris::tracts(state = NULL, cb = TRUE, year = 2020) %>%
  st_transform(crs = 4326) %>%
  rename(
    abbr = STUSPS,
    tract_fips = GEOID
  ) %>%
  mutate(
    state_fips = str_sub(tract_fips, 1, 2)
  ) %>%
  mutate(across(c(tract_fips, state_fips), as.numeric)) %>%
  select(abbr, state_fips, tract_fips)

# Download city (place) boundaries
cities <- tigris::places(state = NULL, cb = TRUE) %>%
  st_transform(crs = 4326) %>%
  rename(
    abbr = STUSPS,
    city_fips = GEOID,
    city_name = NAME
  ) %>%
  mutate(
    state_fips = str_sub(city_fips, 1, 2),
    # Standardize city names for consistency
    city_name = case_when(
      city_name == "New York city" ~ "New York",
      city_name == "Washington city" ~ "Washington",
      city_name == "Nashville-Davidson metropolitan government (balance)" ~ "Nashville",
      city_name == "Indianapolis city (balance)" ~ "Indianapolis",
      city_name == "Lexington-Fayette" ~ "Lexington",
      TRUE ~ city_name
    )
  ) %>%
  mutate(across(c(city_fips, state_fips), as.numeric)) %>%
  select(abbr, state_fips, city_fips, city_name) %>%
  mutate(city_name = paste(abbr, city_name, sep = ", "))

#-------------------------------------------------------------------------------
# 2. IMPORT AND CLEAN AVP INCIDENT DATA
#-------------------------------------------------------------------------------

# Load American Violence Project fatal shooting data (2019-2023)
avp1 <- readRDS("C:/Users/User/OneDrive/Github Desktop/replication-code_misappropriating-vulnerability/Data/fatal_shootings_2019-2023.RDS")

#-------------------------------------------------------------------------------
# 3. SPATIAL JOINS AND FILTERING
#-------------------------------------------------------------------------------

# Identify which tracts fall within city boundaries
city_tract <- tract %>%
  st_join(cities, join = st_within) %>%
  filter(!is.na(city_name)) %>%
  rename(
    abbr = abbr.x,
    state_fips = state_fips.x
  ) %>%
  select(-contains(".y"))

# Optional: visualize city tracts
# mapview(city_tract)

# Join AVP shooting data with city-tract geography
avp_sf <- city_tract %>%
  left_join(avp1, by = c("tract_fips", "abbr", "city_name"))

# Filter to keep only cities with at least one AVP incident
avp_sf_filtered <- avp_sf %>%
  group_by(city_name) %>%
  filter(any(avp == 1, na.rm = TRUE)) %>%
  ungroup()

# Optional: visualize filtered data
# mapview(avp_sf_filtered)

#-------------------------------------------------------------------------------
# 4. AGGREGATE SHOOTING COUNTS BY TRACT
#-------------------------------------------------------------------------------

avp_sf_gr <- avp_sf_filtered %>%
  group_by(tract_fips) %>%
  reframe(
    abbr = first(abbr),
    state_fips = first(state_fips),
    city_fips = first(city_fips),
    city_name = first(city_name),
    crime_count = sum(crime_count, na.rm = TRUE),
    geometry = first(geometry),
    population = first(population)
  ) %>%
  st_as_sf()

#-------------------------------------------------------------------------------
# 5. IMPORT SOCIAL VULNERABILITY INDEX (SVI)
#-------------------------------------------------------------------------------

# Load CDC/ATSDR Social Vulnerability Index 2020 (tract level)
# Source: https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
svi2020 <- read.csv(file.path(data_dir, "SVI_2020_US_tract.csv")) %>% 
  clean_names() %>%
  # Convert -999 (missing data code) to NA
  mutate(across(everything(), ~ifelse(. == -999, NA, .))) %>%
  rename(
    state_fips = st,
    state_name = state,
    abbr = st_abbr,
    county_fips = stcnty,
    tract_fips = fips
  ) %>%
  # Remove margin of error columns (not needed for analysis)
  select(-contains("m_"))

# Select relevant SVI variables
# CDC SVI includes pre-calculated percentile rankings and theme scores
svi2020_clean <- svi2020 %>%
  select(
    # Geographic identifiers
    state_fips, state_name, abbr, county_fips, county, tract_fips,
    
    # Population estimates
    e_totpop, e_hu, e_hh,
    
    # Theme 1: Socioeconomic Status
    rpl_theme1,  # Percentile ranking for Theme 1
    ep_pov150, ep_unemp, ep_hburd, ep_nohsdp, ep_uninsur,  # Percentages
    e_pov150, e_unemp, e_hburd, e_nohsdp, e_uninsur,  # Counts
    
    # Theme 2: Household Characteristics
    rpl_theme2,  # Percentile ranking for Theme 2
    ep_age65, ep_age17, ep_disabl, ep_sngpnt, ep_limeng,
    e_age65, e_age17, e_disabl, e_sngpnt, e_limeng,
    
    # Theme 3: Racial & Ethnic Minority Status
    rpl_theme3,  # Percentile ranking for Theme 3
    ep_minrty, e_minrty,
    
    # Theme 4: Housing Type & Transportation
    rpl_theme4,  # Percentile ranking for Theme 4
    ep_munit, ep_mobile, ep_crowd, ep_noveh, ep_groupq,
    e_munit, e_mobile, e_crowd, e_noveh, e_groupq,
    
    # Overall SVI
    rpl_themes  # Overall percentile ranking
  )

# Merge AVP data with SVI
finaldata1 <- avp_sf_gr %>% 
  left_join(svi2020_clean) %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
  separate(city_name, into = c("abbr", "city_name"), sep = ", ", remove = FALSE) %>%
  mutate(
    # Replace NA crime counts with 0 (no incidents)
    crime_count = ifelse(is.na(crime_count), 0, crime_count),
    # Calculate crime rate per 1,000 population
    crime_rate = crime_count / e_totpop * 1000,
    crime_rate = ifelse(is.na(crime_rate), 0, crime_rate)
  ) %>%
  rename(tract_pop = e_totpop) %>%
  select(-population) %>%
  select(abbr, state_fips, city_name, city_fips, county_fips, tract_fips, everything())

cat("Number of unique cities in final data:", length(unique(finaldata1$city_name)), "\n")

#-------------------------------------------------------------------------------
# 6. SVI SCREE PLOTS (EXPLORATORY FACTOR ANALYSIS)
#-------------------------------------------------------------------------------

# Extract SVI variables for factor analysis
svi_vars <- finaldata1 %>%
  st_drop_geometry() %>%
  select(starts_with("e_")) %>%
  na.omit()

# Define variable groups based on SVI themes
theme1_vars <- c("e_pov150", "e_unemp", "e_hburd", "e_nohsdp", "e_uninsur")
theme2_vars <- c("e_age65", "e_age17", "e_disabl", "e_sngpnt", "e_limeng")
theme4_vars <- c("e_munit", "e_mobile", "e_crowd", "e_noveh", "e_groupq")
combined_vars <- c(theme1_vars, theme2_vars, theme4_vars, "e_minrty")

# Function to create scree plots
plot_scree <- function(data, var_list, title_text) {
  # Subset data to selected variables
  df_subset <- data %>% select(all_of(var_list))
  
  # Calculate eigenvalues from correlation matrix
  eigen_values <- eigen(cor(df_subset))$values
  
  # Create plotting dataframe
  plot_data <- data.frame(
    Factor = 1:length(eigen_values),
    Eigenvalue = eigen_values
  )
  
  # Create scree plot
  p <- ggplot(plot_data, aes(x = Factor, y = Eigenvalue)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "black", size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +  # Kaiser criterion
    scale_x_continuous(breaks = 1:length(eigen_values)) +
    labs(
      title = title_text,
      subtitle = paste0("Variables: ", length(var_list)),
      y = "Eigenvalue",
      x = "Factor Number"
    ) +
    theme_classic(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text = element_text(color = "black", size = 10),
      axis.line = element_line(color = "black", linewidth = 0.8)
    )
  
  return(p)
}

# Generate and save scree plots for each SVI theme
# Theme 1: Socioeconomic Status
p_t1 <- plot_scree(svi_vars, theme1_vars, "Scree Plot: SVI Theme 1 (Socioeconomic)")
print(p_t1)
ggsave(
  filename = file.path(output_dir, "Figure/SVI_Scree_Theme1.png"), 
  plot = p_t1, 
  width = 7, 
  height = 5, 
  dpi = 300
)

# Theme 2: Household Characteristics
p_t2 <- plot_scree(svi_vars, theme2_vars, "Scree Plot: SVI Theme 2 (Household Comp.)")
print(p_t2)
ggsave(
  filename = file.path(output_dir, "Figure/SVI_Scree_Theme2.png"), 
  plot = p_t2, 
  width = 7, 
  height = 5, 
  dpi = 300
)

# Theme 4: Housing & Transportation
p_t4 <- plot_scree(svi_vars, theme4_vars, "Scree Plot: SVI Theme 4 (Housing/Transp.)")
print(p_t4)
ggsave(
  filename = file.path(output_dir, "Figure/SVI_Scree_Theme4.png"), 
  plot = p_t4, 
  width = 7, 
  height = 5, 
  dpi = 300
)

# Combined SVI
p_comb <- plot_scree(svi_vars, combined_vars, "Scree Plot: Combined SVI Index")
print(p_comb)
ggsave(
  filename = file.path(output_dir, "Figure/SVI_Scree_Combined.png"), 
  plot = p_comb, 
  width = 7, 
  height = 5, 
  dpi = 300
)

#-------------------------------------------------------------------------------
# 7. IMPORT CONCENTRATED DISADVANTAGE INDEX (CDI)
#-------------------------------------------------------------------------------

# Load previously calculated CDI from ACS data
condis <- readRDS("C:/Users/User/OneDrive/Github Desktop/replication-code_misappropriating-vulnerability/Data/(created) concentrated disadvantage items.RDS")

# Merge CDI with existing data
finaldata2 <- finaldata1 %>% 
  left_join(condis, by = "tract_fips")

# Save intermediate file (optional)
# saveRDS(finaldata2, file.path(data_dir, "AVP_SF_intermediate.RDS"))

#-------------------------------------------------------------------------------
# 8. MULTI-LEVEL FACTOR ANALYSIS FOR CDI
#-------------------------------------------------------------------------------

# Prepare data for factor analysis
finaldata_clean <- finaldata2 %>%
  mutate(row_id = row_number()) %>%
  st_drop_geometry() %>%
  mutate(across(c(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate), 
                as.numeric))

#-----------------------------------
# 8A. NATIONAL LEVEL FACTOR ANALYSIS
#-----------------------------------

cat("\n=== NATIONAL LEVEL FACTOR ANALYSIS ===\n")

# Select CDI variables
efa_vars_national <- finaldata_clean %>%
  select(row_id, tract_fips, state_fips, city_fips, 
         pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)

# Filter to complete cases
complete_cases_national <- complete.cases(
  efa_vars_national %>% 
    select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
)
efa_data_national <- efa_vars_national %>% filter(complete_cases_national)

# Diagnostic tests
cor_matrix_national <- cor(
  efa_data_national %>% 
    select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
)

cat("\nCorrelation Matrix:\n")
print(round(cor_matrix_national, 3))

cat("\nKMO (Kaiser-Meyer-Olkin) Test:\n")
print(KMO(efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)))

cat("\nBartlett's Test of Sphericity:\n")
print(cortest.bartlett(cor_matrix_national, n = nrow(efa_data_national)))

cat("\nParallel Analysis:\n")
fa.parallel(
  efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate), 
  fm = "pa", 
  plot = TRUE
)

# Conduct Exploratory Factor Analysis (EFA)
# Method: Principal Axis Factoring (fm = "pa")
# Rotation: Promax (oblique rotation allowing correlated factors)
# Scores: Regression method
efa_national <- fa(
  efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate), 
  nfactors = 1, 
  rotate = "promax", 
  fm = "pa", 
  scores = "regression"
)

cat("\nFactor Loadings:\n")
print(efa_national, cut = 0.3)

cat("\nVariance Accounted For:\n")
print(efa_national$Vaccounted)

cat("\nEigenvalues:\n")
print(efa_national$e.values)

cat("\nReliability (Cronbach's Alpha):\n")
print(alpha(efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)))

# Add factor scores to dataset
finaldata2$disad_national <- NA
finaldata2$disad_national[efa_data_national$row_id] <- efa_national$scores[, 1]

# Create scree plot for national level
national_eigen <- data.frame(
  Factor = 1:length(efa_national$values),
  Eigenvalue = efa_national$values
)

scree_national <- ggplot(national_eigen, aes(x = Factor, y = Eigenvalue)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") + 
  scale_x_continuous(breaks = 1:5) +
  ylim(-0.5, 3) +
  labs(
    title = "[A] Nation Level",
    subtitle = "",
    y = "Eigenvalue",
    x = "Factor Number"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(color = "black", linewidth = 0.8)
  )

ggsave(
  filename = file.path(output_dir, "Figure/CDI_Scree_Nation.png"),
  plot = scree_national, 
  width = 7, 
  height = 5, 
  dpi = 300
)

#-----------------------------------
# 8B. STATE LEVEL FACTOR ANALYSIS
#-----------------------------------

cat("\n=== STATE LEVEL FACTOR ANALYSIS ===\n")

# Run separate factor analyses for each state
state_results <- finaldata_clean %>%
  group_by(state_fips) %>%
  nest() %>%
  mutate(
    efa_result = map(data, ~{
      vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
      complete_cases <- complete.cases(vars)
      
      # Require minimum 5 complete cases
      if(sum(complete_cases) < 5) return(NULL)
      
      # Run EFA with error handling
      tryCatch({
        fa(vars[complete_cases, ], nfactors = 1, rotate = "promax", 
           fm = "pa", scores = "regression", max.iter = 100)
      }, error = function(e) NULL)
    }),
    disad_state_new = map2(data, efa_result, ~{
      scores <- rep(NA, nrow(.x))
      if(!is.null(.y)) {
        vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
        complete_cases <- complete.cases(vars)
        scores[complete_cases] <- .y$scores[, 1]
      }
      scores
    })
  )

# Print state-level diagnostics
cat("\nState-Level Summary Statistics:\n")
state_summary <- state_results %>%
  filter(!map_lgl(efa_result, is.null)) %>%
  ungroup() %>%
  transmute(
    eigenvalue_1 = map_dbl(efa_result, ~.x$e.values[1]),
    alpha_value = map_dbl(data, ~{
      vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
      complete_cases <- complete.cases(vars)
      if(sum(complete_cases) < 5) return(NA)
      result <- tryCatch(alpha(vars[complete_cases, ]), error = function(e) NULL)
      if(is.null(result)) return(NA)
      result$total$raw_alpha
    })
  ) %>%
  summarise(
    mean_eigen = mean(eigenvalue_1, na.rm = TRUE),
    sd_eigen = sd(eigenvalue_1, na.rm = TRUE),
    min_eigen = min(eigenvalue_1, na.rm = TRUE),
    max_eigen = max(eigenvalue_1, na.rm = TRUE),
    mean_alpha = mean(alpha_value, na.rm = TRUE),
    sd_alpha = sd(alpha_value, na.rm = TRUE),
    min_alpha = min(alpha_value, na.rm = TRUE),
    max_alpha = max(alpha_value, na.rm = TRUE)
  )
print(state_summary)

# Extract state-level scores
state_disad <- state_results %>%
  select(state_fips, data, disad_state_new) %>%
  unnest(cols = c(data, disad_state_new)) %>%
  select(tract_fips, state_fips, disad_state = disad_state_new)

# Create state-level scree plot
state_eigen_data <- state_results %>%
  filter(!map_lgl(efa_result, is.null)) %>%
  mutate(eigenvalues = map(efa_result, ~ .x$values)) %>%
  select(state_fips, eigenvalues) %>%
  unnest(eigenvalues) %>%
  group_by(state_fips) %>%
  mutate(factor_num = row_number()) %>%
  ungroup()

scree_state <- ggplot(state_eigen_data, aes(x = factor_num, y = eigenvalues, group = state_fips)) +
  geom_line(alpha = 0.3, color = "grey50") +  # Individual state lines
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), 
               color = "black", linewidth = 1.5) +  # Mean line
  stat_summary(fun = mean, geom = "point", aes(group = 1), 
               color = "black", size = 3) +
  scale_x_continuous(breaks = 1:5) +
  ylim(-0.5, 3) +
  labs(
    title = "[B] State Level",
    subtitle = "",
    x = "Factor Number",
    y = "Eigenvalue"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(color = "black", linewidth = 0.8)
  )

ggsave(
  filename = file.path(output_dir, "Figure/CDI_Scree_State.png"),
  plot = scree_state, 
  width = 7, 
  height = 5, 
  dpi = 300
)

#-----------------------------------
# 8C. CITY LEVEL FACTOR ANALYSIS
#-----------------------------------

cat("\n=== CITY LEVEL FACTOR ANALYSIS ===\n")

# Run separate factor analyses for each city (minimum 40 tracts)
city_results <- finaldata_clean %>%
  filter(!is.na(city_fips)) %>%
  group_by(city_fips) %>%
  filter(n() > 40) %>%  # Cities must have >40 tracts
  nest() %>%
  mutate(
    efa_result = map(data, ~{
      vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
      complete_cases <- complete.cases(vars)
      
      if(sum(complete_cases) < 5) return(NULL)
      
      tryCatch({
        suppressWarnings(
          fa(vars[complete_cases, ], nfactors = 1, rotate = "promax", 
             fm = "pa", scores = "regression", max.iter = 100)
        )
      }, error = function(e) NULL)
    }),
    disad_city_new = map2(data, efa_result, ~{
      scores <- rep(NA, nrow(.x))
      if(!is.null(.y)) {
        vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
        complete_cases <- complete.cases(vars)
        scores[complete_cases] <- .y$scores[, 1]
      }
      scores
    })
  )

# Print city-level diagnostics
cat("\nCity-Level Summary Statistics:\n")
city_summary <- city_results %>%
  filter(!map_lgl(efa_result, is.null)) %>%
  ungroup() %>%
  transmute(
    eigenvalue_1 = map_dbl(efa_result, ~.x$e.values[1]),
    alpha_value = map_dbl(data, ~{
      vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
      complete_cases <- complete.cases(vars)
      if(sum(complete_cases) < 5) return(NA)
      result <- tryCatch(alpha(vars[complete_cases, ]), error = function(e) NULL)
      if(is.null(result)) return(NA)
      result$total$raw_alpha
    })
  ) %>%
  summarise(
    mean_eigen = mean(eigenvalue_1, na.rm = TRUE),
    sd_eigen = sd(eigenvalue_1, na.rm = TRUE),
    min_eigen = min(eigenvalue_1, na.rm = TRUE),
    max_eigen = max(eigenvalue_1, na.rm = TRUE),
    mean_alpha = mean(alpha_value, na.rm = TRUE),
    sd_alpha = sd(alpha_value, na.rm = TRUE),
    min_alpha = min(alpha_value, na.rm = TRUE),
    max_alpha = max(alpha_value, na.rm = TRUE)
  )
print(city_summary)

# Extract city-level scores
city_disad <- city_results %>%
  select(city_fips, data, disad_city_new) %>%
  unnest(cols = c(data, disad_city_new)) %>%
  select(tract_fips, city_fips, disad_city = disad_city_new)

# Create city-level scree plot
city_eigen_data <- city_results %>%
  filter(!map_lgl(efa_result, is.null)) %>%
  mutate(eigenvalues = map(efa_result, ~ .x$values)) %>%
  select(city_fips, eigenvalues) %>%
  unnest(eigenvalues) %>%
  group_by(city_fips) %>%
  mutate(factor_num = row_number()) %>%
  ungroup()

scree_city <- ggplot(city_eigen_data, aes(x = factor_num, y = eigenvalues, group = city_fips)) +
  geom_line(alpha = 0.1, color = "grey50") +  # Individual city lines (lighter due to many cities)
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), 
               color = "black", linewidth = 1.5) +
  stat_summary(fun = mean, geom = "point", aes(group = 1), 
               color = "black", size = 3) +
  scale_x_continuous(breaks = 1:5) +
  ylim(-0.5, 3) +
  labs(
    title = "[C] City Level",
    subtitle = "",
    x = "Factor Number",
    y = "Eigenvalue"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(color = "black", size = 10),
    axis.line = element_line(color = "black", linewidth = 0.8)
  )

ggsave(
  filename = file.path(output_dir, "Figure/CDI_Scree_City.png"),
  plot = scree_city, 
  width = 7, 
  height = 5, 
  dpi = 300
)

#-----------------------------------
# 8D. MERGE ALL DISADVANTAGE SCORES
#-----------------------------------

finaldata3 <- finaldata2 %>%
  left_join(state_disad, by = c("tract_fips", "state_fips")) %>%
  left_join(city_disad, by = c("tract_fips", "city_fips"))

# Calculate correlations between disadvantage scores
cor_disad <- finaldata3 %>%
  st_drop_geometry() %>%
  select(disad_national, disad_state, disad_city) %>%
  na.omit() %>%
  cor()

cat("\n=== CORRELATIONS BETWEEN DISADVANTAGE SCORES ===\n")
print(round(cor_disad, 3))

cat("\nFinal number of unique cities:", length(unique(finaldata3$city_name)), "\n")

#-------------------------------------------------------------------------------
# 9. SAVE FINAL DATASETS
#-------------------------------------------------------------------------------

# Clean infinite values
finaldata3_clean <- finaldata3 %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA)))

# Save as RDS (preserves spatial object)
saveRDS(
  finaldata3_clean, 
  file.path(data_dir, "(created) finaldata.RDS")
)

# Save as CSV (drops geometry)
finaldata3_csv <- finaldata3 %>% 
  st_drop_geometry() %>%
  mutate(across(everything(), ~replace(., is.infinite(.), NA)))

write.csv(
  finaldata3_csv, 
  file.path(data_dir, "(created) finaldata.csv"), 
  na = "", 
  row.names = FALSE
)

#===============================================================================
# END OF SCRIPT
#===============================================================================