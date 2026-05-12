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
data_dir <- "C:/Users/User/OneDrive/Github Desktop/Concentrated-Disadvantage-Index/Data"
output_dir <- "C:/Users/User/OneDrive/Github Desktop/Concentrated-Disadvantage-Index"

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
avp1 <- readRDS("C:/Users/User/OneDrive/Github Desktop/Concentrated-Disadvantage-Index/Data/fatal_shootings_2019-2023.RDS")

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

#--- PCA Helper
# Runs 1-component PCA on complete cases; returns scores aligned to full data
run_svi_pca <- function(data, vars) {
  cc  <- complete.cases(data %>% select(all_of(vars)))
  sub <- data %>% filter(cc) %>% select(all_of(vars))
  pca <- principal(sub, nfactors = 1, rotate = "none", scores = TRUE)
  scores <- rep(NA, nrow(data))
  scores[cc] <- pca$scores[, 1]
  list(pca = pca, scores = scores)
}

#--- Prepare analytic data (drop geometry for PCA)
svi_clean <- finaldata1 %>%
  st_drop_geometry() %>%
  mutate(across(c(e_pov150, e_unemp, e_hburd, e_nohsdp, e_uninsur,
                  e_age65, e_age17, e_disabl, e_sngpnt, e_limeng,
                  e_minrty,
                  e_munit, e_mobile, e_crowd, e_noveh, e_groupq), as.numeric))

#--- Theme 1: Socioeconomic Status
pca_svi_t1 <- run_svi_pca(svi_clean, c("e_pov150", "e_unemp", "e_hburd", "e_nohsdp", "e_uninsur"))
cat("\n=== SVI Theme 1 PCA (Socioeconomic) ===\n"); print(pca_svi_t1$pca)

#--- Theme 2: Household Characteristics
pca_svi_t2 <- run_svi_pca(svi_clean, c("e_age65", "e_age17", "e_disabl", "e_sngpnt", "e_limeng"))
cat("\n=== SVI Theme 2 PCA (Household Characteristics) ===\n"); print(pca_svi_t2$pca)

#--- Theme 3: Racial & Ethnic Minority (single indicator — z-standardize)
svi_clean <- svi_clean %>%
  mutate(svi_theme3_z = as.numeric(scale(e_minrty)))
cat("\n=== SVI Theme 3: e_minrty z-standardized (single indicator) ===\n")

#--- Theme 4: Housing & Transportation
pca_svi_t4 <- run_svi_pca(svi_clean, c("e_munit", "e_mobile", "e_crowd", "e_noveh", "e_groupq"))
cat("\n=== SVI Theme 4 PCA (Housing & Transportation) ===\n"); print(pca_svi_t4$pca)

#--- Overall SVI
pca_svi_overall <- run_svi_pca(svi_clean,
                               c("e_pov150", "e_unemp", "e_hburd", "e_nohsdp", "e_uninsur",
                                 "e_age65",  "e_age17", "e_disabl", "e_sngpnt", "e_limeng",
                                 "e_munit",  "e_mobile", "e_crowd", "e_noveh",  "e_groupq",
                                 "e_minrty"))
cat("\n=== SVI Overall PCA ===\n"); print(pca_svi_overall$pca)

#--- Store PCA scores in finaldata1
finaldata1 <- finaldata1 %>%
  mutate(
    svi_theme1_pca  = pca_svi_t1$scores,
    svi_theme2_pca  = pca_svi_t2$scores,
    svi_theme3_z    = svi_clean$svi_theme3_z,
    svi_theme4_pca  = pca_svi_t4$scores,
    svi_overall_pca = pca_svi_overall$scores
  )

finaldata1 %>%
  st_drop_geometry() %>%
  summarise(
    t1_mean = mean(svi_theme1_pca,  na.rm = TRUE),
    t1_sd   = sd(svi_theme1_pca,    na.rm = TRUE),
    t2_mean = mean(svi_theme2_pca,  na.rm = TRUE),
    t2_sd   = sd(svi_theme2_pca,    na.rm = TRUE),
    t4_mean = mean(svi_theme4_pca,  na.rm = TRUE),
    t4_sd   = sd(svi_theme4_pca,    na.rm = TRUE),
    ov_mean = mean(svi_overall_pca, na.rm = TRUE),
    ov_sd   = sd(svi_overall_pca,   na.rm = TRUE)
  )

#--- Validate: correlate PCA scores with CDC pre-built rankings
cat("\n=== PCA vs CDC Rankings Correlations ===\n")
finaldata1 %>%
  st_drop_geometry() %>%
  select(svi_theme1_pca, rpl_theme1,
         svi_theme2_pca, rpl_theme2,
         svi_theme4_pca, rpl_theme4,
         svi_overall_pca, rpl_themes) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3) %>%
  print()

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
condis <- readRDS("C:/Users/User/OneDrive/Github Desktop/Concentrated-Disadvantage-Index/Data/(created) concentrated disadvantage items.RDS")

# Merge CDI with existing data
finaldata2 <- finaldata1 %>% 
  left_join(condis, by = "tract_fips")

# Save intermediate file (optional)
# saveRDS(finaldata2, file.path(data_dir, "AVP_SF_intermediate.RDS"))

#-------------------------------------------------------------------------------
# 8. CDI PCA (NATIONAL, STATE, CITY LEVELS)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# CDI VARIABLES: THREE VERSIONS
#-------------------------------------------------------------------------------

cdi_v1 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate")
cdi_v2 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate", "pr_std_hs_or_low")
cdi_v3 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate", "pr_std_hs_or_low", "pr_std_income") # pr_std_income is reverse-coded median income
cdi_versions <- list(v1 = cdi_v1, v2 = cdi_v2, v3 = cdi_v3)

#-------------------------------------------------------------------------------
# PCA HELPER
#-------------------------------------------------------------------------------

run_pca_scores <- function(data, vars) {
  cc     <- complete.cases(data %>% select(all_of(vars)))
  scores <- rep(NA, nrow(data))
  if (sum(cc) >= 5) {
    pca        <- principal(data[cc, vars], nfactors = 1, rotate = "none", scores = TRUE)
    scores[cc] <- pca$scores[, 1]
  }
  scores
}

#-------------------------------------------------------------------------------
# DIAGNOSTIC HELPER
#-------------------------------------------------------------------------------

run_diagnostics <- function(data, vars, label) {
  cat("\n==============================\n")
  cat("DIAGNOSTICS:", label, "\n")
  cat("==============================\n")
  
  df  <- data %>% select(all_of(vars)) %>% na.omit()
  cor_matrix <- cor(df)
  
  cat("\nCorrelation Matrix:\n");         print(round(cor_matrix, 3))
  cat("\nKMO Test:\n");                   print(KMO(df))
  cat("\nBartlett's Test:\n");            print(cortest.bartlett(cor_matrix, n = nrow(df)))
  cat("\nCronbach's Alpha:\n");           print(round(alpha(df)$total$raw_alpha, 3))
  cat("\nAvg Inter-item Correlation:\n"); print(round(alpha(df)$total$average_r, 3))
  cat("\nParallel Analysis:\n")
  fa.parallel(df, fm = "pa", plot = FALSE)
  
  pca <- principal(df, nfactors = 1, rotate = "none", scores = TRUE)
  cat("\nPCA Loadings:\n");              print(pca)
  cat("\nEigenvalue (PC1):",            round(pca$values[1], 3), "\n")
  cat("Variance Explained:",            round(pca$Vaccounted[2, 1] * 100, 1), "%\n")
  
  invisible(pca)
}

#-------------------------------------------------------------------------------
# NATIONAL LEVEL: PCA + DIAGNOSTICS FOR ALL THREE VERSIONS
#-------------------------------------------------------------------------------

finaldata_clean <- finaldata2 %>%
  mutate(row_id = row_number()) %>%
  st_drop_geometry()

for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  
  national_data <- finaldata_clean %>%
    filter(complete.cases(across(all_of(vars))))
  
  # Diagnostics + PCA
  pca <- run_diagnostics(national_data, vars, 
                         paste0(toupper(v), " - National (", length(vars), " items)"))
  
  # Store scores
  finaldata2[[paste0("disad_national_", v)]] <- NA
  finaldata2[[paste0("disad_national_", v)]][national_data$row_id] <- pca$scores[, 1]
  
  # Scree plot
  scree <- data.frame(Factor = 1:length(pca$values), Eigenvalue = pca$values) %>%
    ggplot(aes(x = Factor, y = Eigenvalue)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "black", size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = 1:length(pca$values)) +
    ylim(-0.5, max(pca$values) + 0.5) +
    labs(title = paste0("CDI Scree - National - ", toupper(v)),
         y = "Eigenvalue", x = "Factor Number") +
    theme_classic(base_family = "serif")
  
  ggsave(file.path(output_dir, paste0("Figure/CDI_Scree_National_", v, ".png")),
         plot = scree, width = 7, height = 5, dpi = 300)
}

#-------------------------------------------------------------------------------
# STATE LEVEL: PCA FOR ALL THREE VERSIONS (summary diagnostics only)
#-------------------------------------------------------------------------------

for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  cat("\n===", toupper(v), "- STATE ===\n")
  
  state_results <- finaldata_clean %>%
    group_by(state_fips) %>%
    nest() %>%
    mutate(
      pca_result = map(data, ~{
        cc <- complete.cases(.x %>% select(all_of(vars)))
        if (sum(cc) < 5) return(NULL)
        tryCatch(principal(.x[cc, vars], nfactors = 1, rotate = "none", scores = TRUE),
                 error = function(e) NULL)
      }),
      scores = map2(data, pca_result, ~{
        s <- rep(NA, nrow(.x))
        if (!is.null(.y)) {
          cc <- complete.cases(.x %>% select(all_of(vars)))
          s[cc] <- .y$scores[, 1]
        }
        s
      })
    )
  
  # Summary diagnostics across states
  state_results %>%
    filter(!map_lgl(pca_result, is.null)) %>%
    ungroup() %>%
    transmute(
      eigenvalue_1 = map_dbl(pca_result, ~.x$values[1]),
      alpha_value  = map_dbl(data, ~{
        cc  <- complete.cases(.x %>% select(all_of(vars)))
        if (sum(cc) < 5) return(NA)
        res <- tryCatch(alpha(.x[cc, vars]), error = function(e) NULL)
        if (is.null(res)) return(NA)
        res$total$raw_alpha
      })
    ) %>%
    summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                        sd   = ~sd(.x,   na.rm = TRUE),
                                        min  = ~min(.x,  na.rm = TRUE),
                                        max  = ~max(.x,  na.rm = TRUE)))) %>%
    print()
  
  # Extract and store scores
  state_scores <- state_results %>%
    unnest(cols = c(data, scores)) %>%
    select(tract_fips, state_fips, scores) %>%
    rename(!!paste0("disad_state_", v) := scores)
  
  finaldata2 <- finaldata2 %>%
    left_join(state_scores, by = c("tract_fips", "state_fips"))
}

#-------------------------------------------------------------------------------
# CITY LEVEL: PCA FOR ALL THREE VERSIONS (summary diagnostics only)
#-------------------------------------------------------------------------------

for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  cat("\n===", toupper(v), "- CITY ===\n")
  
  city_results <- finaldata_clean %>%
    filter(!is.na(city_fips)) %>%
    group_by(city_fips) %>%
    filter(n() > 40) %>%
    nest() %>%
    mutate(
      pca_result = map(data, ~{
        cc <- complete.cases(.x %>% select(all_of(vars)))
        if (sum(cc) < 5) return(NULL)
        tryCatch(suppressWarnings(
          principal(.x[cc, vars], nfactors = 1, rotate = "none", scores = TRUE)),
          error = function(e) NULL)
      }),
      scores = map2(data, pca_result, ~{
        s <- rep(NA, nrow(.x))
        if (!is.null(.y)) {
          cc <- complete.cases(.x %>% select(all_of(vars)))
          s[cc] <- .y$scores[, 1]
        }
        s
      })
    )
  
  # Summary diagnostics across cities
  city_results %>%
    filter(!map_lgl(pca_result, is.null)) %>%
    ungroup() %>%
    transmute(
      eigenvalue_1 = map_dbl(pca_result, ~.x$values[1]),
      alpha_value  = map_dbl(data, ~{
        cc  <- complete.cases(.x %>% select(all_of(vars)))
        if (sum(cc) < 5) return(NA)
        res <- tryCatch(alpha(.x[cc, vars]), error = function(e) NULL)
        if (is.null(res)) return(NA)
        res$total$raw_alpha
      })
    ) %>%
    summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                        sd   = ~sd(.x,   na.rm = TRUE),
                                        min  = ~min(.x,  na.rm = TRUE),
                                        max  = ~max(.x,  na.rm = TRUE)))) %>%
    print()
  
  # Extract and store scores
  city_scores <- city_results %>%
    unnest(cols = c(data, scores)) %>%
    select(tract_fips, city_fips, scores) %>%
    rename(!!paste0("disad_city_", v) := scores)
  
  finaldata2 <- finaldata2 %>%
    left_join(city_scores, by = c("tract_fips", "city_fips"))
}

#-------------------------------------------------------------------------------
# 9. SAVE FINAL DATASETS
#-------------------------------------------------------------------------------

# Clean infinite values
finaldata3_clean <- finaldata2 %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA)))

# Save as RDS (preserves spatial object)
saveRDS(
  finaldata3_clean, 
  file.path(data_dir, "(created) finaldata_260508.RDS")
)

# Save as CSV (drops geometry)
names(finaldata3_clean)
finaldata3_csv <- finaldata3_clean %>% 
  st_drop_geometry() %>%
  mutate(across(everything(), ~replace(., is.infinite(.), NA))) %>%
  select(abbr, state_fips, state_name, city_name, city_fips, county_fips, tract_fips,
         tract_pop, crime_count, crime_rate,
         contains("theme"),
         contains("disad"),
         everything()
         )

write.csv(
  finaldata3_csv, 
  file.path(data_dir, "(created) finaldata_260508.csv"), 
  na = "", 
  row.names = FALSE
)

#===============================================================================
# END OF SCRIPT
#===============================================================================