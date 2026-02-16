# American Violence Project
# Heeyoung Lee
# 25-11-02

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
library(dplyr)
library(tidyr)

options(tigris_use_cache = TRUE)

#===============================================================================
# MERGE AMERICAN VIOLENCE PROJECT WITH CENSUS SHAPE FILE ####
#===============================================================================

# ===============================
# 1. LOAD AND PREPARE BASE GEOGRAPHIC DATA ####
# ===============================
# Prepare census tract level data
tract <- tigris::tracts(state = NULL, cb = TRUE, year = 2020) %>%
  st_transform(crs = 4326) %>%
  rename(
    abbr = STUSPS,
    tract_fips = GEOID
  ) %>%
  mutate(
    state_fips = str_sub(tract_fips, 1, 2),
  ) %>%
  mutate(across(c(tract_fips, state_fips), as.numeric)) %>%
  dplyr::select(abbr, state_fips, tract_fips)

# Prepare city level geographic data
cities <- tigris::places(state = NULL, cb = TRUE) %>%
  st_transform(crs = 4326) %>%
  rename(
    abbr = STUSPS,
    city_fips = GEOID,
    city_name = NAME
  ) %>%
  mutate(
    state_fips = str_sub(city_fips, 1, 2),
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
  dplyr::select(abbr, state_fips, city_fips, city_name) %>%
  mutate(city_name = paste(abbr, city_name, sep = ", "))
names(cities)

# ===============================
# 2. IMPORT AND CLEAN AVP INCIDENT DATA ####
# ===============================
avp1 <- read.csv("E:/1. Data/American Violence Project (w Matt)/US_fatal_shootings_2019-2023_HL.csv") %>%
  rename(
    tract_fips = tract_id,
    abbr = state_abr,
    city_name = city
  ) %>%
  mutate(
    date = mdy(date),
    avp = 1,
    avp = ifelse(avp == 1, 1, 0),
    city_name = case_when(
      city_name == "NYC" ~ "New York",
      city_name == "DC" ~ "Washington",
      city_name == "Philly" ~ "Philadelphia",
      city_name == "Las Angeles" ~ "Los Angeles",
      city_name == "CO Springs" ~ "Colorado Springs",
      city_name == "North Vegas" ~ "North Las Vegas",
      city_name == "Nashville/Davidson" ~ "Nashville",
      city_name == "Louisville/Jefferson County" ~ "Louisville",
      city_name == "Cincinatti" ~ "Cincinnati",
      city_name == "LasVegas" ~ "Las Vegas",
      city_name == "Sacremento" ~ "Sacramento",
      city_name == "San Fransisco" ~ "San Francisco",
      city_name == "Tuscon" ~ "Tucson",
      city_name == "Pittsburg" ~ "Pittsburgh",
      city_name == "hialeah" ~ "Hialeah",
      TRUE ~ city_name
    )
  ) %>%
  clean_names() %>%
  mutate(city_name = paste(abbr, city_name, sep = ", "))
names(avp1)
length(unique(avp1$city_name))

# ===============================
# 3. SPATIAL JOINS AND FILTERING ####
# ===============================
# Join tracts with cities -> tract level city data
city_tract <- tract %>%
  st_join(cities, join = st_within) %>%
  filter(!is.na(city_name)) %>%
  rename(
    abbr = abbr.x,
    state_fips = state_fips.x
  ) %>%
  select(-contains(".y"))

mapview(city_tract)

# Join with AVP data
avp_sf <- city_tract %>%
  left_join(avp1)

# Then filter to only keep cities with at least one AVP incident
avp_sf_filtered <- avp_sf %>%
  group_by(city_name) %>%
  filter(any(avp == 1, na.rm = TRUE)) %>%
  ungroup()

mapview(avp_sf_filtered)

# ===============================
# 4. AGGREGATE DATA ####
# ===============================
# Group by tract and calculate crime counts
avp_sf_gr <- avp_sf_filtered %>%
  group_by(tract_fips) %>%
  reframe(
    abbr = first(abbr),
    state_fips = first(state_fips),
    city_fips = first(city_fips),
    city_name = first(city_name),
    crime_count = sum(crime_count),
    geometry = first(geometry),
    population = first(population)
  ) %>%
  st_as_sf()

#===============================================================================
# 5. IMPORT SOCIAL VULNERABILITY INDEX ####
#===============================================================================

svi2020 <- read.csv("E:/1. Data/American Violence Project (w Matt)/Social Vulnerability Index/SVI_2020_US (tract).csv") %>% 
  janitor::clean_names() %>%
  # Convert -999 to NA across all columns
  mutate(across(everything(), ~ifelse(. == -999, NA, .))) %>%
  rename(state_fips = st,
         state_name = state,
         abbr = st_abbr,
         county_fips = stcnty,
         tract_fips = fips) %>%
  # Remove margin of error columns
  dplyr::select(-contains("m_"))

# The CDC data already contains:
# 1. Individual percentile ranks (RPL_ variables)
# 2. Theme rankings (RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4)
# 3. Overall ranking (RPL_THEMES)
# 4. Flags (F_ variables)
# 5. Theme flags (F_THEME1, F_THEME2, F_THEME3, F_THEME4)

# We'll organize the existing CDC calculations
svi2020_clean <- svi2020 %>%
  select(
    # Geographic identifiers
    state_fips, state_name, abbr, county_fips, county, tract_fips,
    
    # Population estimates
    e_totpop, e_hu, e_hh,
    
    # Theme 1: Socioeconomic Status
    rpl_theme1,
    ep_pov150, ep_unemp, ep_hburd, ep_nohsdp, ep_uninsur,
    e_pov150, e_unemp, e_hburd, e_nohsdp, e_uninsur,
    
    # Theme 2: Household Characteristics
    rpl_theme2,
    ep_age65, ep_age17, ep_disabl, ep_sngpnt, ep_limeng,
    e_age65, e_age17, e_disabl, e_sngpnt, e_limeng,
    
    # Theme 3: Racial & Ethnic Minority Status
    rpl_theme3,
    ep_minrty, e_minrty,
    
    # Theme 4: Housing Type & Transportation
    rpl_theme4,
    ep_munit, ep_mobile, ep_crowd, ep_noveh, ep_groupq,
    e_munit, e_mobile, e_crowd, e_noveh, e_groupq,
    
    # Overall SVI
    rpl_themes
  )
  # dplyr::select(tract_fips, e_totpop, rpl_theme1, rpl_theme2, rpl_theme3, rpl_theme4, rpl_themes)

finaldata1 <- avp_sf_gr %>% 
  left_join(svi2020_clean) %>%
  mutate(across(where(is.numeric), ~ifelse(is.infinite(.), NA, .))) %>%
  separate(city_name, into = c("abbr", "city_name"), sep = ", ") %>%
  mutate(
    crime_count = ifelse(is.na(crime_count), 0, crime_count),
    crime_rate = crime_count / e_totpop * 1000,
    crime_rate = ifelse(is.na(crime_rate), 0, crime_rate)) %>%
  rename(tract_pop = e_totpop) %>%
  select(-population) %>%
  select(abbr, state_fips, city_name, city_fips, county_fips, tract_fips, everything())
length(unique(finaldata1$city_name))

svi_vars <- finaldata1 %>%
  st_drop_geometry() %>%
  select(starts_with("e_")) %>%
  na.omit() # Remove rows with missing data for PCA/FA

# Define Variable Groups
theme1_vars <- c("e_pov150", "e_unemp", "e_hburd", "e_nohsdp", "e_uninsur")
theme2_vars <- c("e_age65", "e_age17", "e_disabl", "e_sngpnt", "e_limeng")
theme4_vars <- c("e_munit", "e_mobile", "e_crowd", "e_noveh", "e_groupq")
combined_vars <- c(theme1_vars, theme2_vars, theme4_vars, "e_minrty")

plot_scree <- function(data, var_list, title_text) {
  
  # A. Calculate Eigenvalues
  # Subset data
  df_subset <- data %>% select(all_of(var_list))
  
  # Run PCA/FA to get eigenvalues
  # Using correlation matrix to be safe against scale differences
  eigen_values <- eigen(cor(df_subset))$values
  
  # Create Plotting Dataframe
  plot_data <- data.frame(
    Factor = 1:length(eigen_values),
    Eigenvalue = eigen_values
  )
  
  p <- ggplot(plot_data, aes(x = Factor, y = Eigenvalue)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "black", size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") + 
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

out_path <- "C:/Users/User/OneDrive/Research/SVI Social Vulnerability Index (w matt)/Figures"

# [A] Theme 1: Socioeconomic Status
p_t1 <- plot_scree(svi_vars, theme1_vars, "Scree Plot: SVI Theme 1 (Socioeconomic)")
print(p_t1)
ggsave(filename = paste0(out_path, "/SVI_Scree_Theme1.png"), plot = p_t1, width = 7, height = 5, dpi = 300)

# [B] Theme 2: Household Characteristics
p_t2 <- plot_scree(svi_vars, theme2_vars, "Scree Plot: SVI Theme 2 (Household Comp.)")
print(p_t2)
ggsave(filename = paste0(out_path, "/SVI_Scree_Theme2.png"), plot = p_t2, width = 7, height = 5, dpi = 300)

# [C] Theme 4: Housing & Transportation
p_t4 <- plot_scree(svi_vars, theme4_vars, "Scree Plot: SVI Theme 4 (Housing/Transp.)")
print(p_t4)
ggsave(filename = paste0(out_path, "/SVI_Scree_Theme4.png"), plot = p_t4, width = 7, height = 5, dpi = 300)

# [D] Combined SVI (All Variables)
p_comb <- plot_scree(svi_vars, combined_vars, "Scree Plot: Combined SVI Index")
print(p_comb)
ggsave(filename = paste0(out_path, "/SVI_Scree_Combined.png"), plot = p_comb, width = 7, height = 5, dpi = 300)

#===============================================================================
# 6. IMPORT CONCENTRATED DISADVANTAGE INDEX ####
#===============================================================================
condis <- readRDS("E:/1. Data/ACS/Concentrated Disadvantage Index/disadvantage (tract)_251102.RDS")
finaldata2 <- finaldata1 %>% 
  left_join(condis)
saveRDS(finaldata2, "E:/1. Data/American Violence Project (w Matt)/AVP_SF_HL_251102.RDS")

# Factor Analysis with Geography-Specific Structure
library(psych)
library(dplyr)
library(tidyr)
library(sf)

# Prepare data with row identifier
finaldata_clean <- finaldata2 %>%
  mutate(row_id = row_number()) %>%
  st_drop_geometry() %>%
  mutate(across(c(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate), 
                as.numeric))

# 1. NATIONAL LEVEL
efa_vars_national <- finaldata_clean %>%
  select(row_id, tract_fips, state_fips, city_fips, 
         pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)

complete_cases_national <- complete.cases(efa_vars_national %>% 
                                            select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate))
efa_data_national <- efa_vars_national %>% filter(complete_cases_national)

# Diagnostics
cor_matrix_national <- cor(efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate))
print("=== NATIONAL LEVEL ===")
print(round(cor_matrix_national, 3))
KMO(efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate))
cortest.bartlett(cor_matrix_national, n = nrow(efa_data_national))
fa.parallel(efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate), 
            fm = "pa", plot = TRUE)

# EFA
efa_national <- fa(efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate), 
                   nfactors = 1, rotate = "promax", fm = "pa", scores = "regression")
print(efa_national, cut = 0.3)
print(efa_national$Vaccounted)
print("=== NATIONAL EIGENVALUES ===")
print(efa_national$e.values)
print("=== NATIONAL VARIANCE EXPLAINED ===")
print(efa_national$Vaccounted)
print("=== NATIONAL ALPHA RELIABILITY ===")
alpha(efa_data_national %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate))

# Add scores
finaldata2$disad_national <- NA
finaldata2$disad_national[efa_data_national$row_id] <- efa_national$scores[,1]

# Scree plot
national_eigen <- data.frame(
  Factor = 1:length(efa_national$values),
  Eigenvalue = efa_national$values
)
scree_national <- ggplot(national_eigen, aes(x = Factor, y = Eigenvalue)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black", size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") + 
  scale_x_continuous(breaks = 1:5) + # Assuming 5 variables = max 5 factors
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
  filename = "C:/Users/User/OneDrive/Research/SVI Social Vulnerability Index (w matt)/Figures/CDI_Scree_Nation.png",
  plot = scree_national, 
  width = 7, 
  height = 5, 
  dpi = 300)

# 2. STATE LEVEL
state_results <- finaldata_clean %>%
  group_by(state_fips) %>%
  nest() %>%
  mutate(
    efa_result = map(data, ~{
      vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
      complete_cases <- complete.cases(vars)
      
      if(sum(complete_cases) < 5) return(NULL)
      
      tryCatch({
        fa(vars[complete_cases,], nfactors = 1, rotate = "promax", 
           fm = "pa", scores = "regression", max.iter = 100)
      }, error = function(e) NULL)
    }),
    disad_state_new = map2(data, efa_result, ~{
      scores <- rep(NA, nrow(.x))
      if(!is.null(.y)) {
        vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
        complete_cases <- complete.cases(vars)
        scores[complete_cases] <- .y$scores[,1]
      }
      scores
    })
  )

# Print state diagnostics
state_results %>%
  filter(!map_lgl(efa_result, is.null)) %>%
  ungroup() %>%
  transmute(
    eigenvalue_1 = map_dbl(efa_result, ~.x$e.values[1]),
    alpha_value = map_dbl(data, ~{
      vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
      complete_cases <- complete.cases(vars)
      if(sum(complete_cases) < 5) return(NA)
      result <- tryCatch(alpha(vars[complete_cases,]), error = function(e) NULL)
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

state_disad <- state_results %>%
  select(state_fips, data, disad_state_new) %>%
  unnest(cols = c(data, disad_state_new)) %>%
  select(tract_fips, state_fips, disad_state = disad_state_new)

state_eigen_data <- state_results %>%
  filter(!map_lgl(efa_result, is.null)) %>% # Remove failed models
  mutate(
    eigenvalues = map(efa_result, ~ .x$values) # Extract eigenvalues
  ) %>%
  select(state_fips, eigenvalues) %>%
  unnest(eigenvalues) %>%
  group_by(state_fips) %>%
  mutate(factor_num = row_number()) %>%
  ungroup()
scree_state <- ggplot(state_eigen_data, aes(x = factor_num, y = eigenvalues, group = state_fips)) +
  # Draw faint lines for individual states
  geom_line(alpha = 0.3, color = "grey50") +
  # Add the Kaiser criterion line
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  # Add a bold line for the AVERAGE state (optional, but helpful)
  stat_summary(fun = mean, geom = "line", aes(group = 1), 
               color = "black", linewidth = 1.5) +
  stat_summary(fun = mean, geom = "point", aes(group = 1), 
               color = "black", size = 3) +
  scale_x_continuous(breaks = 1:5) + # Assuming 5 variables = max 5 factors
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
  filename = "C:/Users/User/OneDrive/Research/SVI Social Vulnerability Index (w matt)/Figures/CDI_Scree_State.png",
  plot = scree_state, 
  width = 7, 
  height = 5, 
  dpi = 300)

# 3. CITY LEVEL
city_results <- finaldata_clean %>%
  filter(!is.na(city_fips)) %>%
  group_by(city_fips) %>%
  filter(n() > 40) %>%
  nest() %>%
  mutate(
    efa_result = map(data, ~{
      vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
      complete_cases <- complete.cases(vars)
      
      if(sum(complete_cases) < 5) return(NULL)
      
      tryCatch({
        suppressWarnings(
          fa(vars[complete_cases,], nfactors = 1, rotate = "promax", 
             fm = "pa", scores = "regression", max.iter = 100)
        )
      }, error = function(e) NULL)
    }),
    disad_city_new = map2(data, efa_result, ~{
      scores <- rep(NA, nrow(.x))
      if(!is.null(.y)) {
        vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
        complete_cases <- complete.cases(vars)
        scores[complete_cases] <- .y$scores[,1]
      }
      scores
    })
  )

# City summaries
city_results %>%
  filter(!map_lgl(efa_result, is.null)) %>%
  ungroup() %>%
  transmute(
    eigenvalue_1 = map_dbl(efa_result, ~.x$e.values[1]),
    alpha_value = map_dbl(data, ~{
      vars <- .x %>% select(pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate)
      complete_cases <- complete.cases(vars)
      if(sum(complete_cases) < 5) return(NA)
      result <- tryCatch(alpha(vars[complete_cases,]), error = function(e) NULL)
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

city_disad <- city_results %>%
  select(city_fips, data, disad_city_new) %>%
  unnest(cols = c(data, disad_city_new)) %>%
  select(tract_fips, city_fips, disad_city = disad_city_new)

city_eigen_data <- city_results %>%
  filter(!map_lgl(efa_result, is.null)) %>%
  mutate(eigenvalues = map(efa_result, ~ .x$values)) %>%
  select(city_fips, eigenvalues) %>%
  unnest(eigenvalues) %>%
  group_by(city_fips) %>%
  mutate(factor_num = row_number()) %>%
  ungroup()
scree_city <- ggplot(city_eigen_data, aes(x = factor_num, y = eigenvalues, group = city_fips)) +
  geom_line(alpha = 0.1, color = "grey50") + # Lower alpha because there are many cities
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
  filename = "C:/Users/User/OneDrive/Research/SVI Social Vulnerability Index (w matt)/Figures/CDI_Scree_City.png",
  plot = scree_city, 
  width = 7, 
  height = 5, 
  dpi = 300)

# 4. JOIN ALL 
finaldata3 <- finaldata2 %>%
  left_join(state_disad, by = c("tract_fips", "state_fips")) %>%
  left_join(city_disad, by = c("tract_fips", "city_fips"))

# 5. CHECK CORRELATIONS 
cor_disad <- finaldata3 %>%
  st_drop_geometry() %>%
  select(disad_national, disad_state, disad_city) %>%
  na.omit() %>%
  cor()

print("\n=== CORRELATIONS BETWEEN DISADVANTAGE SCORES ===")
print(round(cor_disad, 3))
length(unique(finaldata3$city_name))

#===============================================================================
# 7. EXTRACT ####
#===============================================================================
finaldata3_clean <- finaldata3 %>%
  mutate(across(where(is.numeric), ~replace(., is.infinite(.), NA)))
saveRDS(finaldata3_clean, "E:/1. Data/American Violence Project (w Matt)/AVP_cleaned_HL_251102.RDS")

finaldata3_csv <- finaldata3 %>% 
  st_drop_geometry() %>%
  mutate(across(everything(), ~replace(., is.infinite(.), NA)))
write.csv(finaldata3_csv, "E:/1. Data/American Violence Project (w Matt)/AVP_cleaned_HL_251102.csv", na = "")
