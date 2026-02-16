#===============================================================================
# AMERICAN VIOLENCE PROJECT: VISUALIZATION
#===============================================================================
# Purpose: Create correlation plots and interactive maps for AVP analysis
# Outputs:
#   1. Correlation plot with SVI and CDI sub-items
#   2. Correlation plot with composite indices
#   3. Interactive Leaflet map with multiple layers
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
library(leaflet)
library(RColorBrewer)
library(corrplot)
library(htmlwidgets)

# Set file paths (modify as needed)
data_dir <- "C:/Users/User/OneDrive/Github Desktop/replication-code_concentrated-disadvantage-index/R Code/Data"
figure_dir <- "C:/Users/User/OneDrive/Github Desktop/replication-code_concentrated-disadvantage-index/R Code/Figure"

# Create figure directory if it doesn't exist
if (!dir.exists(figure_dir)) {
  dir.create(figure_dir, recursive = TRUE)
}

#-------------------------------------------------------------------------------
# 1. CORRELATION PLOT: SVI AND CDI SUB-ITEMS
#-------------------------------------------------------------------------------

# Load data with SVI and CDI variables
figdata <- readRDS(file.path(data_dir, "AVP_SF_HL_251102.RDS"))

# Select and rename variables for correlation analysis
figdata_corr <- figdata %>% 
  st_drop_geometry() %>%
  select(
    # Outcome variable
    crime_rate, 
    # Overall SVI
    rpl_themes, 
    # SVI Theme 1: Socioeconomic Status
    ep_pov150, ep_unemp, ep_hburd, ep_nohsdp, ep_uninsur,
    # SVI Theme 2: Household Characteristics
    ep_age65, ep_age17, ep_disabl, ep_sngpnt, ep_limeng,
    # SVI Theme 3: Racial & Ethnic Minority Status
    ep_minrty,
    # SVI Theme 4: Housing Type & Transportation
    ep_munit, ep_mobile, ep_crowd, ep_noveh, ep_groupq,
    # CDI Components
    pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate
  ) %>%
  filter(!is.na(crime_rate)) %>%
  rename(
    "Firearm Homicide Rate" = crime_rate,
    "Overall SVI" = rpl_themes,
    # SVI Theme 1
    "Below 150% Poverty" = ep_pov150,
    "Unemployment" = ep_unemp,
    "Housing Cost Burden" = ep_hburd,
    "No High School Diploma" = ep_nohsdp,
    "No Health Insurance" = ep_uninsur,
    # SVI Theme 2
    "Age 65 or Older" = ep_age65,
    "Age 17 or Younger" = ep_age17,
    "Disability" = ep_disabl,
    "Single Parent Households" = ep_sngpnt,
    "Limited English" = ep_limeng,
    # SVI Theme 3
    "Minority" = ep_minrty,
    # SVI Theme 4
    "Multi-Unit Housing" = ep_munit,
    "Mobile Homes" = ep_mobile,
    "Crowded Housing" = ep_crowd,
    "No Vehicle" = ep_noveh,
    "Group Quarters" = ep_groupq,
    # CDI Components
    "(CDI) Low Education" = pr_hs_or_low,
    "(CDI) Female-Headed Households" = pr_female_hh,
    "(CDI) Poverty" = pr_pov,
    "(CDI) Public Assistance" = pr_pubassi,
    "(CDI) Unemployment Rate" = pr_unemprate
  )

# Create correlation plot with sub-items
png(
  file.path(figure_dir, "correlation_plot_subitems_251102.png"),
  width = 10, 
  height = 8, 
  units = "in", 
  res = 300
)

corrplot(
  cor(figdata_corr, use = "complete.obs"), 
  method = "ellipse",
  type = "lower",
  addCoef.col = 'black',
  tl.col = "black", 
  tl.srt = 45,
  addrect = 2,
  number.cex = 0.6,
  # Brown-to-teal diverging color palette
  col = colorRampPalette(c("#8C510A", "#BF812D", "#DFC27D", "#F6E8C3", 
                           "#FFFFFF", "#C7EAE5", "#80CDC1", "#35978F", 
                           "#01665E"))(200),
  diag = TRUE
)

dev.off()

cat("Correlation plot (sub-items) saved to:", 
    file.path(figure_dir, "correlation_plot_subitems_251102.png"), "\n")

#-------------------------------------------------------------------------------
# 2. CORRELATION PLOT: COMPOSITE INDICES
#-------------------------------------------------------------------------------

# Load data with multi-level CDI scores
figdata2 <- readRDS(file.path(data_dir, "AVP_cleaned_HL_260216.RDS"))

# Select composite indices only
figdata_corr2 <- figdata2 %>% 
  st_drop_geometry() %>%
  select(
    # Outcome variable
    crime_rate,
    # SVI composite indices
    rpl_themes,      # Overall SVI
    rpl_theme1,      # Theme 1: Socioeconomic Status
    rpl_theme2,      # Theme 2: Household Characteristics
    rpl_theme3,      # Theme 3: Racial & Ethnic Minority Status
    rpl_theme4,      # Theme 4: Housing Type & Transportation
    # CDI at different geographic levels
    disad_national,  # National-level CDI
    disad_state,     # State-level CDI
    disad_city       # City-level CDI
  ) %>%
  filter(!is.na(crime_rate)) %>%
  rename(
    "Firearm Homicide Rate" = crime_rate,
    "Overall SVI" = rpl_themes,
    "SVI1: Socioeconomic Status" = rpl_theme1,
    "SVI2: Household Characteristics" = rpl_theme2,
    "SVI3: Racial & Ethnic Minority" = rpl_theme3,
    "SVI4: Housing & Transportation" = rpl_theme4,
    "CDI (National)" = disad_national,
    "CDI (State)" = disad_state,
    "CDI (City)" = disad_city
  )

# Create correlation plot with composite indices
png(
  file.path(figure_dir, "correlation_plot_251102.png"),
  width = 10, 
  height = 8, 
  units = "in", 
  res = 300
)

corrplot(
  cor(figdata_corr2, use = "complete.obs"), 
  method = "ellipse",
  type = "lower",
  sig.level = 0.1,
  insig = "blank",  # Hide non-significant correlations
  addCoef.col = 'black',
  tl.col = "black", 
  tl.srt = 45,
  addrect = 2,
  number.cex = 0.6,
  col = colorRampPalette(c("#8C510A", "#BF812D", "#DFC27D", "#F6E8C3", 
                           "#FFFFFF", "#C7EAE5", "#80CDC1", "#35978F", 
                           "#01665E"))(200),
  diag = TRUE
)

dev.off()

cat("Correlation plot (composite indices) saved to:", 
    file.path(figure_dir, "correlation_plot_251102.png"), "\n")

#-------------------------------------------------------------------------------
# 3. INTERACTIVE MAP
#-------------------------------------------------------------------------------

# Create categorical variable for crime rate
# Breaks based on quantiles or natural breaks
crime_breaks <- c(0, 0.66, 2.01, 4.36, 8.85, 30.87, 116.82, 187.5)

figdata <- figdata %>%
  mutate(
    crime_rate_rec = cut(
      crime_rate,
      include.lowest = TRUE,
      right = FALSE,
      dig.lab = 2,
      breaks = crime_breaks
    ),
    # Create readable labels for categories
    crime_rate_rec = fct_recode(
      crime_rate_rec,
      "0.00-0.66" = "[0,0.66)",
      "0.66-2.01" = "[0.66,2)",
      "2.01-4.36" = "[2,4.4)",
      "4.36-8.85" = "[4.4,8.8)",
      "8.85-30.87" = "[8.8,31)",
      "30.87-116.82" = "[31,1.2e+02)",
      "116.82-187.50" = "[1.2e+02,1.9e+02]"
    )
  )

# Create interactive Leaflet map with multiple layers
map <- leaflet(figdata) %>%
  # Add basemap
  addProviderTiles("CartoDB.Positron") %>%
  
  # Layer 1: Crime Rate (categorical)
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 1,
    fillColor = ~colorFactor(brewer.pal(7, "Reds"), crime_rate_rec)(crime_rate_rec),
    group = "Crime Rate",
    label = ~crime_rate_rec
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colorFactor(brewer.pal(7, "Reds"), figdata$crime_rate_rec),
    values = ~crime_rate_rec,
    title = "Crime Rate",
    opacity = 1,
    group = "Crime Rate"
  ) %>%
  
  # Layer 2: Overall SVI (continuous)
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 0.8,
    fillColor = ~colorNumeric(brewer.pal(6, "Blues"), rpl_themes)(rpl_themes),
    group = "Overall SVI",
    label = ~rpl_themes
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colorNumeric(brewer.pal(6, "Blues"), figdata$rpl_themes),
    values = ~rpl_themes,
    title = "Overall SVI",
    opacity = 0.8,
    group = "Overall SVI"
  ) %>%
  
  # Layer 3: SVI Theme 1 - Socioeconomic Status
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 0.5,
    fillColor = ~colorNumeric(brewer.pal(9, "Purples"), rpl_theme1)(rpl_theme1),
    group = "SVI 1: Socioeconomic Status",
    label = ~rpl_theme1
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colorNumeric(brewer.pal(9, "Purples"), figdata$rpl_theme1),
    values = ~rpl_theme1,
    title = "SVI 1: Socioeconomic Status",
    opacity = 0.5,
    group = "SVI 1: Socioeconomic Status"
  ) %>%
  
  # Layer 4: SVI Theme 2 - Household Characteristics
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 0.5,
    fillColor = ~colorNumeric(brewer.pal(9, "Greens"), rpl_theme2)(rpl_theme2),
    group = "SVI 2: Household Characteristics",
    label = ~rpl_theme2
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(brewer.pal(9, "Greens"), figdata$rpl_theme2),
    values = ~rpl_theme2,
    title = "SVI 2: Household Characteristics",
    opacity = 0.5,
    group = "SVI 2: Household Characteristics"
  ) %>%
  
  # Layer 5: SVI Theme 3 - Racial & Ethnic Minority Status
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 0.5,
    fillColor = ~colorNumeric(brewer.pal(9, "Oranges"), rpl_theme3)(rpl_theme3),
    group = "SVI 3: Racial & Ethnic Minority",
    label = ~rpl_theme3
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(brewer.pal(9, "Oranges"), figdata$rpl_theme3),
    values = ~rpl_theme3,
    title = "SVI 3: Racial & Ethnic Minority",
    opacity = 0.5,
    group = "SVI 3: Racial & Ethnic Minority"
  ) %>%
  
  # Layer 6: SVI Theme 4 - Housing Type & Transportation
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 0.5,
    fillColor = ~colorNumeric(brewer.pal(9, "BuPu"), rpl_theme4)(rpl_theme4),
    group = "SVI 4: Housing & Transportation",
    label = ~rpl_theme4
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(brewer.pal(9, "BuPu"), figdata$rpl_theme4),
    values = ~rpl_theme4,
    title = "SVI 4: Housing & Transportation",
    opacity = 0.5,
    group = "SVI 4: Housing & Transportation"
  ) %>%
  
  # Add layer controls
  addLayersControl(
    overlayGroups = c(
      "Crime Rate", 
      "Overall SVI", 
      "SVI 1: Socioeconomic Status", 
      "SVI 2: Household Characteristics",
      "SVI 3: Racial & Ethnic Minority", 
      "SVI 4: Housing & Transportation"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

# Display map in RStudio viewer
print(map)

# Save interactive map as standalone HTML file
saveWidget(
  map,
  file = file.path(figure_dir, "interactive_map.html"),
  selfcontained = TRUE
)

cat("Interactive map saved to:", 
    file.path(figure_dir, "interactive_map.html"), "\n")

#-------------------------------------------------------------------------------
# 4. SUMMARY
#-------------------------------------------------------------------------------

cat("\n=== VISUALIZATION REPLICATION COMPLETE ===\n")
cat("Outputs created:\n")
cat("1. Correlation plot (sub-items):", 
    file.path(figure_dir, "correlation_plot_subitems_251102.png"), "\n")
cat("2. Correlation plot (composite indices):", 
    file.path(figure_dir, "correlation_plot_251102.png"), "\n")
cat("3. Interactive map:", 
    file.path(figure_dir, "interactive_map.html"), "\n")

#===============================================================================
# END OF SCRIPT
#===============================================================================