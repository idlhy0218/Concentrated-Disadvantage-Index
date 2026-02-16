# American Violence Project Figures
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

#===============================================================================
# Correlation 1 ####
#===============================================================================
figdata <- readRDS("E:/1. Data/American Violence Project (w Matt)/AVP_SF_HL_251102.RDS")

figdata_corr <- figdata %>% 
  sf::st_drop_geometry() %>%
  select(c(crime_rate, 
           rpl_themes, 
           ep_pov150, ep_unemp, ep_hburd, ep_nohsdp, ep_uninsur,
           ep_age65, ep_age17, ep_disabl, ep_sngpnt, ep_limeng,
           ep_minrty,
           ep_munit, ep_mobile, ep_crowd, ep_noveh, ep_groupq,
           pr_hs_or_low, pr_female_hh, pr_pov, pr_pubassi, pr_unemprate
  )) %>%
  filter(!is.na(crime_rate)) %>%
  rename(
    "Firearm Homicide Rate" = crime_rate,
    "Overall SVI" = rpl_themes,
    "Below 150% Poverty" = ep_pov150,
    "Unemployment" = ep_unemp,
    "Housing Cost Burden" = ep_hburd,
    "No High School Diploma" = ep_nohsdp,
    "No Health Insurance" = ep_uninsur,
    "Age 65 or Older" = ep_age65,
    "Age 17 or Younger" = ep_age17,
    "Disability" = ep_disabl,
    "Single Parent Households" = ep_sngpnt,
    "Limited English" = ep_limeng,
    "Minority" = ep_minrty,
    "Multi-Unit Housing" = ep_munit,
    "Mobile Homes" = ep_mobile,
    "Crowded Housing" = ep_crowd,
    "No Vehicle" = ep_noveh,
    "Group Quarters" = ep_groupq,
    "(CDI) Low Education" = pr_hs_or_low,
    "(CDI) Female-Headed Households" = pr_female_hh,
    "(CDI) Poverty" = pr_pov,
    "(CDI) Public Assistance" = pr_pubassi,
    "(CDI) Unemployment Rate" = pr_unemprate
  )

# Open PNG device
png("E:/1. Data/American Violence Project (w Matt)/correlation_plot_subitems_251102.png",
    width = 10, height = 8, units = "in", res = 300)
corrplot::corrplot(cor(figdata_corr, use = "complete.obs"), 
                   method = "ellipse",
                   type = "lower",
                   addCoef.col = 'black',
                   tl.col = "black", 
                   tl.srt = 45,
                   addrect = 2,
                   number.cex = 0.6,
                   col = colorRampPalette(c("#8C510A", "#BF812D", "#DFC27D", "#F6E8C3", 
                                            "#FFFFFF", "#C7EAE5", "#80CDC1", "#35978F", 
                                            "#01665E"))(200),
                   diag = TRUE)

dev.off()

#===============================================================================
# Correlation 2 ####
#===============================================================================

figdata2 <- readRDS("E:/1. Data/American Violence Project (w Matt)/AVP_cleaned_HL_251102.RDS")
figdata_corr2 <- figdata2 %>% 
  sf::st_drop_geometry() %>%
  select(c(crime_rate,
           rpl_themes,
           rpl_theme1, rpl_theme2, rpl_theme3, rpl_theme4,
           disad_national, disad_state, disad_city)) %>%
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

# Open PNG device
png("E:/1. Data/American Violence Project (w Matt)/correlation_plot_251102.png",
    width = 10, height = 8, units = "in", res = 300, family = "Times New Roman")

# Create correlation plot
corrplot::corrplot(cor(figdata_corr2, use = "complete.obs"), 
                   method = "ellipse",
                   type = "lower",
                   sig.level = 0.1,
                   insig = "blank",
                   addCoef.col = 'black',
                   tl.col = "black", 
                   tl.srt = 45,
                   addrect = 2,
                   number.cex = 0.6,
                   col = colorRampPalette(c("#8C510A", "#BF812D", "#DFC27D", "#F6E8C3", 
                                            "#FFFFFF", "#C7EAE5", "#80CDC1", "#35978F", 
                                            "#01665E"))(200),
                   diag = T)

# Close the device
dev.off()

#===============================================================================
# MAP ####
#===============================================================================
figdata_corr$crime_rate_rec <- cut(figdata$crime_rate,
                                 include.lowest = TRUE,
                                 right = FALSE,
                                 dig.lab = 2,
                                 breaks = c(0, 0.66, 2.01, 4.36, 8.85, 30.87, 116.82, 187.5)
)
figdata$crime_rate_rec <- figdata$crime_rate_rec %>%
  fct_recode(
    "0.00-0.66" = "[0,0.66)",
    "0.66-2.01" = "[0.66,2)",
    "2.01-4.36" = "[2,4.4)",
    "4.36-8.85" = "[4.4,8.8)",
    "8.85-30.87" = "[8.8,31)",
    "30.87-116.82" = "[31,1.2e+02)",
    "116.82-187.50" = "[1.2e+02,1.9e+02]"
  )


map <- leaflet(figdata) %>%
  # Add CartoDB Positron basemap
  addProviderTiles("CartoDB.Positron") %>%
  
  # Crime Rate layer
  addPolygons(
    color = "black",
    weight = 1,
    fillOpacity = 1,
    fillColor = ~colorFactor(brewer.pal(7, "Reds"), crime_rate_rec)(crime_rate_rec),
    group = "Crime Rate",
    label = ~crime_rate_rec
  ) %>%
  addLegend(
    position = "bottomleft",  # Changed to bottomleft
    pal = colorFactor(brewer.pal(7, "Reds"), figdata$crime_rate_rec),
    values = figdata$crime_rate_rec,
    title = "Crime Rate",
    opacity = 1,
    group = "Crime Rate"
  ) %>%
  
  # Overall SVI layer
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
    values = figdata$rpl_themes,
    title = "Overall SVI",
    opacity = 0.8,
    group = "Overall SVI"
  ) %>%
  
  # Theme 1: Socioeconomic Status
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
    values = figdata$rpl_theme1,
    title = "SVI 1: Socioeconomic Status",
    opacity = 0.5,
    group = "SVI 1: Socioeconomic Status"
  ) %>%
  
  # Theme 2: Household Characteristics
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
    values = figdata$rpl_theme2,
    title = "SVI 2: Household Characteristics",
    opacity = 0.5,
    group = "SVI 2: Household Characteristics"
  ) %>%
  
  # Theme 3: Racial & Ethnic Minority Status
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
    values = figdata$rpl_theme3,
    title = "SVI 3: Racial & Ethnic Minority",
    opacity = 0.5,
    group = "SVI 3: Racial & Ethnic Minority"
  ) %>%
  
  # Theme 4: Housing Type & Transportation
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
    values = figdata$rpl_theme4,
    title = "SVI 4: Housing & Transportation",
    opacity = 0.5,
    group = "SVI 4: Housing & Transportation"
  ) %>%
  
  # Add layer controls for all layers
  addLayersControl(
    overlayGroups = c("Crime Rate", "Overall SVI", 
                      "SVI 1: Socioeconomic Status", "SVI 2: Household Characteristics",
                      "SVI 3: Racial & Ethnic Minority", "SVI 4: Housing & Transportation"),
    options = layersControlOptions(collapsed = FALSE)
  )

map

# Save the map
htmlwidgets::saveWidget(map,
                        file = "E:/1. Data/American Violence Project (w Matt)/my_map.html",
                        selfcontained = TRUE)