#===============================================================================
# COUNTY-LEVEL SVI & CDI: PCA (NATIONAL)
#===============================================================================

library(tidyverse)
library(psych)
library(sf)

# CDI versions
cdi_v1 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate")
cdi_v2 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate", "pr_std_hs_or_low")
cdi_v3 <- c("pr_std_female_hh", "pr_std_pov", "pr_std_pubassi", "pr_std_unemprate", "pr_std_hs_or_low", "pr_std_income") # pr_std_income is reverse-coded median income
cdi_versions <- list(v1 = cdi_v1, v2 = cdi_v2, v3 = cdi_v3)

svi_t1_vars  <- c("e_pov150", "e_unemp", "e_hburd", "e_nohsdp", "e_uninsur")
svi_t2_vars  <- c("e_age65", "e_age17", "e_disabl", "e_sngpnt", "e_limeng")
svi_t4_vars  <- c("e_munit", "e_mobile", "e_crowd", "e_noveh", "e_groupq")
svi_all_vars <- c(svi_t1_vars, svi_t2_vars, svi_t4_vars, "e_minrty")

data_dir   <- "C:/Users/User/OneDrive/Github Desktop/replication-code_misappropriating-vulnerability/Data"
output_dir <- "C:/Users/User/OneDrive/Github Desktop/replication-code_misappropriating-vulnerability"

#-------------------------------------------------------------------------------
# 1. IMPORT & MERGE
#-------------------------------------------------------------------------------

tract_finaldf <- readRDS(file.path(data_dir, "(created) finaldata_260508.RDS"))

county_svi <- read.csv(file.path(data_dir, "SVI_2020_US_county.csv")) %>%
  janitor::clean_names() %>%
  mutate(across(everything(), ~ifelse(. == -999, NA, .))) %>%
  rename(county_fips = fips)

# Aggregate CDI components from tract to county
county_cdi <- tract_finaldf %>%
  st_drop_geometry() %>%
  group_by(county_fips) %>%
  summarise(
    pr_female_hh  = mean(pr_female_hh,  na.rm = TRUE),
    pr_pov        = mean(pr_pov,        na.rm = TRUE),
    pr_pubassi    = mean(pr_pubassi,    na.rm = TRUE),
    pr_unemprate  = mean(pr_unemprate,  na.rm = TRUE),
    pr_hs_or_low  = mean(pr_hs_or_low,  na.rm = TRUE),
    pr_std_income = mean(pr_std_income, na.rm = TRUE), # Income is reverse-coded: higher income = lower disadvantage
    .groups = "drop"
  ) %>%
  # Scale
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
    pr_std_income  = (pr_std_income - mean(pr_std_income, na.rm = TRUE)) /
      sd(pr_std_income, na.rm = TRUE)  
  )

county_df1 <- tract_finaldf %>%
  st_drop_geometry() %>%
  group_by(state_fips, state_name, county_fips) %>%
  summarise(
    crime_count = sum(crime_count, na.rm = TRUE),
    county_pop  = sum(tract_pop,   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(county_svi, by = "county_fips") %>%
  left_join(county_cdi, by = "county_fips") %>%
  mutate(
    crime_rate = ifelse(county_pop > 0, crime_count / county_pop * 1000, NA)
  )

#-------------------------------------------------------------------------------
# 2. PREPARE ANALYTIC DATA
#-------------------------------------------------------------------------------

all_cdi_vars <- unique(c(cdi_v1, cdi_v2, cdi_v3))

county_df2 <- county_df1 %>%
  mutate(row_id = row_number()) %>%
  mutate(across(c(all_of(all_cdi_vars), all_of(svi_all_vars), "e_minrty"), as.numeric))

#-------------------------------------------------------------------------------
# HELPERS
#-------------------------------------------------------------------------------

run_pca <- function(data, vars) {
  cc  <- complete.cases(data %>% select(all_of(vars)))
  sub <- data %>% filter(cc) %>% select(all_of(vars))
  pca <- principal(sub, nfactors = 1, rotate = "none", scores = TRUE)
  scores <- rep(NA, nrow(data))
  scores[cc] <- pca$scores[, 1]
  list(pca = pca, scores = scores)
}

run_diagnostics <- function(data, vars, label) {
  cat("\n==============================\n")
  cat("DIAGNOSTICS:", label, "\n")
  cat("==============================\n")
  
  df         <- data %>% select(all_of(vars)) %>% na.omit()
  cor_matrix <- cor(df)
  
  cat("\nCorrelation Matrix:\n");         print(round(cor_matrix, 3))
  cat("\nKMO Test:\n");                   print(KMO(df))
  cat("\nBartlett's Test:\n");            print(cortest.bartlett(cor_matrix, n = nrow(df)))
  cat("\nCronbach's Alpha:\n");           print(round(alpha(df)$total$raw_alpha, 3))
  cat("\nAvg Inter-item Correlation:\n"); print(round(alpha(df)$total$average_r, 3))
  cat("\nParallel Analysis:\n")
  fa.parallel(df, fm = "pa", plot = TRUE)
  
  pca <- principal(df, nfactors = 1, rotate = "none", scores = TRUE)
  cat("\nPCA Loadings:\n");     print(pca)
  cat("Eigenvalue (PC1):",     round(pca$values[1], 3), "\n")
  cat("Variance Explained:",   round(pca$Vaccounted[2, 1] * 100, 1), "%\n")
  
  invisible(pca)
}

#-------------------------------------------------------------------------------
# 3. CDI PCA — THREE VERSIONS
#-------------------------------------------------------------------------------

for (v in names(cdi_versions)) {
  vars <- cdi_versions[[v]]
  
  # Diagnostics + PCA
  pca <- run_diagnostics(county_df2, vars,
                         paste0("CDI ", toupper(v), " - County (", length(vars), " items)"))
  
  # Store scores
  county_df2[[paste0("disad_national_", v)]] <- NA
  county_df2[[paste0("disad_national_", v)]][complete.cases(county_df2 %>% select(all_of(vars)))] <-
    pca$scores[, 1]
  
  # Scree plot
  scree <- data.frame(Factor = 1:length(pca$values), Eigenvalue = pca$values) %>%
    ggplot(aes(x = Factor, y = Eigenvalue)) +
    geom_line(color = "black", linewidth = 1) +
    geom_point(color = "black", size = 3) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = 1:length(pca$values)) +
    ylim(-0.5, max(pca$values) + 0.5) +
    labs(title = paste0("CDI Scree - County - ", toupper(v)),
         y = "Eigenvalue", x = "Factor Number") +
    theme_classic(base_family = "serif")
  
  ggsave(file.path(output_dir, paste0("Figure/CDI_Scree_County_", v, ".png")),
         plot = scree, width = 7, height = 5, dpi = 300)
}

#-------------------------------------------------------------------------------
# 4. SVI PCA
#-------------------------------------------------------------------------------

cat("\n=== SVI PCA ===\n")

pca_svi_t1 <- run_pca(county_df2, svi_t1_vars)
cat("\nSVI Theme 1 PCA:\n"); print(pca_svi_t1$pca)
county_df2$svi_theme1_pca <- pca_svi_t1$scores

pca_svi_t2 <- run_pca(county_df2, svi_t2_vars)
cat("\nSVI Theme 2 PCA:\n"); print(pca_svi_t2$pca)
county_df2$svi_theme2_pca <- pca_svi_t2$scores

county_df2$svi_theme3_z <- as.numeric(scale(county_df2$e_minrty))
cat("\nSVI Theme 3: e_minrty z-standardized\n")

pca_svi_t4 <- run_pca(county_df2, svi_t4_vars)
cat("\nSVI Theme 4 PCA:\n"); print(pca_svi_t4$pca)
county_df2$svi_theme4_pca <- pca_svi_t4$scores

pca_svi_overall <- run_pca(county_df2, svi_all_vars)
cat("\nSVI Overall PCA:\n"); print(pca_svi_overall$pca)
county_df2$svi_overall_pca <- pca_svi_overall$scores

# Validate PCA scores vs CDC rankings
cat("\n=== SVI PCA vs CDC Rankings ===\n")
county_df2 %>%
  select(svi_theme1_pca, rpl_theme1,
         svi_theme2_pca, rpl_theme2,
         svi_theme4_pca, rpl_theme4,
         svi_overall_pca, rpl_themes) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3) %>%
  print()

#-------------------------------------------------------------------------------
# 5. SAVE
#-------------------------------------------------------------------------------

county_final <- county_df2 %>% select(-row_id)

saveRDS(county_final, file.path(data_dir, "(created) finaldata county level_260508.RDS"))

write.csv(county_final, file.path(data_dir, "(created) finaldata county level_260508.csv"),
          na = "", row.names = FALSE)

#===============================================================================
# END OF SCRIPT
#===============================================================================