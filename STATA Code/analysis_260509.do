*===============================================================================
* MISAPPROPRIATING VULNERABILITY: MAIN ANALYSIS
* Authors: Heeyoung Lee
* Date: May 2026
*===============================================================================
* Structure:
*   0. SETUP
*   1. ANALYTIC SAMPLE
*   2. DESCRIPTIVE STATISTICS
*   3. RELIABILITY & FACTOR ANALYSIS
*   4. OVERDISPERSION TEST
*   5. MAIN MODELS (OLS, NBREG, POISSON)
*   6. SUPPLEMENTARY 1: STANDARDIZED COEFFICIENTS
*   7. SUPPLEMENTARY 2: LEVERAGE DIAGNOSTICS
*   8. SUPPLEMENTARY 3: CDI VERSION COMPARISONS
*   9. SUPPLEMENTARY 4: COUNTY-LEVEL ROBUSTNESS CHECK
*===============================================================================

*-------------------------------------------------------------------------------
* 0. SETUP
*-------------------------------------------------------------------------------

clear all
set more off
estimates clear

* File paths (modify as needed)
local data_dir "C:\Users\User\OneDrive\Github Desktop\Concentrated-Disadvantage-Index\Data"
local tract_file  "`data_dir'\(created) finaldata_260508.csv"
local county_file "`data_dir'\(created) finaldata county level_260508.csv"

*-------------------------------------------------------------------------------
* 1. ANALYTIC SAMPLE (TRACT LEVEL)
*-------------------------------------------------------------------------------

import delimited "`tract_file'", clear

* Define analytic sample using listwise deletion across all key predictors
regress crime_rate rpl_theme1 rpl_theme2 rpl_theme3 rpl_theme4 rpl_themes ///
    disad_national_v1 disad_state_v1 disad_city_v1, ro
gen sample = e(sample)
keep if sample == 1

*-------------------------------------------------------------------------------
* 2. DESCRIPTIVE STATISTICS
*-------------------------------------------------------------------------------

sum crime_rate crime_count ///
    rpl_theme1 rpl_theme2 rpl_theme3 rpl_theme4 rpl_themes ///
    disad_national_v1 disad_state_v1 disad_city_v1

* Quartile agreement between SVI and CDI
xtile q_svi = rpl_themes,        nq(4)
xtile q_cdi = disad_national_v1, nq(4)

gen agree = (q_svi == q_cdi)
tab agree
tab q_svi q_cdi, row
tab q_cdi if q_svi == 4

* Explicit % checks
count if q_svi == 4
scalar n_top_svi = r(N)
count if q_svi == 4 & q_cdi == 4
di "Top SVI -> Top CDI: "       round(r(N) / n_top_svi * 100, 0.01) "%"
count if q_svi == 4 & q_cdi <= 2
di "Top SVI -> Bottom 50% CDI: " round(r(N) / n_top_svi * 100, 0.01) "%"
scalar drop n_top_svi

*-------------------------------------------------------------------------------
* 3. RELIABILITY & FACTOR ANALYSIS
*-------------------------------------------------------------------------------

* Variable lists
global svi_all  e_pov150 e_unemp e_hburd e_nohsdp e_uninsur ///
                e_age65 e_age17 e_disabl e_sngpnt e_limeng ///
                e_minrty ///
                e_munit e_mobile e_crowd e_noveh e_groupq
global svi_t1   e_pov150 e_unemp e_hburd e_nohsdp e_uninsur
global svi_t2   e_age65 e_age17 e_disabl e_sngpnt e_limeng
global svi_t4   e_munit e_mobile e_crowd e_noveh e_groupq
global cdi_vars pr_female_hh pr_pov pr_pubassi pr_unemprate

* SVI alphas
alpha $svi_all, item
alpha $svi_t1,  item
alpha $svi_t2,  item
alpha $svi_t4,  item

* SVI PCA
pca $svi_all
screeplot, yline(1)

* CDI alpha and PCA
alpha $cdi_vars, item
pca $cdi_vars
screeplot, yline(1)

*-------------------------------------------------------------------------------
* 4. OVERDISPERSION TEST
*-------------------------------------------------------------------------------

sum crime_count
di "Mean: " r(mean) ", Variance: " r(Var)

quietly poisson crime_count, expo(tract_pop)
scalar ll_p = e(ll)
quietly nbreg   crime_count, expo(tract_pop)
scalar lr = 2 * (e(ll) - ll_p)
di "LR chi2(1) = " round(lr, 0.01) ", p = " round(chi2tail(1, lr), 0.001)
scalar drop ll_p lr

*-------------------------------------------------------------------------------
* 5. MAIN MODELS
*-------------------------------------------------------------------------------

*--- 5A. OLS (crime_rate) ---

/* SVI */
regress crime_rate rpl_theme1, cl(city_fips)
eststo theme1
regress crime_rate rpl_theme2, cl(city_fips)
eststo theme2
regress crime_rate rpl_theme3, cl(city_fips)
eststo theme3
regress crime_rate rpl_theme4, cl(city_fips)
eststo theme4
regress crime_rate rpl_themes, cl(city_fips)
eststo composite

/* CDI */
regress crime_rate disad_national_v1, cl(city_fips)
eststo cd_nation
regress crime_rate disad_state_v1, cl(city_fips)
eststo disad_state_v1
regress crime_rate disad_city_v1, cl(city_fips)
eststo disad_city_v1

esttab theme1 theme2 theme3 theme4 composite cd_nation disad_state_v1 disad_city_v1, ///
    b(%9.2f) se(%9.2f) stats(r2 aic bic N) ///
    mtitles("Theme 1" "Theme 2" "Theme 3" "Theme 4" "Composite" ///
            "CD National" "CD State" "CD City") ///
    title("OLS Models with Robust Standard Errors") ///
    star(* 0.05 ** 0.01 *** 0.001)

eststo clear

*--- 5B. NEGATIVE BINOMIAL (crime_count) ---

/* SVI */
nbreg crime_count rpl_theme1, cl(city_fips) expo(tract_pop)
eststo nbreg_theme1
nbreg crime_count rpl_theme2, cl(city_fips) expo(tract_pop)
eststo nbreg_theme2
nbreg crime_count rpl_theme3, cl(city_fips) expo(tract_pop)
eststo nbreg_theme3
nbreg crime_count rpl_theme4, cl(city_fips) expo(tract_pop)
eststo nbreg_theme4
nbreg crime_count rpl_themes, cl(city_fips) expo(tract_pop)
eststo nbreg_composite

/* CDI */
nbreg crime_count disad_national_v1, cl(city_fips) expo(tract_pop)
eststo nbreg_nation
nbreg crime_count disad_state_v1, cl(city_fips) expo(tract_pop)
eststo nbreg_state
nbreg crime_count disad_city_v1, cl(city_fips) expo(tract_pop)
eststo nbreg_city

esttab nbreg_theme1 nbreg_theme2 nbreg_theme3 nbreg_theme4 nbreg_composite ///
    nbreg_nation nbreg_state nbreg_city, ///
    b(%9.2f) ci(%9.2f) eform stats(r2_p aic bic N) ///
    mtitles("Theme 1" "Theme 2" "Theme 3" "Theme 4" "Composite" ///
            "CD National" "CD State" "CD City") ///
    title("Negative Binomial Models (IRRs)") ///
    star(* 0.05 ** 0.01 *** 0.001)

eststo clear

*--- 5C. POISSON (crime_count) ---

/* SVI */
poisson crime_count rpl_theme1, cl(city_fips) expo(tract_pop)
eststo pois_theme1
poisson crime_count rpl_theme2, cl(city_fips) expo(tract_pop)
eststo pois_theme2
poisson crime_count rpl_theme3, cl(city_fips) expo(tract_pop)
eststo pois_theme3
poisson crime_count rpl_theme4, cl(city_fips) expo(tract_pop)
eststo pois_theme4
poisson crime_count rpl_themes, cl(city_fips) expo(tract_pop)
eststo pois_composite

/* CDI */
poisson crime_count disad_national_v1, cl(city_fips) expo(tract_pop)
eststo pois_nation
poisson crime_count disad_state_v1, cl(city_fips) expo(tract_pop)
eststo pois_state
poisson crime_count disad_city_v1, cl(city_fips) expo(tract_pop)
eststo pois_city

esttab pois_theme1 pois_theme2 pois_theme3 pois_theme4 pois_composite ///
    pois_nation pois_state pois_city, ///
    b(%9.2f) se(%9.2f) eform stats(r2_p aic bic N) ///
    mtitles("Theme 1" "Theme 2" "Theme 3" "Theme 4" "Composite" ///
            "CDI National" "CDI State" "CDI City") ///
    title("Poisson Models (Clustered SE by City)") ///
    star(* 0.05 ** 0.01 *** 0.001)

eststo clear

*-------------------------------------------------------------------------------
* 6. SUPPLEMENTARY 1: STANDARDIZED COEFFICIENTS
*-------------------------------------------------------------------------------

* Standardize all predictors (1 SD = 1 unit)
foreach v of varlist rpl_theme1 rpl_theme2 rpl_theme3 rpl_theme4 rpl_themes ///
                     disad_national_v1 disad_state_v1 disad_city_v1 {
    egen std_`v' = std(`v')
}

/* SVI */
nbreg crime_count std_rpl_theme1, cl(city_fips) expo(tract_pop)
eststo std_nbreg_theme1
nbreg crime_count std_rpl_theme2, cl(city_fips) expo(tract_pop)
eststo std_nbreg_theme2
nbreg crime_count std_rpl_theme3, cl(city_fips) expo(tract_pop)
eststo std_nbreg_theme3
nbreg crime_count std_rpl_theme4, cl(city_fips) expo(tract_pop)
eststo std_nbreg_theme4
nbreg crime_count std_rpl_themes, cl(city_fips) expo(tract_pop)
eststo std_nbreg_composite

/* CDI */
nbreg crime_count std_disad_national_v1, cl(city_fips) expo(tract_pop)
eststo std_nbreg_nation
nbreg crime_count std_disad_state_v1, cl(city_fips) expo(tract_pop)
eststo std_nbreg_state
nbreg crime_count std_disad_city_v1, cl(city_fips) expo(tract_pop)
eststo std_nbreg_city

esttab std_nbreg_theme1 std_nbreg_theme2 std_nbreg_theme3 std_nbreg_theme4 ///
    std_nbreg_composite std_nbreg_nation std_nbreg_state std_nbreg_city, ///
    b(%9.2f) se(%9.2f) eform stats(r2_p aic bic N) ///
    mtitles("Theme 1" "Theme 2" "Theme 3" "Theme 4" "Composite" ///
            "CD National" "CD State" "CD City") ///
    title("Negative Binomial Models with Standardized Predictors (IRRs)") ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    note("IRR represents effect of 1-SD increase in predictor")

eststo clear

*-------------------------------------------------------------------------------
* 7. SUPPLEMENTARY 2: LEVERAGE DIAGNOSTICS (FN #11)
*-------------------------------------------------------------------------------

* OLS-based diagnostics: Cook's D and studentized residuals
regress crime_rate rpl_themes disad_national_v1
predict cooksd, cooksd
predict rstud,  rstudent

sum cooksd
scalar n_obs = r(N)

gen flag = (cooksd > 4 / n_obs) | (abs(rstud) > 2)
tab flag

* Re-estimate key models on trimmed sample
nbreg crime_count rpl_themes        if !flag, cl(city_fips) expo(tract_pop)
nbreg crime_count disad_national_v1 if !flag, cl(city_fips) expo(tract_pop)

* Clean up
drop cooksd rstud flag
scalar drop n_obs

*-------------------------------------------------------------------------------
* 8. SUPPLEMENTARY 3: CDI VERSION COMPARISONS
*-------------------------------------------------------------------------------

/* V1: baseline (4 items) */
nbreg crime_count disad_national_v1, cl(city_fips) expo(tract_pop)
eststo nbreg_nation1
nbreg crime_count disad_state_v1,    cl(city_fips) expo(tract_pop)
eststo nbreg_state1
nbreg crime_count disad_city_v1,     cl(city_fips) expo(tract_pop)
eststo nbreg_city1

/* V2: + education */
nbreg crime_count disad_national_v2, cl(city_fips) expo(tract_pop)
eststo nbreg_nation2
nbreg crime_count disad_state_v2,    cl(city_fips) expo(tract_pop)
eststo nbreg_state2
nbreg crime_count disad_city_v2,     cl(city_fips) expo(tract_pop)
eststo nbreg_city2

/* V3: + education + income */
nbreg crime_count disad_national_v3, cl(city_fips) expo(tract_pop)
eststo nbreg_nation3
nbreg crime_count disad_state_v3,    cl(city_fips) expo(tract_pop)
eststo nbreg_state3
nbreg crime_count disad_city_v3,     cl(city_fips) expo(tract_pop)
eststo nbreg_city3

esttab nbreg_nation1 nbreg_nation2 nbreg_nation3 ///
    nbreg_state1 nbreg_state2 nbreg_state3 ///
    nbreg_city1  nbreg_city2  nbreg_city3, ///
    b(%9.2f) se(%9.2f) eform stats(r2_p aic bic N) ///
    mtitles("National V1" "National V2" "National V3" ///
            "State V1"    "State V2"    "State V3" ///
            "City V1"     "City V2"     "City V3") ///
    title("CDI Version Comparisons (IRRs)") ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    note("V1 = 4 items; V2 = V1 + education; V3 = V2 + income")

eststo clear

*-------------------------------------------------------------------------------
* 9. SUPPLEMENTARY 4: COUNTY-LEVEL ROBUSTNESS CHECK
*-------------------------------------------------------------------------------

import delimited "`county_file'", clear

* Define analytic sample
regress crime_rate rpl_theme1 rpl_theme2 rpl_theme3 rpl_theme4 rpl_themes ///
    disad_national_v1, ro
gen sample = e(sample)
keep if sample == 1

sum crime_rate crime_count ///
    rpl_theme1 rpl_theme2 rpl_theme3 rpl_theme4 rpl_themes disad_national_v1

/* SVI */
nbreg crime_count rpl_theme1, cl(county_fips) expo(county_pop)
eststo nbreg_theme1
nbreg crime_count rpl_theme2, cl(county_fips) expo(county_pop)
eststo nbreg_theme2
nbreg crime_count rpl_theme3, cl(county_fips) expo(county_pop)
eststo nbreg_theme3
nbreg crime_count rpl_theme4, cl(county_fips) expo(county_pop)
eststo nbreg_theme4
nbreg crime_count rpl_themes, cl(county_fips) expo(county_pop)
eststo nbreg_composite

/* CDI */
nbreg crime_count disad_national_v1, cl(county_fips) expo(county_pop)
eststo nbreg_nation

esttab nbreg_theme1 nbreg_theme2 nbreg_theme3 nbreg_theme4 ///
    nbreg_composite nbreg_nation, ///
    b(%9.2f) se(%9.2f) eform stats(r2_p aic bic N) ///
    mtitles("Theme 1" "Theme 2" "Theme 3" "Theme 4" "Composite" "CD National") ///
    title("County-Level Robustness Check (IRRs)") ///
    star(* 0.05 ** 0.01 *** 0.001)

eststo clear

********************************************************************************
* 10. SUPPLEMENTARY 5: SVI PCA-BASED INDICE
********************************************************************************

/* nbreg Models using PCA-scored SVI themes */
nbreg crime_count svi_theme1_pca, cl(city_fips) expo(tract_pop)
eststo nbreg_svi_t1_pca
nbreg crime_count svi_theme2_pca, cl(city_fips) expo(tract_pop)
eststo nbreg_svi_t2_pca
nbreg crime_count svi_theme3_z,   cl(city_fips) expo(tract_pop)
eststo nbreg_svi_t3_z
nbreg crime_count svi_theme4_pca, cl(city_fips) expo(tract_pop)
eststo nbreg_svi_t4_pca
nbreg crime_count svi_overall_pca, cl(city_fips) expo(tract_pop)
eststo nbreg_svi_overall_pca

/* CDI for comparison */
nbreg crime_count disad_national_v1, cl(city_fips) expo(tract_pop)
eststo nbreg_cdi_national
nbreg crime_count disad_state_v1,    cl(city_fips) expo(tract_pop)
eststo nbreg_cdi_state
nbreg crime_count disad_city_v1,     cl(city_fips) expo(tract_pop)
eststo nbreg_cdi_city

esttab nbreg_svi_t1_pca nbreg_svi_t2_pca nbreg_svi_t3_z ///
    nbreg_svi_t4_pca nbreg_svi_overall_pca ///
    nbreg_cdi_national nbreg_cdi_state nbreg_cdi_city, ///
    b(%9.2f) se(%9.2f) eform stats(r2_p aic bic N) ///
    mtitles("SVI Theme 1" "SVI Theme 2" "SVI Theme 3" ///
            "SVI Theme 4" "SVI Overall" ///
            "CDI National" "CDI State" "CDI City") ///
    title("Sup5: Negative Binomial Models — PCA-Scored SVI vs. CDI (IRRs)") ///
    star(* 0.05 ** 0.01 *** 0.001) ///
    note("SVI Themes 1/2/4 scored via PCA; Theme 3 z-standardized (single indicator); CDI National/State/City (V1) for comparison")

eststo clear

*===============================================================================
* END OF DO-FILE
*===============================================================================
