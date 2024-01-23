
library(DiagrammeR)
library(vtree)

# total n -----------------------------------------------------------------

n_no_excl = obi_cohort %>%
  count() %>%
  pull()

n_no_excl

# n after exclusions ------------------------------------------------------

n_excl = paper_cohort %>%
  count() %>%
  pull()

n_excl

# n unplanned ces for FHT -------------------------------------------------

n_unplanned_ces = paper_cohort %>% 
  filter(ces_for_FHT == 1) %>% 
  count() %>% 
  pull()

n_unplanned_ces

unplanned_ces_pct = round(n_unplanned_ces / n_excl, digits = 3)*100

# individual Ns for Figure 1 ----------------------------------------------

## number births not missing race or ethnicity

n_raceeth_notmissing = obi_cohort %>% 
  filter(race_ethnicity3 != "RACE AND/OR ETHNICITY MISSING") %>% 
  nrow()

## number of births not spont after induction OR planned ces

n_right_MOD = obi_cohort %>% 
  filter(race_ethnicity3 != "RACE AND/OR ETHNICITY MISSING",
         planned_mode_of_delivery_cd != 2,
         admit_labor_status_cd != 5) %>% 
  nrow()

## number of births not at hospitals that didn't have all three years data

n_hosp = obi_cohort %>% 
  filter(race_ethnicity3 != "RACE AND/OR ETHNICITY MISSING",
         planned_mode_of_delivery_cd != 2,
         admit_labor_status_cd != 5,
         site_name %in% sites_3_yr) %>% 
  nrow()

## number of births not missing any covariates

n_nomiss = obi_cohort %>%
  filter(
    race_ethnicity3 != "RACE AND/OR ETHNICITY MISSING",
    planned_mode_of_delivery_cd != 2,
    admit_labor_status_cd != 5,
    site_name %in% sites_3_yr,
    payment_source_e != "",
    TeachingStatus != ""
  ) %>%
  drop_na(
    ppreg_chronic_hyper_b,
    gest_hyper_b,
    ppreg_diabetes_b,
    gest_diabetes_b,
    payment_source_e,
    prenatal_care_e,
    covid_test_status_e,
    admit_bmi_no,
    mom_age,
    prosp_region_char,
    TeachingStatus,
    NICU_Status,
    bc_birth_vol_2022
  ) %>% 
nrow()


# create Figure 1 ---------------------------------------------------------

fig_1 = grViz(
  "digraph flowchart {
      node [fontname = Helvetica, shape = rectangle]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']

      tab1 -> tab2 -> tab3 -> tab4 -> tab5;
}

[1]: paste0('N = ', prettyNum(n_no_excl, big.mark = ','), ' births abstracted from participating OBI hospitals in study timeframe')
[2]: paste0('N = ', prettyNum(n_raceeth_notmissing, big.mark = ','), ' births not missing race or ethnicity')
[3]: paste0('N = ', prettyNum(n_right_MOD, big.mark = ','), ' births not admitted after a prior induction or for a planned Cesarean')
[4]: paste0('N = ', prettyNum(n_hosp, big.mark = ','), ' births at hospitals that participated in OBI all three years (2020 - 2022)')
[5]: paste0('N = ', prettyNum(n_nomiss, big.mark = ','), ' births not missing data on any covariates')"
)

## output -----------------------------------------------------------------

# https://rdrr.io/cran/vtree/man/grVizToPNG.html#:~:text=First%20the%20grViz%20object%20is%20exported%20to%20an,is%20exported%20as%20a%20PNG%20file%20%28using%20png%3A%3AwritePNG%29.

# vtree::grVizToPNG(fig_1, folder = "G:/Shared drives/OBI Administration/Analytics/1. Projects/Birth equity/Unplanned Ces for FHT/FINAL analytics (NOV 2023)/Figures/", filename = "fig_1.png")
