library(gtsummary)
library(glmmTMB)
library(gt)

# read in provider data and crosswalk -------------------------------------

provider_dt_pre <- data.table::fread(provider_dt_path) |> 
  janitor::clean_names()

provider_crosswalk <- data.table::fread(provider_crosswalk_path) |> 
  janitor::clean_names()

# data merge --------------------------------------------------------------

provider_dt <- left_join(provider_dt_pre,
                         provider_crosswalk,
                         by = c("specialty" = "specialty_cd")) |> 
  select(internal_provider_id, specialty_desc)

paper_cohort_prov <- left_join(paper_cohort,
                               provider_dt,
                               by = c("labor_professional_id" = "internal_provider_id")) |>
  # drop NA providers
  drop_na(specialty_desc) |>
  mutate(specialty = factor(
    specialty_desc,
    levels = c(
      "OBGYN",
      "CNM",
      "Family Medicine",
      "Maternal Fetal Medicine",
      "Nurse Practitioner"
    )
  ))

# provider type cohorts ---------------------------------------------------

midwife_cohort <- paper_cohort_prov |> 
  filter(specialty_desc == "CNM")

OBGYN_cohort <- paper_cohort_prov |> 
  filter(specialty_desc == "OBGYN")

# tests -------------------------------------------------------------------

## is the risk of cesarean for FHT higher when delivering with an OBGYN vs. midwife?

prov_model <- glmmTMB(
  formula = ces_for_FHT ~ race_eth +
    specialty +
    insurance_char +
    IA_at_admission +
    ppreg_chronic_hyper_b +
    gest_hyper_b +
    ppreg_diabetes_b +
    gest_diabetes_b +
    covid_status +
    AMA +
    BMI_cat +
    induction_status +
    prenatal_care +
    prosp_region +
    teaching_status +
    nicu_status +
    birth_vol_22 +
    (1 | site_name),
  data = paper_cohort_prov,
  family = binomial
)

## is the risk of ces for FHT different between race-ethnicity groups for midwives and OBGYNs?
fisher.test(midwife_cohort$race_eth, midwife_cohort$ces_for_FHT, simulate.p.value = T)

chisq.test(OBGYN_cohort$race_eth, OBGYN_cohort$ces_for_FHT)

# tables ------------------------------------------------------------------

## model tbl --------------------------------------------------------------

prov_mod_tbl <- prov_model |>
  tbl_regression(
    exponentiate = T,
    label = list(
      race_eth = "Race-ethnicity",
      specialty = "Provider Taxonomy",
      insurance_char = "Insurance status",
      IA_at_admission = "IA at admission",
      ppreg_chronic_hyper_b = "Prepregnancy chronic hypertension",
      gest_hyper_b = "Gestational Hypertension",
      ppreg_diabetes_b = "Prepregnancy Diabetes",
      gest_diabetes_b = "Gestational Diabetes",
      covid_status = "COVID status at admission",
      AMA = "Maternal Age",
      BMI_cat = "BMI category",
      induction_status = "Induction status",
      prenatal_care = "Prenatal care",
      prosp_region = "Prosperity region",
      teaching_status = "Hospital teaching status",
      nicu_status = "Hospital NICU status",
      birth_vol_22 = "Singleton birth volume (2022)"
    )
  ) |>
  as_gt() |>
  tab_header(title = "Table Sx. Post-Hoc Analysis: Add Provider Taxonomy")

## save

gtsave(prov_mod_tbl,
       paste0(output_path_RnR, "table_Sx_model_prov.docx"))

## chisq ------------------------------------------------------------------

cnm_bivar_tbl <- midwife_cohort |>
  select(race_eth, ces_for_FHT) |>
  gtsummary::tbl_summary(by = race_eth) |>
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value = T)) |>
  as_gt() |>
  tab_header(title = "Table Sx. Risk of Cesarean for FHT by Race-Ethnicity Among Births with a CNM as the Active Labor Provider")

obgyn_bivar_tbl <- OBGYN_cohort |>
  select(race_eth, ces_for_FHT) |>
  gtsummary::tbl_summary(by = race_eth) |>
  add_p() |>
  as_gt() |>
  tab_header(title = "Table Sx. Risk of Cesarean for FHT by Race-Ethnicity Among Births with an OBGYN as the Active Labor Provider")

## save

gtsave(cnm_bivar_tbl,
       paste0(output_path_RnR, "table_Sx_bivar_CNM.docx"))

gtsave(obgyn_bivar_tbl,
       paste0(output_path_RnR, "table_Sx_bivar_OBGYN.docx"))
