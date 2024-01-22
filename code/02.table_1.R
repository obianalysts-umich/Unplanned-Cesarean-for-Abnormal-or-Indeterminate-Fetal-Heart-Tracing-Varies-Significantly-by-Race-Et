
library(gtsummary)
library(gt)
library(gtExtras)

table_1 = paper_cohort %>%
  select(-c(patientid, site_name, infant_dob_dt, ces_for_FHT, mode_of_delivery_cd)) %>%
  tbl_summary(
    label = list(
      ces_for_FHT_char = "Cesarean for FHT",
      race_eth ~ "Race-ethnicity",
      insurance_char = "Insurance status",
      IA_at_admission ~ "IA at admission",
      ppreg_chronic_hyper_b = "Prepregnancy chronic hypertension",
      gest_hyper_b = "Gestational hypertension",
      ppreg_diabetes_b = "Prepregnancy diabetes",
      gest_diabetes_b = "Gestational diabetes",
      covid_status = "COVID status at admission",
      AMA = "Maternal age",
      BMI_cat = "BMI category",
      induction_status ~ "Induction status",
      prenatal_care = "Prenatal care",
      prosp_region = "Prosperity region",
      teaching_status = "Hospital teaching status",
      nicu_status = "Hospital NICU status",
      birth_vol_22 = "Singleton birth volume (2022)"
    ),
    type = list(c(ces_for_FHT_char, 
                  ppreg_chronic_hyper_b,
                  gest_hyper_b,
                  ppreg_diabetes_b,
                  gest_diabetes_b,
                  teaching_status) ~ "categorical"),
    digits = list(everything() ~ c(0, 1))
  ) %>%
  as_gt() %>%
  tab_header(title = "Table 1. Selected Characteristics of Study Population")

table_1

# output ------------------------------------------------------------------

# gtsave(table_1, paste0(output_path, "Tables/table_1.docx"))
