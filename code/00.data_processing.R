
library(tidyverse)
library(obstinit)


# paths -------------------------------------------------------------------

output_path = "G:/Shared drives/OBI Administration/Analytics/1. Projects/Birth equity/Unplanned Ces for FHT/FINAL analytics (NOV 2023)/"


# data process ------------------------------------------------------------

obi = data.table::fread("P:/OBI_abstracted_data/2023-11-20/data/output/sourcetables_OBI_export_recodes.csv")

obi_cohort = obi %>% 
  create_obi_cohort() %>% 
  # start cohort in March 2020 since that is when we started collecting COVID data and those obs will be dropped from the model anyway
  filter(infant_dob_dt >= lubridate::ymd("2020-03-01"),
         birth_year <= 2022)

## sites with all 3 years data --------------------------------------------

sites_3_yr = obi_cohort %>% 
  group_by(site_name, birth_year) %>% 
  count() %>% 
  pivot_wider(names_from = birth_year, values_from = n) %>% 
  drop_na() %>% 
  pull(site_name)


# cohort for paper --------------------------------------------------------

paper_cohort_pre = obi_cohort %>%
  # planned labors for vaginal birth, drop prior inductions, drop missing RE
  filter(
    planned_mode_of_delivery_cd == 1,
    race_ethnicity3 != "RACE AND/OR ETHNICITY MISSING",
    admit_labor_status_cd != 5,
    payment_source_e != "",
    TeachingStatus != "",
    site_name %in% sites_3_yr
  ) %>%
  #drop missings
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
  mutate(
    # cesarean for FHT
    ces_for_FHT = ifelse(mode_of_delivery_cd == 4 &
                           ces_primary_indication_cd == 6, 1, 0),
    ces_for_FHT_char = factor(
      ces_for_FHT,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    #induction status
    induction_status =
      
      case_when(
        admit_labor_status_cd %in% c(3, 4) ~ "Spontaneous",
        admit_labor_status_cd %in% c(1, 2) &
          induce_primary_indication_cd %in% c(1, 3, 10, 18, 19) ~ "Maternal medical induction",
        admit_labor_status_cd %in% c(1, 2) &
          induce_primary_indication_cd %in% c(4, 8, 13) ~ "Elective induction",
        admit_labor_status_cd %in% c(1, 2) &
          induce_primary_indication_cd %in% c(2, 5, 6, 7, 9, 11, 12, 14, 16) ~ "Medical fetal induction",
        admit_labor_status_cd %in% c(1, 2) &
          induce_primary_indication_cd %in% c(15, 17) ~ "Induction for another indication"
      ),
    induction_status = factor(
      induction_status,
      levels = c(
        "Spontaneous",
        "Maternal medical induction",
        "Medical fetal induction",
        "Elective induction",
        "Induction for another indication"
      )
    ),
    # IA at admission
    IA_at_admission = ifelse(is.na(admit_fm_IA), "No IA at admission", "IA at admission"),
    IA_at_admission = factor(IA_at_admission, levels = c("No IA at admission", "IA at admission")),
    covid_status = case_when(
      covid_test_status_e %in% c(1, 2) ~ "Positive",
      covid_test_status_e == 4 ~ "Negative",
      covid_test_status_e %in% c(3, 5) ~ "Unknown",
      TRUE ~ as.character(NA)
    ),
    covid_status = factor(covid_status, levels = c("Negative", "Positive", "Unknown")),
    # BMI category
    BMI_cat = case_when(
      admit_bmi_no < 18.5 ~ "<18.5",
      admit_bmi_no >= 18.5 &
        admit_bmi_no < 25 ~ "18.5 - 24.9",
      admit_bmi_no >= 25 &
        admit_bmi_no < 30 ~ "25 - 29.9",
      admit_bmi_no >= 30 &
        admit_bmi_no < 35 ~ "30 - 34.9",
      admit_bmi_no >= 35 &
        admit_bmi_no < 40 ~ "35 - 39.9",
      TRUE ~ "BMI >= 40"
    ),
    BMI_cat = factor(
      BMI_cat,
      levels = c(
        "<18.5",
        "18.5 - 24.9",
        "25 - 29.9",
        "30 - 34.9",
        "35 - 39.9",
        "BMI >= 40"
      )
    ),
    # Advanced maternal age
    AMA = ifelse(mom_age >= 35, ">= 35", "< 35"),
    AMA = factor(AMA, levels = c("< 35", ">= 35")),
    # race ethnicity group
    race_eth = factor(
      race_ethnicity3,
      levels = c(
        "WHITE, NON-HISPANIC",
        "AMERICAN INDIAN/ALASKAN NATIVE, NON-HISPANIC",
        "ASIAN/PACIFIC ISLANDER, NON-HISPANIC",
        "BLACK, NON-HISPANIC",
        "HISPANIC",
        "MORE THAN ONE RACE, NOT HISPANIC/LATINO",
        "RACE AND/OR ETHNICITY UNKNOWN"
      ),
      labels = c(
        "White",
        "American Indian or Alaskan Native",
        "Asian or Pacific Islander",
        "Black",
        "Hispanic",
        "More than one race",
        "Race and/or ethnicity unknown"
      )
    ),
    # insurance
    insurance_char = factor(
      insurance_char,
      levels = c(
        "Private only",
        "Medicaid only",
        "Self-pay/none",
        "Multiple/other"
      )
    ),
    # prenatal care
    prenatal_care = ifelse(prenatal_care_e == 1, "Yes", "No/Unknown"),
    prenatal_care = factor(prenatal_care, levels = c("Yes", "No/Unknown")),
    # prosperity region
    prosp_region = factor(
      prosp_region_char,
      levels = c(
        "Southeast Michigan Prosperity Regio",
        "Upper Peninsula Prosperity Alliance",
        "Northwest Prosperity Region",
        "Northeast Prosperity Region",
        "West Michigan Prosperity Alliance",
        "East Central Michigan Prosperity Re",
        "East Michigan Prosperity Region",
        "South Central Prosperity Region",
        "Southwest Prosperity Region",
        "Detroit Metro Prosperity Region"
      ),
      labels = c(
        "Southeast Michigan Prosperity Region",
        "Upper Peninsula Prosperity Alliance",
        "Northwest Prosperity Region",
        "Northeast Prosperity Region",
        "West Michigan Prosperity Alliance",
        "East Central Michigan Prosperity Region",
        "East Michigan Prosperity Region",
        "South Central Prosperity Region",
        "Southwest Prosperity Region",
        "Detroit Metro Prosperity Region"
      )
    ),
    # ppreg HTN
    ppreg_chronic_hyper_b = factor(
      ppreg_chronic_hyper_b,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    # gest HTN
    gest_hyper_b = factor(
      gest_hyper_b,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    # ppreg DBM
    ppreg_diabetes_b = factor(
      ppreg_diabetes_b,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    # gest DBM
    gest_diabetes_b = factor(
      gest_diabetes_b,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    # teaching status
    teaching_status = factor(TeachingStatus, levels = c("No", "Yes")),
    # NICU status
    nicu_status = factor(NICU_Status, levels = c("No", "SCN", "NICU")),
    # singleton birth volume
    birth_vol_22 = case_when(
      bc_birth_vol_2022 < 500 ~ "<500",
      bc_birth_vol_2022 >= 500 &
        bc_birth_vol_2022 < 1000 ~ "500-999",
      bc_birth_vol_2022 >= 1000 &
        bc_birth_vol_2022 < 1500 ~ "1000-1499",
      bc_birth_vol_2022 >= 1500 &
        bc_birth_vol_2022 < 2000 ~ "1500-1999",
      TRUE ~ ">=2000"
    ),
    birth_vol_22 = factor(
      birth_vol_22,
      levels = c("<500",
                 "500-999",
                 "1000-1499",
                 "1500-1999",
                 ">=2000")
    )
  ) 

paper_cohort = paper_cohort_pre %>%
  select(
    patientid,
    site_name,
    infant_dob_dt,
    mode_of_delivery_cd,
    ces_for_FHT,
    ces_for_FHT_char,
    race_eth,
    insurance_char,
    IA_at_admission,
    ppreg_chronic_hyper_b,
    gest_hyper_b,
    ppreg_diabetes_b,
    gest_diabetes_b,
    covid_status,
    AMA,
    BMI_cat,
    induction_status,
    prenatal_care,
    prosp_region,
    teaching_status,
    nicu_status,
    birth_vol_22
  )
