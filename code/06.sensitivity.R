
library(glmmTMB)
library(gt)
library(gtsummary)

# race-ethnicity and overall unplanned ces risk ---------------------------

cohort_ids = paper_cohort %>%
  select(patientid) %>%
  pull()

overall_ces_dt = obi_cohort %>%
  mutate(race_eth = factor(
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
      "More then one race",
      "Race and/or ethnicity unknown"
    )
  )) %>%
  filter(patientid %in% c(cohort_ids)) %>% 
  select(patientid, cesarean, race_eth)

ces_model = glmmTMB(
  formula = cesarean ~ race_eth,
  data = overall_ces_dt,
  family = binomial
)

table_s1 = tbl_regression(ces_model,
                          exponentiate = T,
                          label = list(race_eth ~ "Race-ethnicity")) %>%
  modify_table_styling(column = estimate,
                       label = "**OR**") %>%
  modify_footnote(estimate ~ "Odds Ratio") %>%
  as_gt() %>%
  tab_header(title = "Table S1. Relationship between Race-Ethnicity and Unplanned Cesarean")

# gtsave(table_s1, paste0(output_path, "Tables/table_s1.docx"))


# race-ethnicity and unplanned ces for FHT in Detroit metro ---------------

detroit = paper_cohort %>% 
  filter(prosp_region == "Detroit Metro Prosperity Region")

detroit_model = glm(ces_for_FHT ~ race_eth,
                    data = detroit,
                    family = binomial)

## crude model first

detroit_crude_tbl = tbl_regression(
  detroit_model,
  exponentiate = T,
  label = list(race_eth = "Race-ethnicity")
) %>%
  modify_table_styling(column = estimate,
                       label = "**OR**")

## try adjusted model

mm_detroit = glmmTMB(
  formula = ces_for_FHT ~ race_eth + 
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
    teaching_status +
    nicu_status +
    birth_vol_22 + 
    (1 | site_name),
  data = detroit,
  family = binomial
)

mm_detroit_tbl = tbl_regression(
  mm_detroit,
  exponentiate = T,
  include = race_eth,
  label = list(race_eth ~ "Race-ethnicity")
) %>%
  modify_table_styling(column = estimate,
                       label = "**OR**")

table_s2 = tbl_merge(
  tbls = list(detroit_crude_tbl, mm_detroit_tbl),
  tab_spanner = c("Crude", "Adjusted")
) %>%
  as_gt() %>%
  tab_header(title = "Table S2. Crude and adjusted odds of Cesarean for FHT among births in Detroit Metro prosperity region")

# gtsave(table_s2, paste0(output_path, "Tables/table_s2.docx"))


# limit to unplanned cesareans --------------------------------------------

unplanned_ces = paper_cohort_pre %>%
  filter(planned_mode_of_delivery_cd == 1,
         mode_of_delivery_cd == 4,
         patientid %in% cohort_ids)

mm_unplanned_ces = glmmTMB(
  formula = ces_for_FHT ~ race_eth +
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
    teaching_status +
    nicu_status +
    birth_vol_22 +
    (1 | site_name),
  data = unplanned_ces,
  family = binomial
)

table_s3 = tbl_regression(
  mm_unplanned_ces,
  exponentiate = T,
  include = race_eth,
  label = list(race_eth ~ "Race-ethnicity")
) %>%
  modify_table_styling(column = estimate,
                       label = "**OR**") %>%
  modify_footnote(estimate ~ "Odds Ratio") %>% 
  as_gt() %>% 
  tab_header(title = "Table S3: Adjusted odds of cesarean for FHT among unplanned Cesarean births")

# gtsave(table_s3, paste0(output_path, "Tables/table_s3.docx"))
