
library(tidyverse)
library(obstinit)
library(lme4)
library(glmmTMB)
library(marginaleffects)
library(gt)
library(gtsummary)

# crude -------------------------------------------------------------------

var_list = c(
  "race_eth",
  "insurance_char",
  "IA_at_admission",
  "ppreg_chronic_hyper_b",
  "gest_hyper_b",
  "ppreg_diabetes_b",
  "gest_diabetes_b",
  "covid_status",
  "AMA",
  "BMI_cat",
  "induction_status",
  "prenatal_care",
  "prosp_region",
  "teaching_status",
  "nicu_status",
  "birth_vol_22"
)

## loop over to look ------------------------------------------------------

for(i in var_list) {
  print(glm(
    formula = ces_for_FHT ~ eval(parse(text = i)),
    data = paper_cohort,
    family = binomial
  ))
} # all converge - good

## assign -----------------------------------------------------------------

for(i in var_list) {
  assign(
    paste0(i, "_crude_model"),
    glm(
      formula = ces_for_FHT ~ eval(parse(text = i)),
      data = paper_cohort,
      family = binomial
    )
  )
  
  print(i)
  
  print(eval(parse(text = paste0(i, "_crude_model"))))
  
  Sys.sleep(5)
}

## formatted table --------------------------------------------------------

crude_tbl = paper_cohort %>%
  select(ces_for_FHT, all_of(var_list)) %>%
  tbl_uvregression(
    method = glm,
    method.args = list(family = binomial),
    y = ces_for_FHT,
    exponentiate = T,
    label = list(
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
    hide_n = T
  )

# adjusted ----------------------------------------------------------------

model_mixed = glmmTMB(
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
    prosp_region +
    teaching_status +
    nicu_status +
    birth_vol_22 + 
    (1 | site_name),
  data = paper_cohort,
  family = binomial
)

regression_tbl = tbl_regression(
  model_mixed,
  exponentiate = TRUE,
  label = list(
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
  )
) %>% 
  modify_table_styling(column = estimate,
                       label = "**OR**")

# merge crude and adjusted ------------------------------------------------

tbl_all = tbl_merge(
  tbls = list(crude_tbl, regression_tbl),
  tab_spanner = c("Crude", "Adjusted")
) %>%
  as_gt() %>% 
  tab_header(title = "Table 3. Crude and adjusted odds of Cesarean for FHT")

# output ------------------------------------------------------------------

# gtsave(tbl_all, paste0(output_path, "Tables/table_3.docx"))
