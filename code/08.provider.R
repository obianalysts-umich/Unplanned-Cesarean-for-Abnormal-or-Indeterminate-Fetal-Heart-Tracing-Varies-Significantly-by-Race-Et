library(gtsummary)
library(glmmTMB)

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
  # only OBGYN and CNM
  filter(specialty_desc %in% c("CNM", "OBGYN"))

# provider type cohorts ---------------------------------------------------

midwife_cohort <- paper_cohort_prov |> 
  filter(specialty_desc == "CNM")

OBGYN_cohort <- paper_cohort_prov |> 
  filter(specialty_desc == "OBGYN")

# descriptive -------------------------------------------------------------

midwife_cohort |>
  select(race_eth, ces_for_FHT) |>
  gtsummary::tbl_summary(by = race_eth)

OBGYN_cohort |>
  select(race_eth, ces_for_FHT) |>
  gtsummary::tbl_summary(by = race_eth)

# tests -------------------------------------------------------------------

## is the risk of cesarean for FHT higher when delivering with an OBGYN vs. midwife?

prov_model <- glmmTMB(
  formula = ces_for_FHT ~ race_eth +
    specialty_desc +
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

broom.mixed::tidy(prov_model)

## is the risk of ces for FHT different between race-ethnicity groups for midwives and OBGYNs?
fisher.test(midwife_cohort$race_eth, midwife_cohort$ces_for_FHT, simulate.p.value = T)

chisq.test(OBGYN_cohort$race_eth, OBGYN_cohort$ces_for_FHT)
