
# prosp reg & Ces for FHT -------------------------------------------------

paper_cohort %>% 
  group_by(prosp_region) %>% 
  summarize(prop_ces_FHT = mean(ces_for_FHT, na.rm = T))

chisq.test(paper_cohort$prosp_region, paper_cohort$ces_for_FHT_char)

# prosp reg & race-ethnicity ----------------------------------------------

paper_cohort %>% 
  group_by(prosp_region, race_eth) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = race_eth, values_from = n)

fisher.test(paper_cohort$prosp_region, paper_cohort$race_eth, simulate.p.value = T)

# race-ethnicity & Ces for FHT strat by reg --------------------------------

paper_cohort %>% 
  group_by(prosp_region, race_eth) %>% 
  summarize(ces_for_FHT_prop = mean(ces_for_FHT, na.rm = T)) %>% 
  pivot_wider(names_from = race_eth, values_from = ces_for_FHT_prop)

chisq.test(paper_cohort$prosp_region, paper_cohort$ces_for_FHT_char)
