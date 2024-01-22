
# bar graph ---------------------------------------------------------------

bar_graph = paper_cohort_pre %>%
  group_by(race_eth) %>%
  summarize(ces_for_FHT_rate = sum(ces_for_FHT) / sum(birth)) %>%
  ggplot(aes(y = race_eth, x = ces_for_FHT_rate)) +
  geom_bar(stat = 'identity', fill = OBI.color::prim_med_blue()) +
  geom_text(aes(label = scales::percent(ces_for_FHT_rate, accuracy = 0.1)), hjust = 0, size = 6, color = OBI.color::prim_dark_blue()) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 0.22)) +
  labs(title = "NTSV Births with an Unplanned Cesarean for Abnormal/Indeterminate FHT by Race-Ethnicity") +
  theme_obi() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(size = 15),
    plot.title.position = 'plot',
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.background = element_rect(fill = "white",
                                   color = "white"),
    panel.background = element_rect(fill = "white",
                                    color = "white")
  )

# ggsave(bar_graph, filename = paste0(output_path, "Figures/bar_graph.png"), width = 9, height = 5, units = "in")

# errorbar plot -----------------------------------------------------------

mm_df = tbl_regression(model_mixed, exponentiate = T) %>%
  as.data.frame() %>%
  janitor::clean_names() %>%
  filter(
    characteristic %in% c(
      "White",
      "American Indian or Alaskan Native",
      "Asian or Pacific Islander",
      "Black",
      "Hispanic",
      "More than one race",
      "Race and/or ethnicity unknown"
    )
  ) %>% 
  separate_wider_delim(cols = "x95_percent_ci", delim = ",", names = c("CI_low", "CI_high")) %>% 
  mutate(across(c(exp_beta, CI_low, CI_high), as.numeric))

OR_plot = mm_df %>%
  drop_na() %>%
  ggplot(aes(y = characteristic)) +
  geom_vline(aes(xintercept = 1), linetype = "dashed", linewidth = 0.6) +
  geom_point(aes(x = exp_beta), size = 2.5) +
  geom_errorbarh(aes(xmin = as.numeric(CI_low), xmax = as.numeric(CI_high)),
                 linewidth = 0.8,
                 height = 0.3) +
  labs(title = "Adjusted Odds of Delivering via Unplanned Cesarean for Abnormal/Indeterminate FHT by Race-Ethnicity",
       caption = "White is the reference group and is not included in the plot") +
  theme_minimal() +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    plot.title.position = 'plot',
    plot.caption = element_text(hjust = 0, size = 10),
    plot.caption.position = 'plot',
    plot.background = element_rect(fill = "white",
                                   color = "white"),
    panel.background = element_rect(fill = "white",
                                    color = "white")
  )

# ggsave(OR_plot, filename = paste0(output_path, "Figures/OR_plot.png"), width = 9, height = 5, units = "in")
 