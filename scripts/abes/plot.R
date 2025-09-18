# 📝 Section 2: Coverage Analysis
library(viridis)
coverage_summary <- df_all %>%
  summarise(
    across(all_of(vaccine_order),
      list(
        coverage_rate = ~ mean(!is.na(.), na.rm = TRUE) * 100,
        n = ~ sum(!is.na(.))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("vaccine", ".value"),
    names_pattern = "(.*)_(coverage_rate|n)"
  ) %>%
  mutate(
    vaccine = factor(vaccine, levels = as.character(unlist(vaccine_groups)))
  )

p_coverage <- ggplot(coverage_summary, aes(x = vaccine, y = coverage_rate)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(
      label = paste0(sprintf("%.1f", coverage_rate), "% (n=", n, ")"),
      hjust = ifelse(coverage_rate > 10, 1.1, -0.1)
    ),
    color = "black", size = 3
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "Immunization Coverage (%)",
    y = "Coverage %",
    x = "Vaccine"
  ) +
  theme_pubclean()

df_cov_by_year <- df_all %>%
  mutate(birth_year = lubridate::year(`Tanggal Lahir Anak`)) %>%
  group_by(birth_year) %>%
  summarise(
    across(
      all_of(vaccine_order),
      ~ mean(!is.na(.), na.rm = TRUE) * 100,
      .names = "{.col}_cov"
    ),
    n = n(),
    .groups = "drop"
  )

df_cov_long <- df_cov_by_year %>%
  pivot_longer(
    cols = ends_with("_cov"),
    names_to = "vaccine",
    values_to = "coverage"
  ) %>%
  mutate(
    vaccine = gsub("_cov$", "", vaccine),
    vaccine = factor(vaccine, levels = as.character(unlist(vaccine_groups))),
  )

df_cov_long_complete <- df_cov_long %>%
  complete(birth_year, vaccine, fill = list(coverage = 0)) %>%
  mutate(
    coverage_group = ifelse(coverage > 50, "Above 50%", "0–50%")
  )

df_labels <- df_cov_long_complete %>%
  group_by(vaccine, coverage_group) %>%
  slice_min(abs(birth_year - 2024)) %>%
  ungroup()

library(ggrepel)
p_cov_dob <- ggplot(df_cov_long_complete, aes(x = birth_year, y = coverage, color = vaccine)) +
  geom_line(size = 1) +
  geom_point(size = 2, alpha = 0.7) +
  geom_text_repel(
    data = df_labels,
    aes(label = vaccine),
    hjust = 0,
    nudge_x = 0.3,
    size = 3,
    show.legend = FALSE
  ) +
  scale_color_viridis_d(option = "turbo") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(
    min(df_cov_long$birth_year, na.rm = TRUE),
    max(df_cov_long$birth_year, na.rm = TRUE), 1
  )) +
  labs(
    title = "Vaccine Coverage by Birth Year",
    subtitle = "",
    x = "Birth Year",
    y = "Coverage (%)",
    color = "Vaccine"
  ) +
  theme_pubclean() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  guides(color = guide_legend(ncol = 6))

# 📝 Section 3: Timeliness of Vaccination
df_timeliness <- df_all %>%
  mutate(across(all_of(vaccine_order),
    ~ as.numeric(difftime(., `Tanggal Lahir Anak`, units = "days")) / 30.44,
    .names = "{.col}_age"
  ))

df_timeliness %>%
  summarise(across(ends_with("_age"),
    ~ quantile(., probs = c(0.25, 0.5, 0.75), na.rm = TRUE),
    .names = "{.col}_{.fn}"
  ))

df_long_age <- df_timeliness %>%
  select(ends_with("_age")) %>%
  pivot_longer(everything(), names_to = "vaccine", values_to = "age_months") %>%
  mutate(
    vaccine = gsub("_age$", "", vaccine),
    vaccine = factor(vaccine, levels = as.character(unlist(vaccine_groups)))
  )

p_timelines <- ggplot(df_long_age, aes(x = vaccine, y = age_months)) +
  geom_boxplot(outlier.alpha = 0.1, fill = "lightblue") +
  coord_flip() +
  labs(title = "Age at Vaccination (Years)", y = "Age (years)", x = "Vaccine") +
  scale_y_continuous(
    breaks = seq(0, max(df_long_age$age_months, na.rm = TRUE), by = 12),
    labels = function(x) paste0(x / 12, " yr") # 👈 here
  ) +
  theme_pubclean()


# 📝 Section 4: Inequities & Associations
df_region <- df_all %>%
  mutate(birth_year = lubridate::year(`Tanggal Lahir Anak`)) %>%
  group_by(Kecamatan, birth_year) %>% # assumes a column `region`
  summarise(
    across(all_of(vaccine_order),
      ~ mean(!is.na(.), na.rm = TRUE) * 100,
      .names = "{.col}_cov"
    ),
    n = n()
  )

df_long_region <- df_region %>%
  pivot_longer(ends_with("_cov"), names_to = "vaccine", values_to = "coverage") %>%
  mutate(
    vaccine = gsub("_cov$", "", vaccine),
    vaccine = factor(vaccine, levels = vaccine_order)
  )

# CHECK ==========>>>>>>>>
p_cov_region <- ggplot(df_long_region, aes(x = birth_year, y = coverage, color = Kecamatan)) +
  geom_line() +
  facet_wrap(~vaccine) +
  labs(title = "Vaccine Coverage by Region", y = "% Coverage", x = "Birth Year") +
  theme_pubclean()

# 📝 Section 6: Full Immunization Coverage
df_all <- df_all %>%
  mutate(
    FIC = ifelse(rowSums(is.na(select(., all_of(required_vaccines)))) == 0, 1, 0)
  )

fic_rate <- mean(df_all$FIC, na.rm = TRUE) * 100
fic_rate

df_fic_year <- df_all %>%
  mutate(birth_year = lubridate::year(`Tanggal Lahir Anak`)) %>%
  group_by(birth_year) %>%
  summarise(
    FIC_rate = mean(FIC, na.rm = TRUE) * 100,
    n = n()
  )

p_fic_line <- ggplot(df_fic_year, aes(x = birth_year, y = FIC_rate)) +
  geom_line(color = "darkgreen", linewidth = 1.2) +
  geom_point(size = 2) +
  theme_pubclean() +
  labs(
    title = "Full Immunization Coverage Over Time",
    y = "% Fully Immunized Children", x = "Birth Year"
  )

df_fic_region <- df_all %>%
  group_by(Kecamatan) %>%
  summarise(FIC_rate = mean(FIC, na.rm = TRUE) * 100, n = n()) %>%
  arrange(desc(FIC_rate))

p_fic_region <- ggplot(df_fic_region, aes(
  x = reorder(Kecamatan, FIC_rate), y = FIC_rate, fill = Kecamatan
)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(
      label = paste0(sprintf("%.2f%%", FIC_rate), " (n=", n, ")"),
      hjust = ifelse(FIC_rate > 3, 1.1, -0.1)
    ),
    color = "black", size = 3.5
  ) +
  coord_flip(clip = "off") + # allow labels outside bars
  theme_minimal() +
  labs(
    title = "Full Immunization Coverage by Region",
    x = "Region",
    y = "% Fully Immunized Children"
  )

# 📝 Section 7: Missed Opportunities & Partial Immunization

df_all <- df_all %>%
  mutate(vaccine_count = rowSums(!is.na(select(., all_of(vaccine_order)))))

df_count <- df_all %>%
  count(vaccine_count) %>%
  mutate(prop = n / sum(n) * 100)

p_dist_vac <- ggplot(df_count, aes(x = factor(vaccine_count), y = prop)) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(
    aes(
      label = paste0(sprintf("%.2f%%", prop), " (n=", n, ")"),
      hjust = ifelse(prop > 10, 1.1, -0.1)
    ),
    size = 3.5,
    color = "black"
  ) +
  coord_flip(clip = "off") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)), # add space for labels
    labels = scales::percent_format(scale = 1)
  ) +
  labs(
    title = "Distribution of Vaccines Received per Child",
    x = "Number of Vaccines Received",
    y = "Percentage of Children"
  ) +
  theme_pubclean() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(), # remove horizontal gridlines
    panel.grid.major.x = element_line(color = "grey85", linetype = "dashed")
  )

df_missed <- df_all %>%
  group_by(Kecamatan) %>%
  summarise(
    avg_vaccine_count = mean(vaccine_count, na.rm = TRUE),
    missed_opportunities = mean(FIC == 0 & vaccine_count > 0, na.rm = TRUE) * 100,
    n = n()
  )

df_long_completion <- df_all %>%
  select(Kecamatan, all_of(vaccine_order)) %>%
  pivot_longer(-Kecamatan, names_to = "vaccine", values_to = "status") %>%
  mutate(
    status = ifelse(is.na(status), 0, 1),
    vaccine = factor(vaccine, levels = vaccine_order)
  )

p_heatmap <- ggplot(df_long_completion, aes(x = vaccine, y = Kecamatan, fill = factor(status))) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = c("0" = "lightgrey", "1" = "darkgreen"),
    name = "Received"
  ) +
  theme_pubclean() +
  labs(
    title = "Heatmap of Vaccine Completion by Region",
    x = "Vaccine", y = "Region"
  )

idl <- make_idl_completion_summary(
  df_all, required_vaccines, "dob"
)

age_table <- make_vaccine_age_table(
  df_all, vaccine_groups, "dob"
)

## $by_group, $by_idl, $by_dose
age_plots <- plot_vaccine_age(age_table)

p_idl_trend <- ggplot(
  trend_data(df_all),
  aes(x = year_birth, y = percent, fill = idl_status)
) +
  geom_col(position = position_dodge()) +
  geom_text(
    aes(
      label = paste0(round(percent, 1), "%\n(n=", n, ")"),
      vjust = ifelse(percent > 10, 1.1, -0.1)
    ),
    color = "black", size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  scale_fill_manual(
    values = c(
      "Lengkap" = "#4CAF50",
      "Tidak" = "#FFC107"
    )
  ) +
  labs(
    title = "Tren Cakupan IDL Anak per Tahun Lahir dan Status",
    x = "Tahun Lahir Anak",
    y = "Persentase Anak per Status",
    fill = "Status"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(family = "Times", size = 10)
  )

p_dropoff <- ggplot(
  dropoff_data(df_all, as.character(unlist(required_vaccines))),
  aes(x = Vaccine, y = Rate, fill = Status)
) +
  geom_col(position = position_dodge(width = 0.7)) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Completion" = "#1f77b4", # blue
      "Dropoff"    = "#FFC107" # yellow
    )
  ) +
  labs(
    title = "Vaccine Completion vs Drop-off",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    text = element_text(family = "Times", size = 10)
  )
