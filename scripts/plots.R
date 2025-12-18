# ==>> Call from 04_vaccine_trends
# ------------------------------------------------------------
p_idl_trend <- ggplot(
  trend_data(all_data),
  aes(x = year_birth, y = percent, fill = idl_status)
) +
  geom_col(position = position_dodge()) +
  geom_text(
    aes(label = paste0(round(percent, 1), "%\n(n=", n, ")")),
    vjust = -0.5,
    size = 3
  ) +
  # geom_hline(yintercept = 90, linetype = "dashed", color = "red") +
  # annotate("text",
  #   x = 1, y = 92, label = "Target 90%",
  #   hjust = 0, color = "red", size = 3.5
  # ) +
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
  dropoff_data(all_data, as.character(unlist(required_vaccines))),
  aes(x = Vaccine, y = Rate, fill = Status)
) +
  geom_col(position = position_dodge(width = 0.7)) +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "Completion" = "#1f77b4", # blue
      "Dropoff" = "#d62728" # red
    )
  ) +
  labs(
    title = "Vaccine Completion vs Drop-off (Ordered by Drop-off)",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    text = element_text(family = "Times", size = 10)
  )

p_chi_sq <- ggplot(
  chi_sq_data(all_data, chi_test),
  aes(
    x = reorder(VaccineGroup, Proportion),
    y = Proportion,
    fill = Status
  )
) +
  geom_col(position = position_dodge()) +
  geom_text(
    aes(label = paste0(round(Proportion, 1), "%\n(n=", Count, ")")),
    vjust = -0.5,
    size = 3
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  scale_fill_manual(
    values = c(
      "Completed" = "#4CAF50",
      "Missed" = "#FFC107"
    )
  ) +
  labs(
    title = "Proportion of Children per Vaccine Group",
    x = "Vaccine Group",
    y = "Proportion of Children"
  ) +
  theme_pubclean() +
  theme(
    legend.position = "right",
    text = element_text(family = "Times", size = 10)
  )

# ==>> Call from 06_descriptive
# ------------------------------------------------------------
## $by_group, $by_idl, $by_dose
age_plots <- plot_vaccine_age(age_table)

## $summary, $plot
p_percent <- plot_percent(all_data)

## $summary, $plot
p_biner <- plot_biner(all_data)
