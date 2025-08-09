## 0. by_group, 1. by_idl, 2. by_dose
plots <- plot_vaccine_age(age_table)

p_idl_trend <- ggplot(
  trend_data(all_data),
  aes(x = year_birth, y = percent, fill = idl_status)
) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percent, 1), "%\n(n=", n, ")")),
    vjust = -0.5, size = 3
  ) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "red") +
  annotate("text",
    x = 1, y = 92, label = "Target 90%",
    hjust = 0, color = "red", size = 3.5
  ) +
  facet_wrap(~idl_status, ncol = 1) +
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
    y = "Persentase Anak per Status"
  ) +
  theme_pubclean() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
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
      "Dropoff"    = "#d62728" # red
    )
  ) +
  labs(
    title = "Vaccine Completion vs Drop-off (Ordered by Drop-off)",
    x = NULL,
    y = "Percentage (%)"
  ) +
  theme_pubclean()
