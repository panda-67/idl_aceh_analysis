# Pre ------
source(here("scripts/abes/pre.R"))

sapply(df_all, class)

# 📝 Section 1: Data Quality & Completeness
df_all %>%
  mutate(HB0_count = !is.na(`HB-0`)) %>%
  summarise(
    total_HB0 = sum(HB0_count, na.rm = TRUE), # number vaccinated
    total = n() # total children
  )

df_all %>%
  summarise(
    total = n(),
    dob_match = sum(`Tanggal Lahir Anak` == dob, na.rm = TRUE),
    dob_diff = sum(`Tanggal Lahir Anak` != dob, na.rm = TRUE),
    sum_tgl = sum(!is.na(`Tanggal Lahir Anak`)),
    sum_dob = sum(!is.na(dob))
  )

df_all %>%
  mutate(
    OPV_count = rowSums(!is.na(select(., `OPV 1`, `OPV 2`, `OPV 3`, `OPV 4`)))
  ) %>%
  summarise(
    mismatch = sum(OPV_count != OPV_doses, na.rm = TRUE),
    total = n()
  )

library(naniar)

df_all %>%
  miss_var_summary() %>%
  arrange(desc(pct_miss))

p_miss <- vis_miss(df_all %>% select(any_of(vaccine_order))) +
  labs(title = "Missingness Pattern (sample vaccines)")

# chisq_results <- lapply(vaccine_order, function(vac) {
#   tab <- table(df_all[[vac]], df_all$Kecamatan, useNA = "no")
#   chisq.test(tab)
# })
# names(chisq_results) <- vaccine_order
#
# model <- glm(!is.na(BCG) ~ Kecamatan + `jenis kelamin`,
#   data = df_all, family = binomial
# )
# summary(model)

# Plot -----
source(here("scripts/abes/plot.R"))
# Post -----
source(here("scripts/abes/post.R"))
# ----------
# ----------
