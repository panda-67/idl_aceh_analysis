# 0. Pre
# ----------------------------
source(here::here("scripts/pre_analysis.R"))

names(all_data)
all_data <- all_data %>% filter(kecamatan != "kbj")

# 1. Descriptive Analysis
idl <- make_idl_completion_summary(all_data, required_vaccines)
age_table <- make_vaccine_age_table(all_data, vaccine_groups)

# 2. Inferial Analysis
# ----------------------------

# ===>>> 2.1 Continuos <<<===
assump <- check_assumptions(all_data)
print(assump$normality)

stat_cont <- cont_test(assump, all_data)

# ===>>> 2.2 Binary <<<===
stat_biner <- biner_test(
  all_data,
  status_col = "idl_status", region_col = "region", district_col = "district"
)

# ===>>> 2.3 Chi Square <<<===
chi_test <- run_vaccine_completion_chisq(
  data = all_data,
  intended_cols = vaccine_groups
)

# 3. Post
# ----------------------------
source(here("scripts/plots.R"))
source(here("scripts/post_analysis.R"))

# ---
# ---
