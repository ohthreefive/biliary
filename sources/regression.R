# ── Cox proportional hazards regression ───────────────────────────────────────
#
# Restricted to malignant obstruction cohort only.
# Outcome: Surv(Survival, Alive == "No")
#
# exponentiate = TRUE throughout — results presented as hazard ratios (HR)
# rather than log(HR). HR > 1 = increased hazard of death; HR < 1 = protective.

# Select only the variables entering the models; drop 2 rows with missing CRP
# so all tables and the forest plot use the same consistent dataset.
# Character variables converted to factors — required by coxph.
biliary_cox_complete <- biliary_malignant |>
  select(Survival, Alive, ageProcedure, Sex, Site, ERCP,
         Hb, WCC, Plts, PT, Urea, Cr, Bili, Alb, CRP) |>
  na.omit() |>
  mutate(across(where(is.character), as.factor))

# Labels for Cox models — subset of var_labels, excluding Weight
cox_labels <- list(
  ageProcedure ~ "Age",
  Hb           ~ "Haemoglobin (g/L)",
  WCC          ~ "White cell count (×10⁹/L)",
  Plts         ~ "Platelets (×10⁹/L)",
  PT           ~ "Prothrombin time (seconds)",
  Urea         ~ "Urea (mmol/L)",
  Cr           ~ "Creatinine (µmol/L)",
  Bili         ~ "Bilirubin (µmol/L)",
  Alb          ~ "Albumin (g/L)",
  CRP          ~ "CRP (mg/L)"
)

# ── Univariable Cox regression ────────────────────────────────────────────────

# tbl_uvregression() fits a separate Cox model for each variable automatically
tbl_uv <- biliary_cox_complete |>
  tbl_uvregression(
    method       = coxph,
    y            = Surv(Survival, Alive == "No"),
    exponentiate = TRUE,
    label        = cox_labels,
    hide_n       = TRUE
  ) |>
  bold_p() |>                     # bold significant p-values
  bold_labels()

tbl_uv

# ── Multivariable Cox regression ──────────────────────────────────────────────

# All variables entered simultaneously
fit_cox <- coxph(
  Surv(Survival, Alive == "No") ~ ageProcedure + Sex + Site + ERCP +
    Hb + WCC + Plts + PT + Urea + Cr + Bili + Alb + CRP,
  data = biliary_cox_complete
)

tbl_mv <- tbl_regression(
  fit_cox,
  exponentiate = TRUE,
  label        = cox_labels
) |>
  bold_p() |>
  bold_labels()

tbl_mv

# ── Merged univariable + multivariable table ──────────────────────────────────

# Standard journal format: univariable and multivariable results side by side
tbl_cox_merged <- tbl_merge(
  tbls        = list(tbl_uv, tbl_mv),
  tab_spanner = c("**Univariable**", "**Multivariable**")
) |>
  as_gt() |>
  tab_header(title = "Cox proportional hazards regression (malignant obstruction)") |>
  tab_options(table.font.names = "Helvetica")

tbl_cox_merged

# ── Forest plot ───────────────────────────────────────────────────────────────

# ggforest() has a known bug with this dataset; instead we build the forest plot
# manually from broom::tidy(), which gives us more control over appearance.

cox_forest_data <- broom::tidy(fit_cox, exponentiate = TRUE, conf.int = TRUE) |>
  # Apply readable labels to match the regression tables
  mutate(term = recode(term,
    "ageProcedure"  = "Age",
    "SexWoman"      = "Sex: Woman",
    "SiteQEUH"      = "Hospital: QEUH",
    "ERCPYes"       = "Prior ERCP: Yes",
    "Hb"            = "Haemoglobin (g/L)",
    "WCC"           = "White cell count (×10^9/L)",
    "Plts"          = "Platelets (×10^9/L)",
    "PT"            = "Prothrombin time (seconds)",
    "Urea"          = "Urea (mmol/L)",
    "Cr"            = "Creatinine (µmol/L)",
    "Bili"          = "Bilirubin (µmol/L)",
    "Alb"           = "Albumin (g/L)",
    "CRP"           = "CRP (mg/L)"
  ))

chart_cox_forest <- ggplot(cox_forest_data, aes(x = estimate, y = term)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, orientation = "y") +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  scale_x_log10() +      # log scale so CIs are symmetrical around HR
  labs(
    title = "Multivariable Cox regression — malignant obstruction",
    x     = "Hazard ratio (95% CI)",
    y     = NULL
  ) +
  theme_biliary

chart_cox_forest
