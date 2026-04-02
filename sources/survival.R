# ── Survival analysis ─────────────────────────────────────────────────────────
#
# Survival time  = Survival column (days since procedure)
# Event          = Alive == "No" (1 = died, 0 = censored/still alive)
#
# Analyses:
#   1. Overall survival — all patients
#   2. By diagnosis     — all patients (Benign vs Malignant)
#   3–5. Malignant only — by Sex, Hospital, ERCP

# Subset for malignant patients (used in analyses 3–5)
biliary_malignant <- biliary |> filter(Diagnosis == "Malignant")

# ── 1. Overall survival ───────────────────────────────────────────────────────

fit_overall <- survfit(
  Surv(Survival, Alive == "No") ~ 1,   # ~ 1 means a single curve (no grouping)
  data = biliary
)

# Extract median survival to use as x-axis limit in subsequent plots.
# surv_median() returns a data frame; [1, "median"] pulls the single value.
# This updates automatically when new data is added each year.
median_overall <- surv_median(fit_overall)[1, "median"]

surv_overall <- ggsurvplot(
  fit_overall,
  data             = biliary,
  conf.int         = TRUE,
  risk.table       = TRUE,
  surv.median.line = "hv",   # horizontal + vertical lines at median survival
  xlab             = "Days",
  ylab             = "Survival probability",
  title            = "Overall survival",
  ggtheme          = theme_minimal()
)

surv_overall

# ── 2. Survival by diagnosis ──────────────────────────────────────────────────

fit_diagnosis <- survfit(
  Surv(Survival, Alive == "No") ~ Diagnosis,
  data = biliary
)

surv_diagnosis <- ggsurvplot(
  fit_diagnosis,
  data             = biliary,
  conf.int         = TRUE,
  risk.table       = TRUE,
  pval             = TRUE,
  legend.title     = "Diagnosis",
  xlim             = c(0, median_overall),
  break.time.by    = 25,
  xlab             = "Days",
  ylab             = "Survival probability",
  title            = "Survival by diagnosis",
  ggtheme          = theme_minimal()
)

surv_diagnosis

# ── Malignant cohort only (analyses 3–5) ──────────────────────────────────────

# ── 3. Survival by sex (malignant) ────────────────────────────────────────────

fit_sex <- survfit(
  Surv(Survival, Alive == "No") ~ Sex,
  data = biliary_malignant
)

surv_sex <- ggsurvplot(
  fit_sex,
  data          = biliary_malignant,
  conf.int      = TRUE,
  risk.table    = TRUE,
  pval          = TRUE,
  legend.title  = "Sex",
  xlim          = c(0, median_overall),
  break.time.by = 25,
  xlab          = "Days",
  ylab          = "Survival probability",
  title         = "Survival by sex (malignant obstruction)",
  ggtheme       = theme_minimal()
)

surv_sex

# ── 4. Survival by hospital (malignant) ───────────────────────────────────────

fit_hospital <- survfit(
  Surv(Survival, Alive == "No") ~ Site,
  data = biliary_malignant
)

surv_hospital <- ggsurvplot(
  fit_hospital,
  data          = biliary_malignant,
  conf.int      = TRUE,
  risk.table    = TRUE,
  pval          = TRUE,
  legend.title  = "Hospital",
  xlim          = c(0, median_overall),
  break.time.by = 25,
  xlab          = "Days",
  ylab          = "Survival probability",
  title         = "Survival by hospital (malignant obstruction)",
  ggtheme       = theme_minimal()
)

surv_hospital

# ── 5. Survival by ERCP (malignant) ───────────────────────────────────────────

fit_ercp <- survfit(
  Surv(Survival, Alive == "No") ~ ERCP,
  data = biliary_malignant
)

surv_ercp <- ggsurvplot(
  fit_ercp,
  data          = biliary_malignant,
  conf.int      = TRUE,
  risk.table    = TRUE,
  pval          = TRUE,
  legend.title  = "Prior ERCP",
  xlim          = c(0, median_overall),
  break.time.by = 25,
  xlab          = "Days",
  ylab          = "Survival probability",
  title         = "Survival by prior ERCP (malignant obstruction)",
  ggtheme       = theme_minimal()
)

surv_ercp
