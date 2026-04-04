# ── Survival analysis ─────────────────────────────────────────────────────────
#
# Survival time  = Survival column (days since procedure)
# Event          = Alive == "No" (1 = died, 0 = censored/still alive)
#
# Analyses:
#   1. Overall survival — all patients
#   2. By diagnosis     — all patients (Benign vs Malignant)
#   3–5. Malignant only — by Sex, Hospital, ERCP

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
  data              = biliary,
  conf.int          = TRUE,
  censor.shape      = 124,
  risk.table        = "nrisk_cumevents",   # show both number at risk and events
  surv.median.line  = "hv",                # horizontal + vertical lines at median
  surv.scale        = "percent",           # y axis as 0–100%
  xscale            = "d_y",              # convert days to years on x axis
  break.time.by     = 730.5,             # one break per year
  xlab              = "Time (years)",
  ylab              = "Survival (%)",
  title             = "Overall survival after biliary procedure",
  legend            = "none",
  legend.title      = "",
  legend.labs       = "",
  palette           = "lancet",
  ggtheme           = theme_biliary
)

surv_overall

# ── 2. Survival by diagnosis ──────────────────────────────────────────────────

fit_diagnosis <- survfit(
  Surv(Survival, Alive == "No") ~ Diagnosis,
  data = biliary
)

surv_diagnosis <- ggsurvplot(
  fit_diagnosis,
  data          = biliary,
  conf.int      = TRUE,
  censor.shape  = 124,
  risk.table    = "nrisk_cumevents",
  pval          = FALSE,
  surv.scale    = "percent",
  legend        = "none",
  legend.labs   = c("Benign", "Malignant"),   # remove "Diagnosis=" prefix
  legend.title  = "",
  xlim          = c(0, median_overall),
  break.time.by = 30,
  xlab          = "Days",
  ylab          = "Survival (%)",
  title         = "Survival by diagnosis",
  palette       = "lancet",
  ggtheme       = theme_biliary
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
  censor.shape  = 124,
  risk.table    = "nrisk_cumevents",
  pval          = FALSE,
  surv.scale    = "percent",
  legend        = "none",
  legend.labs   = c("Man", "Woman"),   # remove "Sex=" prefix
  legend.title  = "",
  xlim          = c(0, median_overall),
  break.time.by = 30,
  xlab          = "Days",
  ylab          = "Survival (%)",
  title         = "Survival by sex (malignant obstruction)",
  palette       = "lancet",
  ggtheme       = theme_biliary
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
  censor.shape  = 124,
  risk.table    = "nrisk_cumevents",
  pval          = FALSE,
  surv.scale    = "percent",
  legend        = "none",
  legend.labs   = c("GRI", "QEUH"),   # remove "Site=" prefix
  legend.title  = "",
  xlim          = c(0, median_overall),
  break.time.by = 30,
  xlab          = "Days",
  ylab          = "Survival (%)",
  title         = "Survival by hospital (malignant obstruction)",
  palette       = "lancet",
  ggtheme       = theme_biliary
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
  censor.shape  = 124,
  risk.table    = "nrisk_cumevents",
  pval          = FALSE,
  surv.scale    = "percent",
  legend        = "none",
  legend.labs   = c("No", "Yes"),   # remove "ERCP=" prefix
  legend.title  = "",
  xlim          = c(0, median_overall),
  break.time.by = 30,
  xlab          = "Days",
  ylab          = "Survival (%)",
  title         = "Survival by whether prior ERCP performed (malignant obstruction)",
  palette       = "lancet",
  ggtheme       = theme_biliary
)

surv_ercp
