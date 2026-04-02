# ── Normality testing ─────────────────────────────────────────────────────────

# Shapiro-Wilk test on all continuous variables
# NA values are dropped automatically via na.omit() — relevant for Weight
# p < 0.05 suggests non-normal distribution

continuous_vars <- c("ageProcedure", "Hb", "WCC", "Plts", "PT",
                     "Urea", "Cr", "Bili", "Alb", "CRP", "Weight")

normality_results <- map_dfr(continuous_vars, function(var) {
  values <- na.omit(biliary[[var]])
  test   <- shapiro.test(values)
  tibble(
    Variable  = var,
    W         = round(test$statistic, 3),
    p_value   = round(test$p.value, 3),
    Normal    = ifelse(test$p.value >= 0.05, "Yes", "No")
  )
})

print(normality_results)

# ── Shared variable labels ────────────────────────────────────────────────────

# Defined once and reused across all three tables
var_digits <- list(
  PT  ~ 0,   # prothrombin time: 0 decimal places
  Alb ~ 0    # albumin: 0 decimal places
)

var_labels <- list(
  ageProcedure ~ "Age",
  Hb           ~ "Haemoglobin (g/L)",
  WCC          ~ "White cell count (×10⁹/L)",
  Plts         ~ "Platelets (×10⁹/L)",
  PT           ~ "Prothrombin time (seconds)",
  Urea         ~ "Urea (mmol/L)",
  Cr           ~ "Creatinine (µmol/L)",
  Bili         ~ "Bilirubin (µmol/L)",
  Alb          ~ "Albumin (g/L)",
  CRP          ~ "CRP (mg/L)",
  Weight       ~ "Weight (kg)"
)

# ── Table 1a: by Hospital (GRI vs QEUH) ──────────────────────────────────────

table1a <- biliary |>
  select(Site, ageProcedure, Sex, Diagnosis, ERCP,
         Hb, WCC, Plts, PT, Urea, Cr, Bili, Alb, CRP, Weight) |>
  tbl_summary(
    by = Site,                                          # split columns by hospital
    label = var_labels,                                 # apply shared labels
    digits = var_digits,                                # override decimal places
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",   # median (IQR) for continuous
      all_categorical() ~ "{n} ({p}%)"                 # n (%) for categorical
    ),
    missing = "no"                                      # don't show missing row (Weight)
  ) |>
  add_overall(last = TRUE) |>                          # add Total column on the right
  add_p(                                               # non-parametric tests
    test = list(
      all_continuous()  ~ "wilcox.test",               # Mann-Whitney U for continuous
      all_categorical() ~ "chisq.test"                 # chi-squared for categorical
    )
  ) |>
  bold_labels() |>
  modify_header(label ~ "") |>                         # remove 'Characteristic' header
  as_gt() |>
  tab_header(title = "Table 1a: Baseline characteristics by hospital")

table1a

# ── Table 1b: by Sex ──────────────────────────────────────────────────────────

table1b <- biliary |>
  select(Sex, ageProcedure, Diagnosis,
         Hb, WCC, Plts, PT, Urea, Cr, Bili, Alb, CRP, Weight) |>
  tbl_summary(
    by = Sex,
    label = var_labels,
    digits = var_digits,                                # override decimal places
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) |>
  add_overall(last = TRUE) |>
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",
      all_categorical() ~ "chisq.test"
    )
  ) |>
  bold_labels() |>
  modify_header(label ~ "") |>
  as_gt() |>
  tab_header(title = "Table 1b: Baseline characteristics by sex")

table1b

# ── Table 1c: by Diagnosis ────────────────────────────────────────────────────

table1c <- biliary |>
  select(Diagnosis, ageProcedure, ERCP,
         Hb, WCC, Plts, PT, Urea, Cr, Bili, Alb, CRP, Weight) |>
  tbl_summary(
    by = Diagnosis,
    label = var_labels,
    digits = var_digits,                                # override decimal places
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  ) |>
  add_overall(last = TRUE) |>
  add_p(
    test = list(
      all_continuous()  ~ "wilcox.test",
      all_categorical() ~ "chisq.test"
    )
  ) |>
  bold_labels() |>
  modify_header(label ~ "") |>
  as_gt() |>
  tab_header(title = "Table 1c: Baseline characteristics by diagnosis")

table1c

# ── Procedures per year ───────────────────────────────────────────────────────

# Count procedures per year (total and by hospital)
procs_per_year       <- biliary |> count(Year)
procs_per_year_site  <- biliary |> count(Year, Site)

# Total procedures per year — column chart
chart_procs_total <- ggplot(procs_per_year, aes(x = Year, y = n)) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(breaks = seq(2015, max(biliary$Year), 1)) +
  labs(
    title = "Biliary procedures per year",
    x     = "Year",
    y     = "Number of procedures"
  ) +
  theme_minimal()

chart_procs_total

# Procedures per year split by hospital — dodged column chart
chart_procs_site <- ggplot(procs_per_year_site, aes(x = Year, y = n, fill = Site)) +
  geom_col(position = "dodge") +
  scale_x_continuous(breaks = seq(2015, max(biliary$Year), 1)) +
  labs(
    title = "Biliary procedures per year by hospital",
    x     = "Year",
    y     = "Number of procedures",
    fill  = "Hospital"
  ) +
  theme_minimal()

chart_procs_site

# ── Study period dates (in-text figures) ─────────────────────────────────────

# Format a date as e.g. "1st Jan 1970"
ordinal_date <- function(date) {
  day    <- as.integer(format(date, "%d"))
  suffix <- ifelse(day %in% c(11, 12, 13), "th",
            ifelse(day %% 10 == 1, "st",
            ifelse(day %% 10 == 2, "nd",
            ifelse(day %% 10 == 3, "rd", "th"))))
  paste0(day, suffix, " ", format(date, "%b %Y"))
}

study_start <- ordinal_date(min(biliary$eventDate))
study_end   <- ordinal_date(max(biliary$eventDate))

# ── Average procedures per month (in-text figures) ───────────────────────────

# Calculate total number of months spanned by the dataset
n_months <- interval(min(biliary$eventDate), max(biliary$eventDate)) / months(1)

# Overall average per month
avg_per_month_total <- round(nrow(biliary) / n_months, 1)
cat("Average procedures per month (overall):", avg_per_month_total, "\n")

# Average per month by hospital
biliary |>
  count(Site) |>
  mutate(avg_per_month = round(n / n_months, 1)) |>
  print()

# ── Age histogram ─────────────────────────────────────────────────────────────

# Using the Decade column (already created in dataWrangle.R) with geom_bar()
# gives perfectly centred decade labels on each bar without axis alignment issues.
# Factor levels set explicitly so decades appear in the correct order.

chart_age <- biliary |>
  mutate(Decade = factor(Decade, levels = paste0(seq(10, 90, 10), "s"))) |>
  ggplot(aes(x = Decade)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Patient age at procedure",
    x     = "Age (decade)",
    y     = "Number of patients"
  ) +
  theme_minimal()

chart_age


