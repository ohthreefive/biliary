# ── Mortality rates ───────────────────────────────────────────────────────────

# All patients have >= 99 days follow-up (min daysSince = 99), so no filtering
# is required before calculating 30, 42, or 90 day mortality.
#
# A patient died within N days if: Alive == "No" AND Survival <= N
# Denominator is all procedures.

# Helper function: mortality rate at a given number of days
mortality_rate <- function(data, days) {
  round(sum(data$Alive == "No" & data$Survival <= days) / nrow(data) * 100)
}

# Calculate rates by Diagnosis and for the total cohort
timepoints <- c("30-day", "42-day", "90-day", "Overall")

mortality_by_diag <- biliary |>
  group_by(Diagnosis) |>
  summarise(
    `30-day`  = mortality_rate(pick(everything()), 30),
    `42-day`  = mortality_rate(pick(everything()), 42),
    `90-day`  = mortality_rate(pick(everything()), 90),
    Overall   = round(sum(Alive == "No") / n() * 100)
  )

mortality_total <- biliary |>
  summarise(
    Diagnosis = "Total",
    `30-day`  = mortality_rate(pick(everything()), 30),
    `42-day`  = mortality_rate(pick(everything()), 42),
    `90-day`  = mortality_rate(pick(everything()), 90),
    Overall   = round(sum(Alive == "No") / n() * 100)
  )

mortality_summary <- bind_rows(mortality_by_diag, mortality_total)

# ── Mortality summary table ───────────────────────────────────────────────────

table_mortality <- mortality_summary |>
  gt() |>
  cols_label(Diagnosis = "") |>
  tab_header(title = "Mortality rate at timepoints (%)") |>
  tab_options(table.font.names = "Helvetica")

table_mortality

# ── Mortality bar chart ───────────────────────────────────────────────────────

# Pivot to long format for ggplot, then fix timepoint order for the x-axis
mortality_long <- mortality_summary |>
  pivot_longer(-Diagnosis, names_to = "Timepoint", values_to = "Rate") |>
  mutate(Timepoint = factor(Timepoint, levels = timepoints))

chart_mortality <- ggplot(
    mortality_long |> filter(Diagnosis != "Total"),
    aes(x = Timepoint, y = Rate, fill = Diagnosis)
  ) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = paste0(Rate, "%")),
    position = position_dodge(width = 0.9),
    vjust     = -0.4,
    size      = 3.5
  ) +
  scale_fill_lancet(alpha = 0.9) +
  labs(
    title = "Mortality by diagnosis and timepoint",
    x     = "Timepoint",
    y     = "Mortality (%)",
    fill  = "Diagnosis"
  ) +
  theme_biliary

chart_mortality

# ── Inpatient mortality ───────────────────────────────────────────────────────

# Denominator is ALL procedures — this is the clinically meaningful framing:
# "X% of patients who undergo this procedure will die in hospital."
# Alive patients contribute to N but have no inpatient death recorded (NA).

inpatient_summary <- biliary |>
  group_by(Diagnosis) |>
  summarise(
    N         = n(),
    Inpatient = sum(`Inpatient mortality` == "Yes", na.rm = TRUE),
    `%`       = round(Inpatient / N * 100)
  ) |>
  bind_rows(
    biliary |>
      summarise(
        Diagnosis = "Total",
        N         = n(),
        Inpatient = sum(`Inpatient mortality` == "Yes", na.rm = TRUE),
        `%`       = round(Inpatient / n() * 100)
      )
  )

table_inpatient <- inpatient_summary |>
  gt() |>
  cols_label(Diagnosis = "", Inpatient = "Inpatient deaths") |>
  tab_header(title = "Inpatient mortality") |>
  tab_options(table.font.names = "Helvetica")

table_inpatient

# ── Mortality by age decade (malignant cohort) ────────────────────────────────

# Prompted by Cox regression finding that age had little independent effect.
# Decade factor levels set explicitly to ensure correct ordering in the table.

decade_mortality <- biliary_malignant |>
  mutate(Decade = factor(Decade, levels = paste0(seq(10, 90, 10), "s"))) |>
  group_by(Decade) |>
  summarise(
    N        = n(),
    `30-day` = round(sum(Alive == "No" & Survival <= 30) / n() * 100),
    `42-day` = round(sum(Alive == "No" & Survival <= 42) / n() * 100),
    `90-day` = round(sum(Alive == "No" & Survival <= 90) / n() * 100)
  ) |>
  arrange(Decade)

table_decade_mortality <- decade_mortality |>
  gt() |>
  cols_label(Decade = "Decade", N = "N") |>
  tab_spanner(label = "Mortality (%)", columns = c(`30-day`, `42-day`, `90-day`)) |>
  tab_header(title = "Mortality by age (malignant obstruction)") |>
  tab_options(table.font.names = "Helvetica")

table_decade_mortality
