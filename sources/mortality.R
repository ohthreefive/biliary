source("sources/dataWrangle.R")

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
  tab_header(title = "Mortality rates (% of all procedures)") |>
  tab_spanner(label = "Mortality (%)", columns = -Diagnosis)

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
  labs(
    title = "Mortality by diagnosis and timepoint",
    x     = "Timepoint",
    y     = "Mortality (%)",
    fill  = "Diagnosis"
  ) +
  theme_minimal()

chart_mortality

# ── Inpatient mortality ───────────────────────────────────────────────────────

# Inpatient mortality is only meaningful for patients who have died (Alive == "No")
# Presented as % of all procedures and % of all deaths

n_total <- nrow(biliary)
n_dead  <- sum(biliary$Alive == "No")

inpatient_summary <- biliary |>
  filter(Alive == "No") |>
  group_by(Diagnosis) |>
  summarise(
    Deaths          = n(),
    Inpatient       = sum(`Inpatient mortality` == "Yes", na.rm = TRUE),
    `% of deaths`   = round(Inpatient / Deaths * 100),
    `% of total`    = round(Inpatient / n_total * 100)
  ) |>
  bind_rows(
    biliary |>
      filter(Alive == "No") |>
      summarise(
        Diagnosis      = "Total",
        Deaths         = n(),
        Inpatient      = sum(`Inpatient mortality` == "Yes", na.rm = TRUE),
        `% of deaths`  = round(Inpatient / n() * 100),
        `% of total`   = round(Inpatient / n_total * 100)
      )
  )

table_inpatient <- inpatient_summary |>
  gt() |>
  cols_label(Diagnosis = "") |>
  tab_header(title = "Inpatient mortality")

table_inpatient
