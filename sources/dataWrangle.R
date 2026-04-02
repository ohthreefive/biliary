source("sources/libraries.R")

# ── Import ────────────────────────────────────────────────────────────────────

biliary <- read_excel("data/biliary.xlsx")

# ── Validate ──────────────────────────────────────────────────────────────────

# dupCHI should contain only 1 (no duplicate patients); halt if not
if (any(biliary$dupCHI != 1, na.rm = TRUE)) {
  stop("Duplicate CHI numbers detected — resolve before proceeding.")
}

# ── Remove columns ────────────────────────────────────────────────────────────

# CHI: patient identifier (not needed for analysis)
# DOB: date of birth (not needed for analysis)
# dupCHI: confirmed all 1s above
# Code: not needed
# dateDeath / daysDeath: Survival column already encodes this information
# Column1: free-text notes spillover from spreadsheet, not part of the dataset
biliary <- biliary |>
  select(-CHI, -dupCHI, -DOB, -Code, -dateDeath, -daysDeath, -Column1)

# ── Derive new columns ────────────────────────────────────────────────────────

biliary <- biliary |>
  mutate(
    # Year of procedure (useful for time-series charts)
    Year = year(eventDate),

    # Age rounded down to decade (e.g. 47 → "40s")
    Decade = paste0(floor(ageProcedure / 10) * 10, "s"),

    # Abbreviated hospital code for legends etc.
    Site = case_when(
      Site == "G107H" ~ "GRI",
      Site == "QEUH"  ~ "QEUH"
    ),

    # Full hospital name for use where space allows
    Hospital = case_when(
      Site == "GRI"  ~ "Glasgow Royal Infirmary",
      Site == "QEUH" ~ "Queen Elizabeth University Hospital"
    ),

    # Rename Diag to Diagnosis with shorter factor labels
    Diagnosis = case_when(
      Diag == "Presumed or confirmed benign obstruction"    ~ "Benign",
      Diag == "Presumed or confirmed malignant obstruction" ~ "Malignant"
    )
  ) |>
  # Drop the original Diag column now that Diagnosis exists
  select(-Diag)

# ── Data corrections ──────────────────────────────────────────────────────────

# Weight of 7.27 kg is a data entry error — should be 72.7 kg
# TODO: correct in source Excel file when possible and remove this line
biliary <- biliary |>
  mutate(Weight = if_else(Weight == 7.27, 72.7, Weight))
