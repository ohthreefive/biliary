# ── Binary classification: 42-day mortality ───────────────────────────────────
#
# Outcome:    died_42 — did the patient die within 42 days? (Yes/No)
# Patients:   all 499 (benign + malignant), with Diagnosis as a predictor
# Validation: temporal — train on earliest 80%, test on most recent 20%
# Models:     logistic regression, random forest, XGBoost

# ── Outcome and predictor dataset ────────────────────────────────────────────

# Sort by eventDate BEFORE selecting columns — this is what makes the
# subsequent train/test split temporal rather than random.

ml_binary <- biliary |>
  arrange(eventDate) |>
  mutate(
    died_42 = factor(
      ifelse(Alive == "No" & Survival <= 42, "Yes", "No"),
      levels = c("Yes", "No")   # "Yes" first = positive class for AUC calculation
    )
  ) |>
  select(died_42, ageProcedure, Sex, Diagnosis, ERCP, Weight,
         Hb, WCC, Plts, PT, Urea, Cr, Bili, Alb, CRP)

# ── Train/test split ──────────────────────────────────────────────────────────

# initial_time_split() takes the first 80% of rows as training (earliest patients)
# and the remaining 20% as test (most recent patients).
# No randomness involved — set.seed() kept for reproducibility if CV is added later.

set.seed(123)
split_binary <- initial_time_split(ml_binary, prop = 0.8)
train_binary <- training(split_binary)
test_binary  <- testing(split_binary)

cat("Training:", nrow(train_binary), "patients |",
    round(mean(train_binary$died_42 == "Yes") * 100), "% 42-day mortality\n")
cat("Test:    ", nrow(test_binary),  "patients |",
    round(mean(test_binary$died_42  == "Yes") * 100), "% 42-day mortality\n")

# ── Preprocessing recipe ──────────────────────────────────────────────────────

# A recipe defines preprocessing steps. Crucially, statistics (e.g. the median
# used for imputation, the mean/SD used for normalisation) are learned from the
# TRAINING data only and then applied to the test data — preventing data leakage.

binary_recipe <- recipe(died_42 ~ ., data = train_binary) |>
  step_impute_median(Weight) |>            # replace missing Weight with training median
  step_dummy(all_nominal_predictors()) |>  # encode Sex, Diagnosis, ERCP as 0/1 columns
  step_normalize(all_numeric_predictors()) |> # centre and scale (needed for logistic regression)
  step_zv(all_predictors())                # remove any zero-variance columns

# ── Model specifications ──────────────────────────────────────────────────────

spec_logistic <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

spec_rf <- rand_forest(trees = 500) |>
  set_engine("ranger", importance = "impurity") |>  # importance needed for vip() later
  set_mode("classification")

spec_xgb <- boost_tree(trees = 500) |>
  set_engine("xgboost") |>
  set_mode("classification")

# ── Workflows ─────────────────────────────────────────────────────────────────

# A workflow bundles the recipe and model spec together.
# Fitting the workflow applies the recipe then trains the model in one step.

wf_logistic <- workflow() |> add_recipe(binary_recipe) |> add_model(spec_logistic)
wf_rf       <- workflow() |> add_recipe(binary_recipe) |> add_model(spec_rf)
wf_xgb      <- workflow() |> add_recipe(binary_recipe) |> add_model(spec_xgb)

# ── Fit models on training data ───────────────────────────────────────────────

fit_logistic <- fit(wf_logistic, data = train_binary)
fit_rf       <- fit(wf_rf,       data = train_binary)
fit_xgb      <- fit(wf_xgb,      data = train_binary)

# ── Evaluate on test set ──────────────────────────────────────────────────────

# Helper: generates predictions and computes AUC, accuracy, sensitivity, specificity
eval_model <- function(fit, test_data, model_name) {
  preds <- bind_cols(
    predict(fit, test_data),                       # predicted class (Yes/No)
    predict(fit, test_data, type = "prob"),         # predicted probabilities
    test_data |> select(died_42)                   # true outcome
  )
  bind_rows(
    roc_auc(    preds, truth = died_42, .pred_Yes),
    accuracy(   preds, truth = died_42, .pred_class),
    sensitivity(preds, truth = died_42, .pred_class, event_level = "first"),
    specificity(preds, truth = died_42, .pred_class, event_level = "first")
  ) |>
    select(.metric, .estimate) |>
    mutate(.estimate = round(.estimate, 3), model = model_name)
}

results <- bind_rows(
  eval_model(fit_logistic, test_binary, "Logistic regression"),
  eval_model(fit_rf,       test_binary, "Random forest"),
  eval_model(fit_xgb,      test_binary, "XGBoost")
)

# ── Comparison table ──────────────────────────────────────────────────────────

table_ml_binary <- results |>
  pivot_wider(names_from = .metric, values_from = .estimate) |>
  rename(Model = model, AUC = roc_auc, Accuracy = accuracy,
         Sensitivity = sensitivity, Specificity = specificity) |>
  arrange(desc(AUC)) |>
  gt() |>
  tab_header(
    title    = "Binary classification: 42-day mortality",
    subtitle = "Temporal validation — most recent 20% as test set"
  ) |>
  fmt_number(columns = -Model, decimals = 3)

table_ml_binary

# ── ROC curves ────────────────────────────────────────────────────────────────

# ROC curve plots sensitivity vs (1 - specificity) across all probability thresholds.
# The diagonal dashed line = random chance (AUC 0.5). The closer to the top-left, the better.

roc_curves <- bind_rows(
  predict(fit_logistic, test_binary, type = "prob") |>
    bind_cols(test_binary |> select(died_42)) |>
    roc_curve(truth = died_42, .pred_Yes) |>
    mutate(model = "Logistic regression"),
  predict(fit_rf, test_binary, type = "prob") |>
    bind_cols(test_binary |> select(died_42)) |>
    roc_curve(truth = died_42, .pred_Yes) |>
    mutate(model = "Random forest"),
  predict(fit_xgb, test_binary, type = "prob") |>
    bind_cols(test_binary |> select(died_42)) |>
    roc_curve(truth = died_42, .pred_Yes) |>
    mutate(model = "XGBoost")
)

chart_roc_binary <- ggplot(roc_curves, aes(x = 1 - specificity, y = sensitivity, colour = model)) +
  geom_line(linewidth = 1) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  labs(
    title  = "ROC curves — 42-day mortality prediction",
    x      = "1 - Specificity (false positive rate)",
    y      = "Sensitivity (true positive rate)",
    colour = "Model"
  ) +
  theme_minimal()

chart_roc_binary

# ── Variable importance ───────────────────────────────────────────────────────

# Shows which predictors drove the predictions in each model.
# Using random forest here; can swap to whichever model performs best.

chart_vip_rf <- fit_rf |>
  extract_fit_parsnip() |>
  vip(num_features = 15) +
  labs(title = "Variable importance — random forest (42-day mortality)") +
  theme_minimal()

chart_vip_rf

# ── Hyperparameter tuning — random forest ─────────────────────────────────────
#
# We tune two hyperparameters:
#   mtry  — how many variables the model randomly considers at each split.
#            Too few = misses important variables. Too many = trees become too similar.
#   min_n — minimum number of patients in a node before it can split further.
#            Too small = trees overfit the training data. Too large = trees too simple.
#
# Method: try 25 combinations of mtry and min_n, evaluate each using 5-fold
# cross-validation on the training data, pick the best combination, then
# evaluate ONCE on the test set. The test set is never touched during tuning.

# Step 1: Redefine the random forest spec with tune() placeholders.
# tune() simply means "I haven't decided this value yet — try several."
spec_rf_tune <- rand_forest(
  trees = 500,
  mtry  = tune(),
  min_n = tune()
) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("classification")

# Step 2: Swap the tunable spec into the existing workflow
wf_rf_tune <- wf_rf |> update_model(spec_rf_tune)

# Step 3: Create 5-fold cross-validation splits from the training data.
# strata = died_42 ensures each fold has a similar proportion of deaths —
# important because 42-day mortality is not evenly distributed.
cv_folds <- vfold_cv(train_binary, v = 5, strata = died_42)

# Step 4: Define the grid of combinations to try.
# grid_regular() with levels = 5 gives 5 evenly-spaced values for each
# parameter, producing 5 × 5 = 25 combinations.
# mtry range: 2–12 (we have ~15 predictors after dummy encoding)
# min_n range: 2–20
rf_grid <- grid_regular(
  mtry(range  = c(2, 12)),
  min_n(range = c(2, 20)),
  levels = 5
)

# Step 5: Run the tuning — fits 25 combinations × 5 folds = 125 models.
# This may take a minute.
cat("Tuning random forest — fitting 125 models...\n")
rf_tune_results <- tune_grid(
  wf_rf_tune,
  resamples = cv_folds,
  grid      = rf_grid,
  metrics   = metric_set(roc_auc)
)

# Step 6: Show the top 5 combinations
cat("\nTop 5 hyperparameter combinations:\n")
print(show_best(rf_tune_results, metric = "roc_auc", n = 5))

# Step 7: Pick the single best combination
best_params <- select_best(rf_tune_results, metric = "roc_auc")
cat("\nBest parameters — mtry:", best_params$mtry, "| min_n:", best_params$min_n, "\n")

# Step 8: Lock in the best parameters and rebuild the workflow
wf_rf_tuned <- finalize_workflow(wf_rf_tune, best_params)

# Step 9: Refit on the FULL training set with best params, then evaluate on test set.
# last_fit() does both in one step — this is the only time the test set is used.
final_fit_rf <- last_fit(wf_rf_tuned, split_binary)

# Step 10: Report tuned AUC on test set
tuned_metrics <- collect_metrics(final_fit_rf)
cat("\nTuned random forest AUC:", round(tuned_metrics$.estimate[tuned_metrics$.metric == "roc_auc"], 3), "\n")
cat("(Original random forest AUC was 0.726)\n")

# ── Tuning visualisation ──────────────────────────────────────────────────────

# Shows how AUC changes across the grid — helps understand which region of
# the parameter space works best.
chart_tune <- autoplot(rf_tune_results) +
  labs(title = "Random forest tuning — AUC across hyperparameter grid") +
  theme_minimal()

chart_tune

# ── Variable importance — tuned model ─────────────────────────────────────────

chart_vip_tuned <- final_fit_rf |>
  extract_fit_parsnip() |>
  vip(num_features = 15) +
  labs(title = "Variable importance — tuned random forest (42-day mortality)") +
  theme_minimal()

chart_vip_tuned

# ── Save model for Shiny app ──────────────────────────────────────────────────

# Now that performance has been validated, retrain on the FULL dataset so the
# deployed model has seen as much data as possible.
# The workflow (recipe + model) is saved as a single object — the Shiny app
# loads this and calls predict() directly without needing any preprocessing steps.

final_model <- fit(wf_rf, data = ml_binary)

# Calculate baseline 42-day mortality rates from the full dataset.
# Saved alongside the model so the Shiny app always reflects current data.
baselines <- biliary |>
  summarise(
    overall   = round(sum(Alive == "No" & Survival <= 42) / n() * 100),
    benign    = round(sum(Alive == "No" & Survival <= 42 & Diagnosis == "Benign")    / sum(Diagnosis == "Benign")    * 100),
    malignant = round(sum(Alive == "No" & Survival <= 42 & Diagnosis == "Malignant") / sum(Diagnosis == "Malignant") * 100)
  )

saveRDS(list(model = final_model, baselines = baselines), "shiny/model.rds")
cat("Model and baselines saved to shiny/model.rds\n")
cat("Baselines — overall:", baselines$overall, "% | benign:", baselines$benign, "% | malignant:", baselines$malignant, "%\n")
