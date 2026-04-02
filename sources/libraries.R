library(tidyverse)   # data manipulation and ggplot2 for visualisation
library(readxl)      # reading .xlsx data files
library(lubridate)   # working with dates
library(gt)          # formatted tables
library(gtsummary)   # publication-ready summary/Table 1 tables
library(survival)    # Surv() and survfit() for Kaplan-Meier analysis
library(survminer)   # ggsurvplot() for publication-ready survival curves
library(tidymodels)  # framework for machine learning (splitting, recipes, models, evaluation)
library(probably)    # calibration plots for ML models
library(vip)         # variable importance plots
library(ggsci)       # lancet and other colour palettes
# tidymodels uses ranger (random forest) and xgboost as engines — install if needed:
# install.packages(c("ranger", "xgboost"))
