library(shiny)
library(tidymodels)

# Load the trained model and baseline rates (both saved by machineLearning.R)
saved     <- readRDS("shiny/model.rds")
model     <- saved$model
baselines <- saved$baselines

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  titlePanel("Biliary Drainage: 42-Day Mortality Risk Predictor"),

  sidebarLayout(

    sidebarPanel(
      width = 5,

      h4("Patient details"),
      fluidRow(
        column(6, numericInput("age",    "Age (years)",   value = NULL, min = 18, max = 100, step = 1)),
        column(6, numericInput("weight", "Weight (kg) — optional", value = NULL, min = 30, max = 250))
      ),
      fluidRow(
        column(6, selectInput("sex",       "Sex",       choices = c("Man", "Woman"))),
        column(6, selectInput("diagnosis", "Diagnosis", choices = c("Benign", "Malignant")))
      ),
      selectInput("ercp", "Prior ERCP attempted?", choices = c("No", "Yes")),

      hr(),
      h4("Blood results"),
      fluidRow(
        column(6, numericInput("hb",   "Haemoglobin (g/L)",   value = NULL)),
        column(6, numericInput("wcc",  "WCC (×10⁹/L)",        value = NULL))
      ),
      fluidRow(
        column(6, numericInput("plts", "Platelets (×10⁹/L)",  value = NULL)),
        column(6, numericInput("pt",   "PT (seconds)",         value = NULL))
      ),
      fluidRow(
        column(6, numericInput("urea", "Urea (mmol/L)",        value = NULL)),
        column(6, numericInput("cr",   "Creatinine (µmol/L)",  value = NULL))
      ),
      fluidRow(
        column(6, numericInput("bili", "Bilirubin (µmol/L)",   value = NULL)),
        column(6, numericInput("alb",  "Albumin (g/L)",        value = NULL))
      ),
      numericInput("crp", "CRP (mg/L)", value = NULL),

      br(),
      actionButton("predict", "Calculate risk", class = "btn-primary btn-lg", width = "100%")
    ),

    mainPanel(
      width = 7,
      uiOutput("result")
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Only calculate when the button is clicked
  prediction <- eventReactive(input$predict, {

    # Validate required fields — all blood tests and age are mandatory
    validate(
      need(!is.na(input$age),    "Please enter patient age."),
      need(!is.na(input$hb),     "Please enter haemoglobin."),
      need(!is.na(input$wcc),    "Please enter white cell count."),
      need(!is.na(input$plts),   "Please enter platelets."),
      need(!is.na(input$pt),     "Please enter prothrombin time."),
      need(!is.na(input$urea),   "Please enter urea."),
      need(!is.na(input$cr),     "Please enter creatinine."),
      need(!is.na(input$bili),   "Please enter bilirubin."),
      need(!is.na(input$alb),    "Please enter albumin."),
      need(!is.na(input$crp),    "Please enter CRP.")
    )

    # Build a one-row tibble matching the training data structure exactly.
    # Weight is optional — if left blank it will be imputed by the recipe.
    new_patient <- tibble(
      ageProcedure = as.numeric(input$age),
      Sex          = input$sex,
      Diagnosis    = input$diagnosis,
      ERCP         = input$ercp,
      Weight       = as.numeric(input$weight),  # NA if left blank — imputed by recipe
      Hb           = as.numeric(input$hb),
      WCC          = as.numeric(input$wcc),
      Plts         = as.numeric(input$plts),
      PT           = as.numeric(input$pt),
      Urea         = as.numeric(input$urea),
      Cr           = as.numeric(input$cr),
      Bili         = as.numeric(input$bili),
      Alb          = as.numeric(input$alb),
      CRP          = as.numeric(input$crp)
    )

    # predict() returns probabilities for both classes; we want .pred_Yes
    round(predict(model, new_patient, type = "prob")$.pred_Yes * 100)
  })

  output$result <- renderUI({
    req(prediction())

    prob     <- prediction()
    diag     <- input$diagnosis

    # Select the appropriate baseline for the chosen diagnosis
    baseline <- if (diag == "Benign") baselines$benign else baselines$malignant
    diag_label <- tolower(diag)

    colour     <- if (prob < baseline)  "#27ae60" else
                  if (prob == baseline) "#e67e22" else
                  "#e74c3c"

    risk_label <- if (prob < baseline)  paste0("Below baseline risk for ", diag_label, " obstruction") else
                  if (prob == baseline) paste0("At baseline risk for ", diag_label, " obstruction") else
                  paste0("Above baseline risk for ", diag_label, " obstruction")

    comparison <- if (prob < baseline) {
      paste0("The baseline 42-day mortality for ", diag_label, " obstruction in this dataset is ",
             baseline, "%. This patient's predicted risk is below that baseline.")
    } else if (prob == baseline) {
      paste0("This patient's predicted risk matches the baseline 42-day mortality for ",
             diag_label, " obstruction (", baseline, "%).")
    } else {
      paste0("The baseline 42-day mortality for ", diag_label, " obstruction in this dataset is ",
             baseline, "%. This patient's predicted risk is above that baseline.")
    }

    tagList(
      br(),
      h3("Predicted 42-day mortality risk", style = "text-align: center;"),
      div(
        style = paste0("font-size: 96px; font-weight: bold; color: ", colour, "; text-align: center; line-height: 1.1;"),
        paste0(prob, "%")
      ),
      div(
        style = paste0("font-size: 24px; font-weight: bold; color: ", colour, "; text-align: center;"),
        risk_label
      ),
      br(),
      p(style = "text-align: center; font-size: 15px;", comparison),
      hr(),
      p(
        style = "color: #777; font-size: 12px;",
        "This figure represents the model's estimated probability that this specific patient will",
        "die within 42 days of their biliary drainage procedure, based on their individual",
        "characteristics. Baseline rates in this dataset:",
        tags$b(paste0("overall ", baselines$overall, "%, benign obstruction ", baselines$benign,
                      "%, malignant obstruction ", baselines$malignant, "%.")),
        br(), br(),
        "Generated by a random forest model (AUC 0.73) trained on 499 biliary drainage procedures",
        "at Glasgow Royal Infirmary and Queen Elizabeth University Hospital (2015–2025).",
        "This tool is intended to support clinical decision-making and should not be used as the",
        "sole basis for clinical decisions."
      )
    )
  })
}

shinyApp(ui, server)
