# ui.R
suppressPackageStartupMessages(library(shiny))

ui <- fluidPage(
  
  titlePanel(HTML(
    paste(
      "<div style='width:100%; height:100%; display: flex; align-items: center; justify-content: left; font-size:64px; font-weight:bold; font-style:italic; color:black;
                        '>FluidFlagger.ai </div>"
    )
  ),
  windowTitle = "FluidFlagger.ai"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          6,
          h3("Current Results"),
          numericInput(
            "sodium",
            "Sodium (mmol/L):",
            145,
            min = 120,
            max = 180
          ),
          numericInput(
            "chloride",
            "Chloride (mmol/L):",
            116,
            min = 50,
            max = 140
          ),
          numericInput(
            "potassium_plas",
            "Potassium (mmol/L):",
            2.9,
            min = 1,
            max = 10
          ),
          numericInput("co2_totl", "CO2 (mmol/L):", 18, min = 5),
          numericInput("bun", "BUN (mg/dL):", 14, min = 0),
          numericInput("creatinine", "Creatinine (mg/dL):", 0.6, min = 0),
          numericInput(
            "calcium",
            "Calcium (mg/dL):",
            6.8,
            min = 3,
            max = 18
          ),
          numericInput(
            "glucose",
            "Glucose (mg/dL):",
            80,
            min = 10,
            max = 5000
          )
        ),
        column(
          6,
          h3("Change from Prior"),
          numericInput("sodium_delta_prior", "Δ Sodium:", 1),
          numericInput("chloride_delta_prior", "Δ Chloride:", 18),
          numericInput("potassium_plas_delta_prior", "Δ Potassium:",-1.5),
          numericInput("co2_totl_delta_prior", "Δ CO2_totl:",-6),
          numericInput("bun_delta_prior", "Δ BUN:", 0),
          numericInput("creatinine_delta_prior", "Δ Creatinine:", 0),
          numericInput("calcium_delta_prior", "Δ Calcium:",-2.2),
          numericInput("glucose_delta_prior", "Δ Glucose:",-8)
        )
      ),
      actionButton("predict", "Predict"),
      width = 5
    ),
    mainPanel(
      uiOutput("finalPrediction", height = "15%"),
      uiOutput("predictedProbability", height = "15%"),
      uiOutput("predictionApplicability", height = "15%"),
      plotOutput("shapPlot", height = 600),
      width = 7
    )
  ),
  
  tags$footer(
    style = "position: fixed; bottom: 0; left: 0; width: 100%; background-color: lightgrey; color: black; text-align: center; font-size: 18pt; font-weight: bold; font-style: italic;",
    "This application should NOT be used for clinical care. It is for research use only."
  )
)
