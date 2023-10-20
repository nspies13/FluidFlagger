# server.R
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(fastshap))
suppressPackageStartupMessages(library(shapviz))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(tidymodels))

helpers <- list.files("helpers", full.names = T)
map(helpers, ~ source(.x))

# Load the trained models
loaded_model <-readRDS("models/BJH_Comments_grid_BMP_Base_XGB_current_with_deltas.rds")$model |> bundle::unbundle()
apd_pca <- readRDS("models/apd_pca.rds")
train <- read.csv("data/train.csv")

pred_function <-
  function(object, newdata) {
    as.numeric(as.character(predict(object, new_data = newdata)$.pred_class))
  }

rec <- extract_recipe(loaded_model)
set.seed(12345)


server <- function(input, output, session) {
  observeEvent(input$predict, {
    input_data <- data.frame(
      sodium = input$sodium,
      chloride = input$chloride,
      potassium_plas = input$potassium_plas,
      co2_totl = input$co2_totl,
      bun = input$bun,
      creatinine = input$creatinine,
      calcium = input$calcium,
      glucose = input$glucose,
      anion_gap = input$sodium - input$chloride - input$co2_totl,
      sodium_delta_prior = input$sodium_delta_prior,
      chloride_delta_prior = input$chloride_delta_prior,
      potassium_plas_delta_prior = input$potassium_plas_delta_prior,
      co2_totl_delta_prior = input$co2_totl_delta_prior,
      bun_delta_prior = input$bun_delta_prior,
      creatinine_delta_prior = input$creatinine_delta_prior,
      calcium_delta_prior = input$calcium_delta_prior,
      glucose_delta_prior = input$glucose_delta_prior,
      anion_gap_delta_prior = input$sodium_delta_prior - input$chloride_delta_prior - input$co2_totl_delta_prior
    )
    
    pred_time <- system.time({
      # Get prediction and probability
      pred <-
        ifelse(
          predict(loaded_model, input_data) %>% pluck(1) == 1,
          "Contaminated",
          "Not Contaminated"
        )
      prob <-
        round(predict(loaded_model, input_data, type = "prob") %>% select(.pred_1),
              digits = 3)
    })[3]
    print(paste("Prediction time:", pred_time, "seconds"))
    
    apd_time <- system.time({
      # Calculate applicability label
      ood_label <- applicable::score(apd_pca, input_data) %>%
        mutate(
          label = ifelse(
            distance_pctl > 99.5 |
              distance_pctl == 1,
            "Out-of-Distribution",
            "Applicable"
          )
        ) %>%
        pluck("label")
    })[3]
    print(paste("Applicability time:", apd_time, "seconds"))
    
    shap_time <- system.time({
      # Calculate SHAP values
      shap <-
        shapviz(
          extract_fit_engine(loaded_model),
          X_pred = as.matrix(input_data %>% select(-matches("anion"))),
          interactions = F
        )
      shap <- as.matrix(shap$S * -1)
    })[3]
    print(paste("SHAP time:", shap_time, "seconds"))
    
    shap_plot_time <- system.time({
      # Make Force Plot
      shap_plot <-
        sv_waterfall(
          shapviz(shap, input_data),
          row_id = 1,
          size = 1,
          max_display = 10,
          show_annotation = F,
          format_shap = function(z)
            prettyNum(
              z,
              digits = 2,
              scientific = F,
              format = "fg"
            ),
          fill_colors = c("darkred", "tan")
        ) +
        ggtitle("Contributions to Prediction by Feature") + theme(
          plot.margin = margin(48, 0, 0, 0),
          plot.title = element_text(
            size = 36,
            hjust = 0.5,
            face = "bold",
            color = "darkgray"
          ),
          axis.text = element_text(face = "bold", size = 18),
          axis.title = element_text(face = "bold.italic", size = 24)
        )
    })[3]
    print(paste("SHAP plot time:", shap_plot_time, "seconds"))
    
    # Display the results
    output$finalPrediction <- renderUI({
      if (pred == "Contaminated") {
        style <- "color:darkred;"
      } else {
        style <- "color:black;"
      }
      HTML(paste("<div style='width:100%; height:100%; display: flex; align-items: center; justify-content: left; font-size:48px; font-weight:bold; ", style, "'>Final Prediction:&nbsp&nbsp <span style='font-weight:normal; font-style:italic; font-weight:bold;'>", pred, "</span></div>"))
    })
    
    output$predictedProbability <- renderUI({
      HTML(
        paste(
          "<div style='width:100%; height:100%; display: flex; align-items: center; justify-content: left; font-size:24px; font-weight:bold; color:darkgrey;'>Probability:&nbsp&nbsp <span style='font-weight:normal; color:black; font-style:italic;'>",
          prob,
          "</span></div>"
        )
      )
    })
    
    output$predictionApplicability <- renderUI({
      if (ood_label == "Out-of-Distribution") {
        style <- "color:darkred;"
      } else {
        style <- "color:black;"
      }
      HTML(paste("<div style='width:100%; height:100%; display: flex; align-items: center; justify-content: left; font-size:24px; font-weight:bold; ", style, "'>Applicability:&nbsp&nbsp <span style='font-weight:normal; font-style:italic;'>", ood_label, "</span></div>"))
    })
    
    output$shapPlot <- renderPlot({
      shap_plot
    })
    
  })
  
}
