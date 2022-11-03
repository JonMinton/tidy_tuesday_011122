library(tidyverse)
library(shiny)

pred_data <- read_rds("scenarios_and_predictions.rds")

choice_labels <- c(
  "$1 Million" = 1e6,
  "$5 Million" = 5e6,
  "$10 Million" = 1e7,
  "£20 Million" = 2e7
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    # Application title
    titlePanel("Predicted Revenue from different scenarios"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("budget",
                        "Select proposed budget",
                        choices = choice_labels,
                        selected = 1e7
                        ),
            selectInput("runtime",
                        "Select proposed runtime",
                        choices = c(
                          "80 minutes" = 80,
                          "90 minutes" = 90,
                          "100 minutes" = 100
                        ),
                        selected = 90
                        ),
            selectInput("year",
                        "When was the film released?",
                        choices = c(1985, 1995, 2005, 2015),
                        selected = 2015
                        ),
            selectInput("is_collection",
                        "Was the film part of an existing franchise?",
                        choices = c("No" = FALSE, "Yes" = TRUE),
                        selected = FALSE
            ),
            selectInput("horror_first",
                        "Is the film primarily a horror film?",
                        choices = c("No" = FALSE, "Yes" = TRUE),
                        selected = TRUE
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h2("Scenario details"),
           textOutput("scenario"),
           br(),
           h2("Conclusion:"),
           uiOutput("scenario_conclusion")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    get_scenario_result <- reactive({
      pred_data %>%
        filter(
          budget == input$budget,
          runtime == input$runtime,
          year == input$year,
          is_collection == input$is_collection,
          horror_first == input$horror_first
        )
    })

    output$scenario <- renderText({
      scenario <- get_scenario_result()
      predicted_revenue <- scenario$predicted_revenue
      predicted_profit     <- scenario$predicted_profit
      predicted_roi        <- scenario$predicted_roi




      glue::glue(
        "A {input$runtime} minute
        {switch(input$horror_first, 'TRUE' = 'horror', 'FALSE' = 'horror-adjacent')} film,
        {switch(input$is_collection, 'TRUE' = 'part of', 'FALSE' = 'not part of')} an existing film franchise,
        released in {input$year} and with a budget of {choice_labels[choice_labels== input$budget] %>% names()}
        is projected to earn around ${round(predicted_revenue / 1e6, 1)} Million. This means it's expected to make
        a {ifelse(predicted_profit > 0, 'profit','loss')} of around
        ${round(abs(predicted_profit / 1e6), 1)} Million, and that for every dollar invested the return will be
        ${round(predicted_roi, 2)}.
        "
      )
    })

    output$scenario_conclusion <- renderUI({

      get_adjective <- function(x){
        cut(abs(x),
            breaks =
              c(0, 0.25,
                0.50,
                0.75,
                1.0,
                1.25,
                1.50,
                1.75,
                2.00, Inf),
            include.lowest = TRUE,
            labels =
              c("disappointing",
                "modest",
                "squelchy",
                "chilling",
                "crunchy",
                "scary",
                "horrifying",
                "terrifying",
                "soul destroying")
        ) %>% as.character()
      }

      scenario <- get_scenario_result()
      prof_color <- ifelse(scenario$predicted_profit > 0, "color:#cc3910", "color:#ffffff")
      prof_type  <- ifelse(scenario$predicted_profit > 0, "success", "failure")
      scale_of_success_failure <- log(scenario$predicted_roi)
      roi_adjective    <- get_adjective(abs(scale_of_success_failure))


      tags$html(
        tags$body(
          p(glue::glue("A {roi_adjective} {prof_type}"), style=glue::glue("font-size:60px;{prof_color}"))
        )
      )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
