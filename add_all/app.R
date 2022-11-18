
library(shiny)
ui <- fluidPage(

    # Application title
    titlePanel("Add select all option"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput("pick_options", "Please select from the options below",
                      choices = c("A", "B", "C"),
                      selected = "A", multiple = TRUE
          ),
          actionButton("select_all", "Click to select all options")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("show_selection")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$select_all, {
      updateSelectInput(inputId = "pick_options", selected = c("A", "B", "C"))
    })

    get_options <- reactive({input$pick_options})

    output$show_selection <- renderText({
      selected_options <- get_options()

      paste("You have selected ", paste(selected_options, collapse = ", "), ".")
    })
}

shinyApp(ui = ui, server = server)
