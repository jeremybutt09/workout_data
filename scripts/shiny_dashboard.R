library(shiny)
library(tidyverse)

#PACKAGE TO CONSIDER DT, LEAFLET, AND PLOTLY

ui <- fluidPage(
  titlePanel("Workout Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "body_part",
                  label = "Choose a Body Part",
                  selected = "Chest",
                  choices = c("Chest", "Arms", "Legs", "Back", "Shoulders"))
    ),
    mainPanel(
      DT::DTOutput("workout_data")
    )
  )
)

server <- function(input, output, session) {
  wo_data <- function() {
    wo_data <- read_csv(file = "C:/Users/Jeremy/Documents/workout_data/data/workout_output.csv")
    ref_data <- read_csv(file = "C:/Users/Jeremy/Documents/workout_data/reference/muscle_ref.csv")
    
    wo_data %>%
      left_join(ref_data, by = c("exercise_name" = "EXERCISE_NAME")) %>%
      filter(str_detect(string = GENERIC_MUSCLE_GROUP,
                        pattern = str_to_upper(input$body_part))) %>%
      top_n(10, weight)
  }
  output$workout_data <- DT::renderDT({
    DT::datatable(wo_data())
  })
}

shinyApp(ui = ui, server = server)