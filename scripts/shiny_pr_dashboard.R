library(shiny)
library(tidyverse)
library(plotly)

#PACKAGE TO CONSIDER DT, LEAFLET, AND PLOTLY

ui <- fluidPage(
  titlePanel("Workout Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "exercise",
                  label = "Choose an Exercise",
                  selected = "BACK SQUAT",
                  choices = c("BACK SQUAT (BARBELL)", "DEADLIFT (BARBELL)", "FLAT BENCH PRESS (BARBELL)", "LAT PULLDOWN (MACHINE)", "STANDING SHOULDER PRESS (BARBELL)"),
                  multiple = FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('PR Reps',
                 DT::DTOutput("pr_rep_data"),
                 plotly::plotlyOutput("plot_pr_reps_trends")),
        tabPanel('PR Sets',
                 DT::DTOutput("pr_set_data"),
                 plotly::plotlyOutput("plot_pr_set_trends"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  pr_reps_data <- function() {
    setwd("C:/Users/Jeremy/Documents/workout_data")
    wo_data <- read_csv(file = "data/workout_output.csv")
    ref_data <- read_csv(file = "reference/muscle_ref.csv")
    
    wo_data %>%
      filter(reps != 0,
             exercise_name == input$exercise) %>%
      group_by(exercise_name,
               reps) %>%
      summarize(pr = max(weight)) %>%
      inner_join(wo_data, by = c("exercise_name", "reps")) %>%
      group_by(exercise_name,
               reps,
               pr) %>%
      summarize(date = max(workout_date)) %>%
      transmute(exercise_name,
                reps,
                pr_details = paste("PR:", pr, "LBS", "DATE:", date)) %>%
      pivot_wider(names_from = exercise_name,
                  values_from = pr_details) %>%
      arrange(reps)
  }
  
  pr_reps_trends <- function() {
    setwd("C:/Users/Jeremy/Documents/workout_data")
    wo_data <- read_csv(file = "data/workout_output.csv")
    
    wo_data %>%
      filter(reps != 0) %>%
      group_by(exercise_name,
               reps,
               workout_date) %>%
      summarize(pr = max(weight)) %>%
      filter(exercise_name %in% input$exercise) %>%
      ggplot(aes(x = workout_date, y = pr, color = as.character(reps))) +
      geom_line() +
      geom_point()
    
  }
  
  pr_sets_data <- function() {
    wo_data %>%
      filter(reps != 0,
             exercise_name == input$exercise) %>%
      group_by(exercise_name,
               set,
               reps) %>%
      summarize(pr = max(weight)) %>%
      inner_join(wo_data, by = c("exercise_name", "reps", "set")) %>%
      group_by(exercise_name,
               set,
               reps,
               pr) %>%
      summarize(date = max(workout_date)) %>%
      transmute(exercise_name,
                set,
                reps,
                pr_details = paste("PR:", pr, "LBS", "DATE:", date)) %>%
      pivot_wider(names_from = exercise_name,
                  values_from = pr_details) %>%
      arrange(reps)
  }
  
  pr_sets_trends <- function() {
    wo_data %>%
      filter(reps != 0,
             exercise_name %in% input$exercise) %>%
      group_by(exercise_name,
               set,
               reps,
               workout_date) %>%
      summarize(pr = max(weight)) %>%
      ggplot(aes(x = workout_date, y = pr, color = as.character(reps))) +
      geom_line() +
      geom_point() +
      facet_wrap(~as.character(set))
  }
  
  output$pr_rep_data <- DT::renderDT({
    DT::datatable(pr_reps_data())
  })
  
  output$plot_pr_reps_trends <- plotly::renderPlotly({
    pr_reps_trends()
  })
  
  output$pr_set_data <- DT::renderDT({
    DT:datatable(pr_sets_data())
  })
  
  output$plot_pr_set_trends <- plotly::renderPlotly({
    pr_sets_trends()
  })
}

shinyApp(ui = ui, server = server)