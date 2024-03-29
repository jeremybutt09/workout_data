library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)

#PACKAGE TO CONSIDER DT, LEAFLET, AND PLOTLY

ui <- fluidPage(
  titlePanel("Workout Data Analysis"),
  theme = shinythemes::shinytheme('flatly'),
  #shinythemes::themeSelector(),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1|input.tabselected==2",
        selectInput(inputId = "exercise",
                    label = "Choose an Exercise",
                    selected = "BACK SQUAT",
                    choices = c("BACK SQUAT (BARBELL)", "DEADLIFT (BARBELL)", "FLAT BENCH PRESS (BARBELL)", "LAT PULLDOWN (MACHINE)", "STANDING SHOULDER PRESS (BARBELL)"),
                    multiple = FALSE)),
      conditionalPanel(condition = "input.tabselected==3",
        selectInput(inputId = "gen_muscle_group",
                    label = "Choose a Muscle Group",
                    selected = "LEGS",
                    choices = c("BACK", "CHEST", "LEGS", "SHOULDERS", "ARMS", "FULL BODY"),
                    multiple = FALSE))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('PR Reps',
                 value = 1,
                 DT::DTOutput("pr_rep_data"),
                 plotly::plotlyOutput("plot_pr_reps_trends")),
        tabPanel('PR Sets',
                 value = 2,
                 DT::DTOutput("pr_set_data"),
                 plotly::plotlyOutput("plot_pr_set_trends")),
        tabPanel('Volume',
                 value = 3,
                 plotly::plotlyOutput("plot_volume")),
        id = "tabselected"
      )
    )
  )
)

server <- function(input, output, session) {
  
  setwd("C:/Users/Jeremy/Documents/workout_data")
  wo_data <- read_csv(file = "data/workout_output.csv") %>%
    mutate(reps_factor = factor(x = as.character(reps),
                                levels = c(1:20)))
  ref_data <- read_csv(file = "reference/muscle_ref.csv")
  
  coeff <- 0.01
    
  pr_reps_data <- function() {

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
                pr,
                date) %>%
      arrange(reps)
  }
  
  pr_reps_trends <- function() {
    
    wo_data %>%
      filter(reps != 0) %>%
      group_by(exercise_name,
               reps_factor,
               workout_date) %>%
      summarize(pr = max(weight)) %>%
      filter(exercise_name %in% input$exercise) %>%
      ggplot(aes(x = workout_date, y = pr, color = reps_factor)) +
      geom_line() +
      geom_point() +
      labs(color = "Reps") +
      theme(axis.title.x = element_blank()) +
      ylab("LBS")
    
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
                pr_lbs = pr,
                date) %>%
      arrange(set,
              reps)
  }
  
  pr_sets_trends <- function() {
    wo_data %>%
      filter(reps_factor != 0,
             exercise_name %in% input$exercise) %>%
      group_by(exercise_name,
               set,
               reps_factor,
               workout_date) %>%
      summarize(pr = max(weight)) %>%
      ggplot(aes(x = workout_date, y = pr, color = reps_factor)) +
      geom_line() +
      geom_point() +
      facet_wrap(~as.character(set)) +
      labs(color = "Reps") +
      theme(axis.title.x = element_blank()) +
      ylab("LBS")
  }
  
  volume_trends <- function() {
    wo_data %>%
      left_join(ref_data, by = "exercise_name") %>%
      filter(generic_muscle_group == input$gen_muscle_group) %>%
      mutate(workout_week = floor_date(workout_date, "week")) %>%
      group_by(workout_week,
               generic_muscle_group) %>%
      summarise(reps = sum(reps),
                volume = sum(volume)) %>%
      ggplot(aes(x = workout_week)) +
      geom_col(aes(y = volume), fill = "skyblue3") +
      geom_line(aes(y = reps/coeff), size = 1, color = "springgreen4") +
      scale_y_continuous(name = "Lbs",
                         sec.axis = sec_axis(~.*coeff, name = "Number of Reps")) + 
      theme(legend.position = "none",
            axis.title.x = element_blank()) +
      labs(title = "Workout Volume and Reps")
  }
  
  output$pr_rep_data <- DT::renderDT({
    DT::datatable(pr_reps_data())
  })
  
  output$plot_pr_reps_trends <- plotly::renderPlotly({
    pr_reps_trends()
  })
  
  output$pr_set_data <- DT::renderDT({
    DT::datatable(pr_sets_data())
  })
  
  output$plot_pr_set_trends <- plotly::renderPlotly({
    pr_sets_trends()
  })
  
  output$plot_volume <- plotly::renderPlotly({
    volume_trends()
  })
}

shinyApp(ui = ui, server = server)