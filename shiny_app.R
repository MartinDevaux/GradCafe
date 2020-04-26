setwd("/Users/martindevaux/Documents/R Path/GradCafe")

library(shiny)
library(tidyverse)
library(plotly)
library(stringr)
library(forcats)
library(ggstance)
library(extrafont)
library(shinyjs)

load("cleaned_data.Rdata")

unique_institutions <- data %>% 
  select(institution) %>% 
  unique() %>%
  arrange(institution)

years <- data %>% 
  select(decision_year) %>% 
  unique() %>%
  arrange(decision_year)

decisions <- data %>%
  select(decision) %>% 
  unique() %>%
  filter(is.na(decision) == F) %>% 
  arrange(decision)

source("Functions.R")

# User interface ----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("PhD Admission Result Past Dates"),
  
  sidebarLayout(
    sidebarPanel(

      div(
        id = "form",
        
        selectInput("institutions", 
                    label = "Which school are you applying to?",
                    choices = unique_institutions),
        
        checkboxGroupInput("years", 
                    label = "Years covered",
                    choices = years[[1]],
                    selected = 2020),
        
        checkboxGroupInput("decisions", 
                           label = "Decision types",
                           choices = decisions[[1]],
                           selected = c("Accepted", "Interview", "Wait listed", "Rejected", "Other"))
      ),
    
      actionButton("resetAll", "Reset all"),
    ),
    
    mainPanel(plotlyOutput("calendar_viz"),
              textOutput("first_acceptance"),
              textOutput("first_rejection"),
              textOutput("first_interview"),
              textOutput("first_waitlist")
              )
  )
)

# Server logic ----
server <- function(input, output) {
  
  output$calendar_viz <- renderPlotly({
    decision_calendar(input$institutions,
                      input$decisions,
                      input$years)
  })
  
  output$first_acceptance <- renderText({
    first_acceptance(input$institutions,
              input$decisions,
              input$years)
  })
  
  output$first_rejection <- renderText({
    first_rejection(input$institutions,
              input$decisions,
              input$years)
  })
  
  output$first_waitlist <- renderText({
    first_waitlist(input$institutions,
              input$decisions,
              input$years)
  })

  output$first_interview <- renderText({
    first_interview(input$institutions,
              input$decisions,
              input$years)
  })
  
  observeEvent(input$resetAll, {
    reset("form")
  })
  
}

# Run app ----
shinyApp(ui, server)