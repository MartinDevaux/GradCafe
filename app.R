library(shiny)
library(tidyverse)
library(plotly)
library(stringr)
library(forcats)
library(ggstance)
library(extrafont)
library(shinyjs)
library(shinyWidgets)

source("Functions.R")

# User interface ----
ui <- fluidPage(
  tags$head(includeHTML(("site_tag.html"))),
  
  # setBackgroundColor("#3d6594"),
  useShinyjs(),
  panel(),
  sidebarLayout(
    sidebarPanel(
      
      div(
        id = "form",
        
        selectInput("Institutions", 
                    label = "Which school are you applying to?",
                    choices = unique_institutions,
                    multiple = F,
                    selected = "Columbia University"),
        
        checkboxGroupInput("years", 
                    label = "Include results from:",
                    choices = years[[1]],
                    selected = c(2020, 2019),
                    inline = TRUE),
        
        checkboxGroupInput("decisions", 
                           label = "Include posts including:",
                           choices = decisions[[1]],
                           selected = c("Accepted", "Rejected"),
                           inline = TRUE),
        actionButton("resetAll", "Reset all")
      )
    ),
    
    mainPanel(
      # fluidRow(
      #   column(11,
      #   panel(
      #     p(strong("Disclaimer: "), "This project was conducted independently of the ", a("GradCafe", href = "https://www.thegradcafe.com/"), " team and any mistake is the author's. None of the information displayed comes from official sources."),
      #     p("Work in progress. Contact the author on ", a("Twitter", href = "https://twitter.com/MartinDevaux"), " or ", a("GitHub", href = "https://github.com/MartinDevaux"), ".")
      #     ))
      # ),
      fluidRow(
        column(11,
        panel(
          plotlyOutput("calendar_viz")
        ))
      ),
      fluidRow(
        column(11,
          panel(
          p("Over the period:"),
          textOutput("first_acceptance"),
          textOutput("first_rejection"),
          textOutput("first_interview"),
          textOutput("first_waitlist")
        ))
    ))
  )
)

# Server logic ----
server <- function(input, output) {
  
  output$calendar_viz <- renderPlotly({
    decision_calendar(input$Institutions,
                      input$decisions,
                      input$years)
  })
  
  output$first_acceptance <- renderText({
    first_acceptance(input$Institutions,
              input$decisions,
              input$years)
  })
  
  output$first_rejection <- renderText({
    first_rejection(input$Institutions,
              input$decisions,
              input$years)
  })
  
  output$first_waitlist <- renderText({
    first_waitlist(input$Institutions,
              input$decisions,
              input$years)
  })

  output$first_interview <- renderText({
    first_interview(input$Institutions,
              input$decisions,
              input$years)
  })
  
  observeEvent(input$resetAll, {
    reset("form")
  })
  
}

# Run app ----
shinyApp(ui, server)