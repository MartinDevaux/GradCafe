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
  
  setBackgroundColor("#3d6594"),
  useShinyjs(),
  titlePanel(h1("Political science PhD admission: result dates", style = "color:white")),
  
  sidebarLayout(
    sidebarPanel(
      
      actionButton("resetAll", "Reset all"),
      
      div(
        id = "form",
        
        selectInput("Institutions", 
                    label = "Which school are you applying to?",
                    choices = unique_institutions,
                    multiple = F),
        
        checkboxGroupInput("years", 
                    label = "Include results from:",
                    choices = years[[1]],
                    selected = 2020),
        
        checkboxGroupInput("decisions", 
                           label = "Include posts including:",
                           choices = decisions[[1]],
                           selected = c("Accepted", "Rejected"))
      )
    ),
    
    mainPanel(
      fluidRow(
        column(11,
        panel(
          p(strong("Disclaimer: "), "This project was conducted independently of the ", a("GradCafe", href = "https://www.thegradcafe.com/"), " team and any mistake is the author's. None of the information displayed comes from official sources."),
          p("Work in progress. Contact the author on ", a("Twitter", href = "https://twitter.com/MartinDevaux"), " or ", a("GitHub", href = "https://github.com/MartinDevaux"), ".")
          ))
      ),
      fluidRow(
        column(11,
        panel(
          plotlyOutput("calendar_viz")
        ))
      ),
      fluidRow(
        column(width = 6,
        panel(
        textOutput("first_acceptance"),
        textOutput("first_rejection"),
        textOutput("first_∑interview"),
        textOutput("first_waitlist"),
        )),
        column(width = 5,
        img(src = "logo.png", align = "right", width = 305)
        )
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