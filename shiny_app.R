setwd("/Users/martindevaux/Documents/R Path/GradCafe")

library(shiny)
library(tidyverse)
library(plotly)
library(stringr)

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


# Dataviz function --------------------------------------------------------

decision_calendar <- function(institutions, decisions, years) {
  
  data_to_visualize <- data %>%
    filter(institution %in% institutions,
           decision %in% decisions,
           decision_year %in% years)
  
  plot <- ggplot(data = data_to_visualize,
                 aes(x = decision_month_day,
                     y = institution,
                     text = str_wrap(string = notes,
                                     width = 40))) +
    geom_jitter(data = data_to_visualize,
                aes(color = decision),
                alpha = .6,
                height = .1) +
    scale_color_manual(values = c("#4daf4a", "#e41a1c", "#984ea3", "#377eb8")) +
    labs(x = "", y = "")
  
  
  ggplotly(plot, tooltip = c("decision_month_day", "text"))
  
}



# User interface ----
ui <- fluidPage(
  titlePanel("PhD Admission Result Past Dates"),
  
  sidebarLayout(
    sidebarPanel(

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
                         selected = c("Accepted"))
      
    ),
    
    mainPanel(plotlyOutput("calendar"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$calendar <- renderPlotly({

    decision_calendar(input$institutions, input$decisions, input$years)

  })
}

# Run app ----
shinyApp(ui, server)