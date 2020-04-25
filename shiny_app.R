setwd("/Users/martindevaux/Documents/R Path/GradCafe")

library(shiny)
library(tidyverse)
library(plotly)
library(stringr)
library(forcats)

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
                     y = factor(decision_year),
                     text = str_wrap(string = paste(paste("Date received:", format(decision_month_day, "%b %d")), notes, sep = "<br>"),
                                     width = 40))) +
    geom_jitter(data = data_to_visualize,
                aes(color = decision),
                alpha = .6,
                height = .05,
                width = 0) +
    scale_x_date(date_labels = "%b %d", date_breaks = "2 week") +
    scale_color_manual(breaks = c("Accepted", "Interview", "Wait listed", "Rejected", "Other"),
                        values = c("#4daf4a", "#984ea3", "#377eb8", "#e41a1c", "#ff7f00")) +
    coord_cartesian(xlim = as.Date(c("2020-01-01", "2020-05-01"))) +
    labs(x = "", y = "", title = institutions) +
    theme(
      axis.text.x = element_text(angle = 30)
    )
  
  
  ggplotly(plot, tooltip = c("text"))
  
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
                         selected = c("Accepted", "Interview", "Wait listed", "Rejected", "Other"))
      
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