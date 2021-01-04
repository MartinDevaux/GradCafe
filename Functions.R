load("cleaned_data.Rdata")

unique_institutions <- data %>% 
  select(institution) %>% 
  unique() %>%
  rename(Institution = institution) %>% 
  arrange(Institution)

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
           decision_year %in% years) %>%
    group_by(institution,
             decision_month_day) %>%
    arrange(decision) %>% 
    mutate(
      id = row_number()
    )

  
  plot <- ggplot(data = data_to_visualize,
                 aes(x = decision_month_day,
                     y = id,
                     text = str_wrap(string = paste(paste("Date received: ", format(decision_month_day, "%b %d"), ", ", decision_year, sep = ""), notes, sep = "<br>"),
                                     width = 45))) +
    geom_point(data = data_to_visualize,
               aes(color = decision),
               size = 2.5,
               alpha = .6) +
    scale_x_date(date_labels = "%b %d",
                 date_breaks = "2 week",
                 date_minor_breaks = "1 days") +
    scale_y_continuous(expand = c(0, 0), limits = c(0.5, max(25, max(data_to_visualize$id + 1)))) +
    scale_color_manual(breaks = c("Accepted", "Interview", "Wait listed", "Rejected", "Other"),
                       values = c("#33a02c", "#b2df8a", "#a6cee3", "#1f78b4", "gray5"),
                       name = "Decision type:") +
    coord_cartesian(xlim = as.Date(c("2020-01-01", "2020-05-01"))) +
    labs(x = "", y = "", title = paste("School:", institutions)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 30),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_line(color = "grey95")
    )
  
  
  ggplotly(plot, tooltip = c("text")) %>%
    layout(legend = list(orientation = "h", x = 0, y = -0.2))
  
}


# Key dates functions -----------------------------------------------------

first_acceptance <- function(institutions, decisions, years) {
  
  data_to_visualize <- data %>%
    filter(institution %in% institutions,
           decision %in% decisions,
           decision_year %in% years)
  
  key_dates <- data_to_visualize %>% 
    select(decision, decision_month_day) %>% 
    filter(decision %in% c("Accepted")) %>% 
      group_by(decision) %>% 
      summarize(
        first_date = min(decision_month_day)
      )
  
  if (dim(key_dates)[1] == 1) first_acceptance_date <- key_dates$first_date[[1]]
  
  if (exists(x = "first_acceptance_date") == T) {
    first_acceptance_text <- paste("First acceptance: ", format(first_acceptance_date, "%B %d"))
  }
  
}

first_rejection <- function(institutions, decisions, years) {
  
  data_to_visualize <- data %>%
    filter(institution %in% institutions,
           decision %in% decisions,
           decision_year %in% years)
  
  key_dates <- data_to_visualize %>% 
    select(decision, decision_month_day) %>% 
    filter(decision %in% c("Rejected")) %>% 
      group_by(decision) %>% 
      summarize(
        first_date = min(decision_month_day)
      )
  
  if (dim(key_dates)[1] == 1) first_rejection_date <- key_dates$first_date[[1]]
  
  if (exists(x = "first_rejection_date") == T) {
    first_rejection_text <- paste("First rejection: ", format(first_rejection_date, "%B %d"))
  }
  
}

first_waitlist <- function(institutions, decisions, years) {
  
  data_to_visualize <- data %>%
    filter(institution %in% institutions,
           decision %in% decisions,
           decision_year %in% years)
  
  key_dates <- data_to_visualize %>% 
    select(decision, decision_month_day) %>% 
    filter(decision %in% c("Wait listed")) %>% 
      group_by(decision) %>% 
      summarize(
        first_date = min(decision_month_day)
      )
  
  if (dim(key_dates)[1] == 1) first_waitlist_date <- key_dates$first_date[[1]]
  
  if (exists(x = "first_waitlist_date") == T) {
    first_waitlist_text <- paste("First waitlist: ", format(first_waitlist_date, "%B %d"))
  }
  
}

first_interview<- function(institutions, decisions, years) {
  
  data_to_visualize <- data %>%
    filter(institution %in% institutions,
           decision %in% decisions,
           decision_year %in% years)
  
  key_dates <- data_to_visualize %>% 
    select(decision, decision_month_day) %>% 
    filter(decision %in% c("Interview")) %>% 
      group_by(decision) %>% 
      summarize(
        first_date = min(decision_month_day)
      )
  
  if (dim(key_dates)[1] == 1) first_interview_date <- key_dates$first_date[[1]]
  
  if (exists(x = "first_interview_date") == T) {
    first_interview_text <- paste("First interview: ", format(first_interview_date, "%B %d"))
  }
  
}

