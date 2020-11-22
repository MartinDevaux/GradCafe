setwd("/Users/martindevaux/Documents/R Path/GradCafe")


# Loading packages --------------------------------------------------------

library(extrafont)
library(lubridate)
library(tidyverse)
library(plotly)
library(stringr) ## https://stackoverflow.com/questions/55643887/format-tooltip-in-plotly-for-long-text-labels
library(ggstance) ## https://stackoverflow.com/questions/52338137/vertical-equivalent-of-position-dodge-for-geom-point-on-categorical-scale

# Loading the clean data --------------------------------------------------

load("cleaned_data.Rdata")
 
parameter_year <- c("2019", "2020")
parameter_institution <- c("Columbia University")
parameter_decision <- c("Accepted", "Interview", "Rejected", "Wait listed")

data_to_visualize <- data %>%
  # filter(institution %in% parameter_institution,
  #        decision_year %in% parameter_year) %>% 
  group_by(institution, decision_year, decision_month_day) %>%
  mutate(
    id = row_number()
  )

key_dates <- data_to_visualize %>% 
  select(decision, decision_month_day) %>% 
  filter(decision %in% c("Accepted", "Rejected", "Wait listed")) %>% 
  group_by(decision) %>% 
  summarize(
    first_date = min(decision_month_day)
  )

first_acceptance <- key_dates %>% 
  filter(decision == "Accepted")
first_acceptance_date <- first_acceptance$first_date[[1]]

first_rejection <- key_dates %>% 
  filter(decision == "Rejected")
first_rejection_date <- first_rejection$first_date[[1]]

first_waitlist <- key_dates %>% 
  filter(decision == "Wait listed")
first_waitlist_date <- first_waitlist$first_date[[1]]

if (exists(x = "first_acceptance_date") == T) print(paste("First acceptance: ", format(first_acceptance_date, "%B %d")))
if (exists(x = "first_rejection_date") == T) print(paste("First rejection: ", format(first_rejection_date, "%B %d")))
if (exists(x = "first_waitlist_date") == T) print(paste("First waitlist: ", format(first_waitlist_date, "%B %d")))

# data_to_visualize <- data_to_visualize %>% 
#   mutate(
#     date = as.Date(decision_month_day)
#   )

# First option: scatter plot ----------------------------------------------

p <- ggplot(data = data_to_visualize,
       aes(x = decision_month_day,
           y = id,
           text = str_wrap(string = notes,
                           width = 40))) +
  geom_point(data = data_to_visualize,
             aes(color = decision)) +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 week") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(data_to_visualize$id) + 1)) +
  facet_wrap(~ decision_year,
             nrow = length(data_to_visualize$decision_year),
             strip.position = "left") +
  # scale_color_manual(values = c("#4daf4a", "#e41a1c", "#984ea3", "#377eb8")) +
  labs(x = "", y = "") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

ggplotly(p, tooltip = "text")

?position_dodge2v
?geom_dotplot

# Turn all this into a function -------------------------------------------

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



decision_calendar(years = parameter_year,
                  institutions =  parameter_institution,
                  decisions = parameter_decision)






