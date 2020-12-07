setwd("/Users/martindevaux/Documents/R Path/GradCafe")

library(tidyverse)
library(lubridate)
library(forcats)
library(plotly)

source("Cleaning.R")

today <- date() %>%
  str_extract("[A-Za-z]+ [A-Za-z]+  [0-9]+") %>%
  str_remove("[A-Za-z]+ ") %>%
  as.Date("%b %d")

## Wrangle the data ----------------------------------------------------------------

# Filter out "others" and filter to January through may

decision_data <- 
  data %>%
  filter(date_post > "2012-01-01") %>% 
  mutate(
    date = as.Date(format(date_decision, "%d-%m"), format = "%d-%m")
  ) %>% 
  filter(decision %in% c("Accepted", "Interview", "Rejected", "Wait listed")) %>% 
  filter(date < "2020-05-01")

# Create an average date by school variable (to order the graph nicely)

date_mean <- decision_data %>%
  group_by(institution) %>% 
  summarize(
    mean_date = mean(date)
  )

decision_data <- left_join(decision_data, date_mean, by = "institution")

# Compute number of decisions posted

post_per_school_since_2012 <- decision_data %>% 
  group_by(institution) %>% 
  summarise(
    n()
  )

decision_data <- left_join(decision_data, post_per_school_since_2012, by = "institution")

# Filter out schools with less than 40 decisions posted and order by mean date

decision_data <- decision_data %>%
  filter(`n()` >= 40) %>% 
  mutate(institution = fct_reorder(institution, mean_date))


## Plot ----------------------------------------------------------------------

ggplot(data = decision_data,
       mapping = aes(x = date, y = institution)) +
  geom_point(aes(color = decision),
             alpha = .3,
             size = 2) +
  # geom_vline(aes(xintercept = today), color = "#F68060") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 week") +
  scale_y_discrete(limits = rev(levels(decision_data$institution))) +
  scale_color_manual(values = c("#4daf4a", "#984ea3", "#e41a1c", "#377eb8")) +
  labs(x = "", y = "", caption = "Political science: decision dates based on Grad Cafe data\nsince 2012 (schools with more than 40 datapoints)") +
  theme(
    axis.text.x=element_text(size = 8),
    axis.text.y=element_text(size = 8),
    legend.title = element_blank(),
    plot.caption = element_text(hjust = .5, size = 9)
      )

ggsave(file = str_c(today, "_decisions.pdf"),
       path = "Outputs",
       height = 11)

remove(decision_data, date_mean, post_per_school_since_2012)







## Just for the 2020 application season  ---------------------------------------------------

data_2020 <-
  data %>%
  filter(date_post > "2020-01-01") %>%
  mutate(
    date = as.Date(format(date_decision, "%d-%m"), format = "%d-%m")
  ) %>%
  filter(decision %in% c("Accepted", "Interview", "Rejected", "Wait listed")) %>%
  filter(date < "2020-05-01")

date_mean <- data_2020 %>%
  group_by(institution) %>%
  summarize(
    mean_date = mean(date)
  )

data_2020 <- left_join(data_2020, date_mean, by = "institution")

post_per_school_in_2020 <- data_2020 %>%
  group_by(institution) %>%
  summarise(
    n()
  )

data_2020 <- left_join(data_2020, post_per_school_in_2020, by = "institution")

data_2020 <- data_2020 %>%
  filter(`n()` >= 5) %>%
  mutate(institution = fct_reorder(institution, mean_date))


ggplot(data = data_2020,
       mapping = aes(x = date, y = institution)) +
  geom_point(aes(color = decision),
             alpha = .3,
             size = 2) +
  # geom_vline(aes(xintercept = today), color = "#F68060") +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 week") +
  scale_y_discrete(limits = rev(levels(data_2020$institution))) +
  scale_color_manual(values = c("#4daf4a", "#984ea3", "#e41a1c", "#377eb8")) +
  labs(x = "", y = "", caption = "Political science: decision dates based on Grad Cafe data\nsince 2012 (schools with more than 40 datapoints)") +
  theme(
    axis.text.x=element_text(size = 8),
    axis.text.y=element_text(size = 8),
    legend.title = element_blank(),
    plot.caption = element_text(hjust = .5, size = 9)
  )

ggsave(file = str_c(today, "_decisions_2020.pdf"),
       path = "Outputs",
       height = 11)
