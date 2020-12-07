library(tidyverse)
library(stargazer)

load("cleaned_data.Rdata")

data <- as_tibble(data)

data_for_lm <- data %>%
  filter(
    decision %in% c("Accepted", "Rejected"),
    is.na(GPA) == F
    ) %>% 
  mutate(
    decision = if_else(decision == "Accepted", 1, 0)
  )

reg1 <- lm(data = data_for_lm,
   decision ~ GPA+GRE_Q+GRE_V+GRE_W)

stargazer(reg1, type = "text")
