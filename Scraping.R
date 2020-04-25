# Set working directory
setwd("/Users/martindevaux/Documents/R Path/GradCafe")

# Load the main scraping package (install if not done) + tidyverse
library(rvest)
library(tidyverse)
library(lubridate)

## Load the main URL --------------------------------------------------

url <- "https://www.thegradcafe.com/survey/index.php?q=Political+Science"

## Scrape the number of pages in history ------------------------------

pages <- url %>% 
  read_html() %>% 
  html_nodes(xpath = '//div[@class="table-wrap"]') %>% 
  html_nodes(xpath = '//div[@class="admission-search pagination"]') %>% 
  html_text()
pages <- pages[1]

pages <-   str_trim(pages) %>% 
  str_sub(23, 36) %>% 
  str_extract("[1-9]+") %>% 
  as.numeric()

## Scrape the first-page table ----------------------------------------

# Get the table
result_posts_table <- url %>%
  read_html() %>%
  html_nodes(xpath = '//table[@class="submission-table"]') %>% 
  html_table() # Obtains a list

# Get the dataframe in that list
result_posts_table <- result_posts_table[[1]]

## Scrape the next-page table -----------------------------------------

# This section is not necessary (can start the loop at 2) but wanted
# to test whether the appending worked.

# Import new page
url <- "https://www.thegradcafe.com/survey/index.php?q=Political+Science&t=a&o=&p=2"

# Get the table
new_rows <- url %>%
  read_html() %>%
  html_nodes(xpath = '//table[@class="submission-table"]') %>% 
  html_table() # Obtains a list

# Get the dataframe in that list
new_rows <- new_rows[[1]]

# Append the two tables
result_posts_table <- bind_rows(result_posts_table, new_rows)

# Get rid of new objects
# remove(url, new_rows)

## Turn that into a loop ------------------------------------------------

for(i in 3:pages) {
  # Import new page
  url <- str_c("https://www.thegradcafe.com/survey/index.php?q=Political+Science&t=a&o=&p=", as.character(i))
  
  # Get the table
  new_rows <- url %>%
    read_html() %>%
    html_nodes(xpath = '//table[@class="submission-table"]') %>%
    html_table() # Obtains a list
  
  # Get the dataframe in that list
  # new_rows <- new_rows[[1]]
  
  # Append the two tables
  result_posts_table <- bind_rows(result_posts_table, new_rows[[1]])
  
  # Get rid of new objects
  # remove(url, new_rows)
  
  print(str_c(i, " out of ", pages))
  
}

remove(i, url, new_rows)

result_posts_table <- as_tibble(result_posts_table)

## Get date/time of retrieval ------------------------------------------------

retrieval_time <- Sys.time()
result_posts_table$`Time scraped` <- retrieval_time
## Save the data  ------------------------------------------------------------

save(result_posts_table, file = "raw_scraped_data.Rdata")

remove(retrieval_time, result_posts_table, pages)
