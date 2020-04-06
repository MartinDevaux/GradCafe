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
url_2 <- "https://www.thegradcafe.com/survey/index.php?q=Political+Science&t=a&o=&p=2"

# Get the table
result_posts_table_2 <- url_2 %>%
  read_html() %>%
  html_nodes(xpath = '//table[@class="submission-table"]') %>% 
  html_table() # Obtains a list

# Get the dataframe in that list
result_posts_table_2 <- result_posts_table_2[[1]]

# Append the two tables
result_posts_table <- rbind(result_posts_table, result_posts_table_2)

# Get rid of new objects
remove(url_2, result_posts_table_2)

## Turn that into a loop ------------------------------------------------

for(i in 3:pages) {
  # Import new page
  url_i <- str_c("https://www.thegradcafe.com/survey/index.php?q=Political+Science&t=a&o=&p=", as.character(i))
  
  # Get the table
  result_posts_table_i <- url_i %>%
    read_html() %>%
    html_nodes(xpath = '//table[@class="submission-table"]') %>% 
    html_table() # Obtains a list
  
  # Get the dataframe in that list
  result_posts_table_i <- result_posts_table_i[[1]]
  
  # Append the two tables
  result_posts_table <- rbind(result_posts_table, result_posts_table_i)
  
  # Get rid of new objects
  remove(url_i, result_posts_table_i)
  
  print(str_c(i, " out of ", pages))

}

remove(i)

## Get date/time of retrieval ------------------------------------------------

retrieval_time <- str_remove_all(str_extract(as.character(Sys.time()), "[0-9]+-[0-9]+-[0-9]+"), "-")

## Save the data  ------------------------------------------------------------

save(result_posts_table, file = str_c(retrieval_time,"_data.Rdata"))

remove(retrieval_time, result_posts_table, url, pages)
