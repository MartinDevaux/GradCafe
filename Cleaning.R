setwd("/Users/martindevaux/Documents/R Path/GradCafe")

library(tidyverse)
library(lubridate)

## Getting the data from the scraping ------------------------------------------

load("raw_scraped_data.Rdata")

data <- result_posts_table

## Rename columns --------------------------------------------------------------

data <- data %>% 
  rename(program = `Program                                     (Season)`) %>% 
  rename(institution = Institution) %>%
  rename(decision_means_date = `Decision & 
                                    Date`) %>% 
  rename(origin = `St1`) %>% 
  rename(date_added = `Date Added`) %>% 
  rename(notes = Notes) %>%
  rename(time_scraped = `Time scraped`)

remove(result_posts_table)

## Filtering to political science PhDs only ------------------------------------

data <- data %>% 
  filter(str_detect(program, "Political Science")) %>%
  filter(str_detect(program, "PhD"))

## Only keeping top 100 US institutions ----------------------------------------

data <- data %>% 
  mutate(institution = replace(institution, str_detect(institution, "Harvard"), "Harvard University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Stanford"), "Stanford University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Standford University"), "Stanford University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Princeton"), "Princeton University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Prineton University"), "Princeton University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Prunceton"), "Princeton University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Berkeley"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Berleley"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UC Bekeley"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Uc Berekeley"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UC Berkekey"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UC BERKELEY"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UC Berkely"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UC Berkerley"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UC Berkley"), "UC-Berkeley")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Michigan State"), "MSU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Wester Michigan"), "WMU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Michigan"), "Michigan")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Yale"), "Yale University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "British Columbia"), "UBC")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Columbia"), "Columbia University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Columbai University"), "Columbia University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Duke"), "Duke University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "MIT"), "MIT")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Mit"), "MIT")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Massachusetts Institute Of Technology"), "MIT")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Massachusett Institute Of Technology"), "MIT")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Diego"), "UC-San Diego")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Ucsd"), "UC-San Diego")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UCSD"), "UC-San Diego")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UCSSD"), "UC-San Diego")) %>%
  mutate(institution = replace(institution, str_detect(institution, "ucsd"), "UC-San Diego")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UNC"), "UNC-Chapel Hill")) %>%
  mutate(institution = replace(institution, str_detect(institution, "North Carolina"), "UNC-Chapel Hill")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Chapel"), "UNC-Chapel Hill")) %>%
  mutate(institution = replace(institution, str_detect(institution, "NYU"), "NYU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "New York University"), "NYU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "New Yor University"), "NYU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "New York Univerisity"), "NYU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "New York Univeristy"), "NYU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "New York Univerity"), "NYU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "New York Universitty"), "NYU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "New Your University"), "NYU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UCLA"), "UC-Los Angeles")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Ucla"), "UC-Los Angeles")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Los Angeles"), "UC-Los Angeles")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UIC"), "UIC")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Illinois Chicago"), "UIC")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Illinois, Chicago"), "UIC")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Loyola"), "Loyola")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Chicago"), "UChicago")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Uchicago"), "UChicago")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Ohio"), "Ohio State University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "THE OHIO STATE UNIVERSITY"), "Ohio State University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "OSU"), "Ohio State University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Madison"), "University of Wisconsin-Madison")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Madi"), "University of Wisconsin-Madison")) %>%
  mutate(institution = replace(institution, str_detect(institution, "MADI"), "University of Wisconsin-Madison")) %>%
  mutate(institution = replace(institution, str_detect(institution, "madi"), "University of Wisconsin-Madison")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Macison"), "University of Wisconsin-Madison")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Davis"), "UC-Davis")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UC Daivs"), "UC-Davis")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Rochester"), "Rochester")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Cornell"), "Cornell")) %>%
  mutate(institution = replace(institution, str_detect(institution, "University Park"), "PSU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Penn State"), "PSU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Pennsylvania State"), "PSU")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Penn"), "UPenn")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Upenn"), "UPenn")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Austin"), "UT-Austin")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "WUSTL"), "WUSTL")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UMSL"), "UMSL")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Northwestern"), "Northwestern")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Northwester"), "Northwestern")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Northwetern University"), "Northwestern")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Emory"), "Emory University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Texas A&M"), "Texas A&M")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Teaxas A&M"), "Texas A&M")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Texas A & M"), "Texas A&M")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Texas A"), "Texas A&M")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Texas  A&M"), "Texas A&M")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "TAMU"), "Texas A&M")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UIUC"), "UIUC")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Champaign"), "UIUC")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Illinois UC"), "UIUC")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "University Of Illinois At UC"), "UIUC")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "University Of Illinois"), "UIUC")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Urbana"), "UIUC")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Minne"), "UMN")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "MINN"), "UMN")) %>%
  mutate(institution = replace(institution, str_detect(institution, "University Of Minessota"), "UMN")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Vanderbilt"), "Vanderbilt University")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Indiana"), "Indiana University-Bloomington")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Stony"), "Stony Brook University-SUNY")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Maryland"), "Maryland")) %>%
  mutate(institution = replace(institution, str_detect(institution, "College Park"), "Maryland")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Georgetown"), "Georgetown")) %>%
  mutate(institution = replace(institution, str_detect(institution, "GeorgeTown"), "Georgetown")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Georgetwon"), "Georgetown")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Rice"), "Rice")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Seattle"), "UW-Seattle")) %>%
  mutate(institution = replace(institution, str_detect(institution, "University of Washington"), "UW-Seattle")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Washington"), "UW-Seattle")) %>%
  mutate(institution = replace(institution, str_detect(institution, "University Of Washington"), "UW-Seattle")) %>%
  mutate(institution = replace(institution, str_detect(institution, "U Of Washington"), "UW-Seattle")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Iowa"), "University of Iowa")) %>%
  mutate(institution = replace(institution, str_detect(institution, "THE UNIVERSITY OF IOWA"), "University of Iowa")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Notre Dame"), "Notre Dame")) %>%
  mutate(institution = replace(institution, str_detect(institution, "NotreDame"), "Notre Dame")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Virginia"), "University of Virginia")) %>%
  mutate(institution = replace(institution, str_detect(institution, "UVA"), "University of Virginia")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Uva"), "University of Virginia")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Brown"), "Brown")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Florida State"), "FSU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Florida International"), "FIU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "South Florida"), "USF")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Florida"), "University of Florida")) %>%
  mutate(institution = replace(institution, str_detect(institution, "George Washington"), "GWU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "George Waahington University"), "GWU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "George Wahington University"), "GWU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "GeorgeWashington"), "GWU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "GEORGE WASHINGTON"), "GWU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "George Washingon University"), "GWU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Georgewashington University"), "GWU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "GWU"), "GWU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Boulder"), "University of Colorado-Boulder")) %>%
  mutate(institution = replace(institution, str_detect(institution, "CU Boudler"), "University of Colorado-Boulder")) %>%
  mutate(institution = replace(institution, str_detect(institution, "University Of Colorado"), "University of Colorado-Boulder")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Colorado State"), "CSU")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Colorado"), "University of Colorado-Boulder")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Pittsburgh"), "Pittsburgh")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Pittsburge University"), "Pittsburgh")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Rutgers"), "Rutgers-New Brunswick")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Ruters"), "Rutgers-New Brunswick")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Irvine"), "UC-Irvine")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UCI"), "UC-Irvine")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Georgia State"), "GSU")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Georgia Institute Of Technology"), "GIT")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Georgia"), "Georgia")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Riverside"), "UC-Riverside")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "RIVERSIDE"), "UC-Riverside")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Hopkins"), "Johns Hopkins University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "JHU"), "Johns Hopkins University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Syracuse"), "Syracuse University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Arizona State"), "ASU")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "ASU"), "ASU")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Arizona"), "University of Arizona")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Houston"), "University of Houston")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Southern California"), "USC")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Souther California"), "USC")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "American"), "American University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Boston University"), "Boston University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Boston Univ"), "Boston University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Boston Univerisity"), "Boston University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Boston Unversity"), "Boston University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Barbara"), "UC-Santa Barbara")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UC Santa Barbar"), "UC-Santa Barbara")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UCSB"), "UC-Santa Barbara")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "University Of Massachusetts"), "University of Massachusetts")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UMass"), "University of Massachusetts")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UMASS"), "University of Massachusetts")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Amherst"), "University of Massachusetts")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "North Texas"), "North Texas")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Boston College"), "Boston College")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Boston  College"), "Boston College")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Purdue"), "Purdue University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Oklahoma"), "Oklahoma")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "CUNY"), "CUNY")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Cuny"), "CUNY")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Cuny, Grad Center"), "CUNY")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "City University"), "CUNY")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Temple"), "Temple University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Buffalo"), "University at Buffalo-SUNY")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Connecticut"), "Connecticut")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Uconn"), "Connecticut")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UConn"), "Connecticut")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UCONN"), "Connecticut")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "University Of Connecticuit"), "Connecticut")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UCONN Storrs"), "Connecticut")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UConn (Storrs)"), "Connecticut")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Kansas"), "University of Kansas")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Nebraska"), "Nebraska")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Lincoln"), "Nebraska")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Oregon"), "University of Oregon")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UNIVERSITY OF OREGON"), "University of Oregon")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Claremont"), "Claremont Graduate University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Claremomt Graduate University"), "Claremont Graduate University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Mason"), "George Mason University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Albany"), "University at Albany-SUNY")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "SUNY ALBANY"), "University at Albany-SUNY")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Missouri"), "Missouri")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "South Carolina"), "South Carolina")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Tennessee"), "University of Tennessee-Knoxville")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "University Of Texas"), "UT-Dallas")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Milwaukee"), "University of Wisconsin-Milwaukee")) %>%
  mutate(institution = replace(institution, str_detect(institution, "University Of Wisconsin"), "University of Wisconsin-Madison")) %>% 
  mutate(institution = replace(institution, institution == "Wisconsin", "University of Wisconsin-Madison")) %>% 
  mutate(institution = replace(institution, institution == "GW", "GWU")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "The University Of Wisconsin"), "University of Wisconsin-Madison")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Brandeis"), "Brandeis University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Louisiana"), "LSU")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Northeastern"), "Northeastern University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Delaware"), "University of Delaware")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Kentucky"), "University of Kentucky")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "New Mexico"), "New Mexico")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Washington State"), "WSU")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "New School"), "New School")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Texas Tech"), "Texas Tech University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Tulane"), "Tulane University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Santa Cruz"), "UC-Santa Cruz")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UC Santa Cluz"), "UC-Santa Cruz")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "UCSC"), "UC-Santa Cruz")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Mississippi"), "Mississippi")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Utah"), "Utah")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Wayne"), "Wayne State University")) %>% 
  mutate(institution = replace(institution, str_detect(institution, "Alabama"), "Alabama")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Cincinnati"), "University of Cincinnati")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Binghamton"), "Binghamton University-SUNY")) %>%
  mutate(institution = replace(institution, str_detect(institution, "SUNY Binghamnton University"), "Binghamton University-SUNY")) %>%
  mutate(institution = replace(institution, str_detect(institution, "SUNY Binghampton University"), "Binghamton University-SUNY")) %>%
  mutate(institution = replace(institution, str_detect(institution, "SUNY-Binghamton"), "Binghamton University-SUNY")) %>%
  mutate(institution = replace(institution, str_detect(institution, "SUNY-Binhamton"), "Binghamton University-SUNY")) %>%
  mutate(institution = replace(institution, str_detect(institution, "Hawaii"), "University of Hawaii-Manoa"))

data <- data %>% 
  filter(institution != "All") %>% 
  filter(institution != "All Schools") %>% 
  filter(institution != "Argosy University - Schaumburg") %>% 
  filter(institution != "Australian National University") %>% 
  filter(institution != "Baylor") %>%
  filter(institution != "University Of Toronto") %>% 
  filter(institution != "Queens University") %>% 
  filter(institution != "Queens (Canada)") %>% 
  filter(institution != "McGill University") %>% 
  filter(institution != "University Of Denver") %>% 
  filter(institution != "The University Of Toronto") %>% 
  filter(institution != "London School Of Economics & Political Science (LSE)") %>% 
  filter(institution != "Iqtisad Uni") %>% 
  filter(institution != "London School Of Economics (LSE)") %>% 
  filter(institution != "Graduate Institute Geneva (IHEID)") %>% 
  filter(institution != "UC Merced") %>% 
  filter(institution != "UC MERCED") %>% 
  filter(institution != "Ch") %>% 
  filter(institution != "University Of Ca") %>% 
  filter(institution != "Baylor University") %>% 
  filter(institution != "Central European University (CEU)") %>% 
  filter(!str_detect(institution, "Mannheim")) %>% 
  filter(!str_detect(institution, "Merced")) %>% 
  filter(!str_detect(institution, "Baptist")) %>% 
  filter(!str_detect(institution, "Cambridge")) %>% 
  filter(!str_detect(institution, "Oxford")) %>% 
  filter(!str_detect(institution, "Catholic")) %>% 
  filter(!str_detect(institution, "Caltech")) %>% 
  filter(!str_detect(institution, "Canada")) %>% 
  filter(!str_detect(institution, "UK")) %>% 
  filter(!str_detect(institution, "European")) %>% 
  filter(!str_detect(institution, "Toronto")) %>% 
  filter(!str_detect(institution, "Wherever")) %>% 
  filter(!str_detect(institution, "Victoria")) %>% 
  filter(!str_detect(institution, "Andrews")) %>% 
  filter(!str_detect(institution, "Ottawa")) %>% 
  filter(!str_detect(institution, "Las Vegas")) %>% 
  filter(!str_detect(institution, "Melbourne")) %>% 
  filter(!str_detect(institution, "Australia")) %>% 
  filter(!str_detect(institution, "Knocks")) %>% 
  filter(!str_detect(institution, "Leeds")) %>% 
  filter(!str_detect(institution, "Gothenburg")) %>% 
  filter(!str_detect(institution, "Mc")) %>% 
  filter(!str_detect(institution, "LSE")) %>% 
  filter(!str_detect(institution, "London")) %>% 
  filter(!str_detect(institution, "Erasmus")) %>% 
  filter(!str_detect(institution, "HELP")) %>% 
  filter(!str_detect(institution, "Howard")) %>% 
  filter(!str_detect(institution, "Josef")) %>% 
  filter(!str_detect(institution, "Lse")) %>% 
  filter(!str_detect(institution, "Singapore")) %>% 
  filter(!str_detect(institution, "NSF GRFP")) %>% 
  filter(!str_detect(institution, "Dominion")) %>% 
  filter(!str_detect(institution, "Queens")) %>% 
  filter(!str_detect(institution, "Carleton")) %>% 
  filter(!str_detect(institution, "Concordia")) %>% 
  filter(!str_detect(institution, "EUI")) %>% 
  filter(!str_detect(institution, "FIU")) %>% 
  filter(!str_detect(institution, "GIT")) %>% 
  filter(!str_detect(institution, "Graduate Center")) %>% 
  filter(!str_detect(institution, "International And Development")) %>% 
  filter(!str_detect(institution, "Internaional And")) %>% 
  filter(!str_detect(institution, "Guelph")) %>% 
  filter(!str_detect(institution, "Kent Sate")) %>% 
  filter(!str_detect(institution, "Kent State")) %>% 
  filter(!str_detect(institution, "Lund")) %>% 
  filter(!str_detect(institution, "Sinagpore")) %>% 
  filter(!str_detect(institution, "Northern Illinois")) %>% 
  filter(!str_detect(institution, "Northern-Illinois")) %>% 
  filter(!str_detect(institution, "Edinburgh")) %>% 
  filter(!str_detect(institution, "Exeter")) %>% 
  filter(!str_detect(institution, "Calgary")) %>% 
  filter(!str_detect(institution, "UBC")) %>% 
  filter(!str_detect(institution, "Bamberg")) %>% 
  filter(!str_detect(institution, "Other")) %>% 
  filter(!str_detect(institution, "Oriental And African")) %>% 
  filter(!str_detect(institution, "Fraser")) %>% 
  filter(!str_detect(institution, "Denver")) %>% 
  filter(!str_detect(institution, "Manchester")) %>% 
  filter(!str_detect(institution, "Toront")) %>% 
  filter(!str_detect(institution, "Tufts")) %>% 
  filter(!str_detect(institution, "South Wales")) %>% 
  filter(!str_detect(institution, "Patna")) %>% 
  filter(!str_detect(institution, "Tornoto")) %>% 
  filter(!str_detect(institution, "Ontario")) %>% 
  filter(!str_detect(institution, "USF")) %>% 
  filter(!str_detect(institution, "Uvic")) %>% 
  filter(!str_detect(institution, "York  University")) %>% 
  filter(!str_detect(institution, "York University")) %>% 
  filter(!str_detect(institution, "York Univetsit")) %>% 
  filter(!str_detect(institution, "Montréal")) %>% 
  filter(!str_detect(institution, "Dublin")) %>% 
  filter(!str_detect(institution, "Alberta")) %>% 
  filter(!str_detect(institution, "Carbondale")) %>% 
  filter(!str_detect(institution, "Henry Jackson")) %>%
  filter(!str_detect(institution, "Dalhousie"))
  
data <- data %>% 
  mutate(institution = replace(institution, institution == "ASU", "Arizona State University")) %>%
  mutate(institution = replace(institution, institution == "Alabama", "University of Alabama")) %>%
  mutate(institution = replace(institution, institution == "Brown", "Brown University")) %>%
  mutate(institution = replace(institution, institution == "Connecticut", "University of Connecticut")) %>%
  mutate(institution = replace(institution, institution == "Cornell", "Cornell University")) %>%
  mutate(institution = replace(institution, institution == "CSU", "Colorado State University")) %>%
  mutate(institution = replace(institution, institution == "CUNY", "City University of New York")) %>%
  mutate(institution = replace(institution, institution == "FSU", "Florida State University")) %>%
  mutate(institution = replace(institution, institution == "Georgia", "University of Georgia")) %>%
  mutate(institution = replace(institution, institution == "University Of Georiga", "University of Georgia")) %>%
  mutate(institution = replace(institution, institution == "GSU", "Georgia State University")) %>%
  mutate(institution = replace(institution, institution == "GWU", "George Washington University")) %>%
  mutate(institution = replace(institution, institution == "Georgetown", "Georgetown University")) %>%
  mutate(institution = replace(institution, institution == "Loyola", "Loyola University Chicago")) %>%
  mutate(institution = replace(institution, institution == "LSU", "Louisiana State University-Baton Rouge")) %>%
  mutate(institution = replace(institution, institution == "MIT", "Massachusetts Institute of Technology")) %>%
  mutate(institution = replace(institution, institution == "Maryland", "University of Maryland-College Park")) %>% 
  mutate(institution = replace(institution, institution == "UMD", "University of Maryland-College Park")) %>% 
  mutate(institution = replace(institution, institution == "Michigan", "University of Michigan-Ann Arbor")) %>% 
  mutate(institution = replace(institution, institution == "Umich", "University of Michigan-Ann Arbor")) %>% 
  mutate(institution = replace(institution, institution == "University Of Michican, Ann Harbor", "University of Michigan-Ann Arbor")) %>% 
  mutate(institution = replace(institution, institution == "University Of Michicgan", "University of Michigan-Ann Arbor")) %>% 
  mutate(institution = replace(institution, institution == "University Of Michign Ann Arbor", "University of Michigan-Ann Arbor")) %>% 
  mutate(institution = replace(institution, institution == "UMich @ Ann Arbor", "University of Michigan-Ann Arbor")) %>% 
  mutate(institution = replace(institution, institution == "Mississippi", "University of Mississippi")) %>% 
  mutate(institution = replace(institution, institution == "Missouri", "University of Missouri")) %>% 
  mutate(institution = replace(institution, institution == "MSU", "Michigan State University")) %>% 
  mutate(institution = replace(institution, institution == "Nebraska", "University of Nebraska-Lincoln")) %>% 
  mutate(institution = replace(institution, institution == "New Mexico", "University of New Mexico")) %>% 
  mutate(institution = replace(institution, institution == "NYU", "New York University")) %>% 
  mutate(institution = replace(institution, institution == "Northwestern", "Northwestern University")) %>% 
  mutate(institution = replace(institution, institution == "Notre Dame", "University of Notre Dame")) %>% 
  mutate(institution = replace(institution, institution == "North Texas", "University of North Texas")) %>% 
  mutate(institution = replace(institution, institution == "Oklahoma", "University of Oklahoma")) %>% 
  mutate(institution = replace(institution, institution == "University Of Oklhaoma", "University of Oklahoma")) %>% 
  mutate(institution = replace(institution, institution == "Pittsburgh", "University of Pittsburgh")) %>% 
  mutate(institution = replace(institution, institution == "University Of Pittsurgh", "University of Pittsburgh")) %>% 
  mutate(institution = replace(institution, institution == "PSU", "Pennsylvania State University-University Park")) %>% 
  mutate(institution = replace(institution, institution == "Purdue University", "Purdue University-West Lafayette")) %>% 
  mutate(institution = replace(institution, institution == "Rice", "Rice University")) %>% 
  mutate(institution = replace(institution, institution == "Rochester", "University of Rochester")) %>% 
  mutate(institution = replace(institution, institution == "University Of Rochesteer", "University of Rochester")) %>% 
  mutate(institution = replace(institution, institution == "South Carolina", "University of South Carolina")) %>% 
  mutate(institution = replace(institution, institution == "Southern Illinois University - Carbondale", "Southern Illinois University-Carbondale")) %>% 
  mutate(institution = replace(institution, institution == "Texas A&M", "Texas A&M University-College Station")) %>% 
  mutate(institution = replace(institution, institution == "UC-Berkeley", "University of California-Berkeley")) %>% 
  mutate(institution = replace(institution, institution == "UC-Davis", "University of California-Davis")) %>% 
  mutate(institution = replace(institution, institution == "UC-Irvine", "University of California-Irvine")) %>% 
  mutate(institution = replace(institution, institution == "UC-Los Angeles", "University of California-Los Angeles")) %>% 
  mutate(institution = replace(institution, institution == "UC-Riverside", "University of California-Riverside")) %>% 
  mutate(institution = replace(institution, institution == "UC-San Diego", "University of California-San Diego")) %>% 
  mutate(institution = replace(institution, institution == "UC-Santa Barbara", "University of California-Santa Barbara")) %>% 
  mutate(institution = replace(institution, institution == "UC-Santa Cruz", "University of California-Santa Cruz")) %>% 
  mutate(institution = replace(institution, institution == "UChicago", "University of Chicago")) %>% 
  mutate(institution = replace(institution, institution == "UIC", "University of Illinois-Chicago")) %>% 
  mutate(institution = replace(institution, institution == "UIUC", "University of Illinois-Urbana-Champaign")) %>% 
  mutate(institution = replace(institution, institution == "UMN", "University of Minnesota-Twin Cities")) %>% 
  mutate(institution = replace(institution, institution == "UMSL", "University of Missouri-St. Louis")) %>% 
  mutate(institution = replace(institution, institution == "WUSTL", "Washington University in St. Louis")) %>% 
  mutate(institution = replace(institution, institution == "UPenn", "University of Pennsylvania")) %>% 
  mutate(institution = replace(institution, institution == "USC", "University of Southern California")) %>% 
  mutate(institution = replace(institution, institution == "UT-Dallas", "University of Texas-Dallas")) %>% 
  mutate(institution = replace(institution, institution == "UT-Austin", "University of Texas-Austin")) %>% 
  mutate(institution = replace(institution, institution == "Utah", "University of Utah")) %>% 
  mutate(institution = replace(institution, institution == "UW-Seattle", "University of Washington")) %>% 
  mutate(institution = replace(institution, institution == "WSU", "Washington State University")) %>% 
  mutate(institution = replace(institution, institution == "UNC-Chapel Hill", "University of North Carolina--Chapel Hill"))

## Decisions  --------------------------------------------------------

data <- data %>% 
  mutate(
    decision = str_remove(str_extract(decision_means_date, "^[A-Za-z]+ [A-Za-z ]*via"), " via")
    )

## Means  --------------------------------------------------------

data <- data %>% 
  mutate(
    means = str_remove(str_remove(str_extract(decision_means_date, "via .+ on"), "via "), " on")
  )

## Date received --------------------------------------------------------

data <- data %>% 
  mutate(
    date_received = str_extract(decision_means_date, "[0-9]+ [A-z][a-z]+ [0-9]+")
  )

## Undergrad GPA --------------------------------------------------------

data <- data %>% 
  mutate(
    GPA = str_remove(str_extract(decision_means_date, "Undergrad GPA: [0-9]\\.[0-9]+"), "Undergrad GPA: ")
  )

data$GPA <- as.numeric(data$GPA)

data <- data %>% 
  mutate(GPA = replace(GPA, GPA ==0, NA)) %>% 
  mutate(GPA = replace(GPA, GPA > 5, NA))

## GRE scores ----------------------------------------------------------------------

data <- data %>% 
  mutate(
    GRE = str_extract(decision_means_date, "[0-9]+/[0-9]+/[0-9]\\.[0-9]+")
  )

# Verbal
data <- data %>% 
  mutate(
    GRE_V = str_remove(GRE, "/[0-9]+/[0-9]\\.[0-9]+")
  )
data$GRE_V <- as.numeric(data$GRE_V)

data <- data %>% 
  mutate(GRE_V = replace(GRE_V, GRE_V <130, NA)) %>% 
  mutate(GRE_V = replace(GRE_V, GRE_V > 170, NA))

# Quant
data <- data %>% 
  mutate(
    GRE_Q = str_remove_all(str_extract(GRE, "/[0-9]+/"), "/")
  )
data$GRE_Q <- as.numeric(data$GRE_Q)

data <- data %>% 
  mutate(GRE_Q = replace(GRE_Q, GRE_Q <130, NA)) %>% 
  mutate(GRE_Q = replace(GRE_Q, GRE_Q > 170, NA))

# Writing
data <- data %>% 
  mutate(
    GRE_W = str_extract(GRE, "[0-9]\\.[0-9]+")
  )
data$GRE_W <- as.numeric(data$GRE_W)

data <- data %>% 
  mutate(GRE_W = replace(GRE_W, GRE_W < 0, NA)) %>% 
  mutate(GRE_W = replace(GRE_W, GRE_W > 6, NA))

## Remove superfluous variable ---------------------------------------------------

data <- data %>% 
  select(-c(GRE, decision_means_date))


## Get date format for dates ---------------------------------------------------

data <- data %>% 
  filter(date_received != "0 Feb 2014")

data <- data %>% 
  mutate(
    decision_year = str_extract(date_received, "[0-9][0-9][0-9][0-9]"),
    decision_year = factor(decision_year),
    decision_year = factor(decision_year, levels = rev(levels(decision_year))),
    decision_month_day = str_remove(date_received, " [0-9][0-9][0-9][0-9]"),
    decision_month_day = parse_date_time(decision_month_day, orders = "dm"),
    decision_month_day = as.Date(decision_month_day)
  )


data <- data %>% 
  mutate(
    post_year = str_extract(date_added, "[0-9][0-9][0-9][0-9]"),
    # post_year = as.integer(post_year),
    post_month_day = str_remove(date_added, " [0-9][0-9][0-9][0-9]"),
    post_month_day = parse_date_time(post_month_day, orders = "dm")
  )

# Remove strings
data <- data %>% 
  select(-c(date_added, date_received))

save(data, file = "cleaned_data.Rdata")


