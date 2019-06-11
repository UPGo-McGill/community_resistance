############## DATA IMPORT ###############################
source("R/01_helper_functions.R")

community_resistance_words = c("protest", "anti", "community-led", "affordability", 
                               "oppose",  "resist", "resistance")

# how to do this so all you have to input is city name and string of neighbourhood names
# from one data set? all articles about airbnb from whichever newssources - filter down by city name?

# Import txt file with airbnb + city name
# need to create a tidy table with the names and then the breakdowns and somehow incorporate that 
neighbourhoods <- c("williamsburg", "slope", "bedford", "bed-stuy", "stuyvesant", 
                    "crown", "flatbush", "dumbo", "bushwick")

city <- lnt_read("txt_files/airbnb_williamsburg.TXT") %>% 
  lnt_convert(to = "tidytext")

# Create a data table that calculates number of mentions and number of those that represent community opposition
n = 1

neighbourhood_resistance <- tibble(neighbourhoods = character(0), mentions = numeric(0), opposition = numeric(0)) 

repeat{
  neighbourhood_resistance[n, 1] <- neighbourhoods[n]
  neighbourhood_resistance[n,2] <- city %>% 
    filter(Token == neighbourhoods[n]) %>% 
    select("ID") %>% 
    distinct() %>% 
    nrow()
  neighbourhood_resistance[n,3] <- 
    city %>% 
    filter(Token %in% community_resistance_words) %>% 
    select("ID") %>% 
    inner_join(city %>% 
                 filter(Token == neighbourhoods[n]) %>% 
                 select("ID") %>% 
                 distinct(), .) %>% 
    distinct() %>% 
    nrow()
  
  n = n+1
  
  if (n > length(neighbourhoods)) {
    break
  }
}



