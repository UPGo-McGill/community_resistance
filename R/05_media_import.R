######################################### MEDIA IMPORT ###############################

source("R/01_helper_functions.R")

# Update cityname files for import to account for the two Glendales
cityname  <- c("Albuquerque", "Anaheim", "Anchorage", "Arlington", "Atlanta", 
               "Aurora", "Austin", "Bakersfield", "Baltimore",
               "Baton Rouge", "Birmingham", "Boise", "Boston",
               "Buffalo", "Cincinnati", "Chandler", "Charlotte", "Chesapeake",
               "Chicago", "Chula Vista", "Cleveland", "Colorado Springs", 
               "Columbus", "Corpus Christi",  "Dallas", "Denver", "Des Moines", 
               "Detroit", "Durham", "El Paso", "Fayetteville", "Fontana", 
               "Fort Wayne", "Fort Worth", "Fremont", "Fresno", "Garland", 
               "Gilbert", "Glendale", "Glendale", "Grand Rapids", 
               "Greensboro", "Henderson", "Hialeah", "Honolulu", "Houston", 
               "Huntington Beach", "Indianapolis", "Irvine", "Irving", "Jacksonville", 
               "Jersey City", "Kansas City", "Laredo", "Las Vegas", "Lexington", 
               "Lincoln", "Long Beach", "Los Angeles", "Louisville", "Lubbock", 
               "Madison", "Memphis", "Mesa", "Miami", "Milwaukee", "Minneapolis", "Modesto", 
               "Moreno Valley", "Nashville", "New Orleans", "New York", "Newark",
               "Norfolk", "North Las Vegas", "Oakland", "Oklahoma City", "Omaha",
               "Orlando", "Oxnard", "Philadelphia", "Phoenix", "Pittsburgh", "Plano", 
               "Portland", "Raleigh", "Reno", "Richmond", "Riverside", "Rochester", 
               "Sacramento", "Saint Louis", "Saint Paul", "Saint Petersburg", "Salt Lake City", 
               "San Antonio", "San Bernardino", "San Diego", "San Francisco", "San Jose", 
               "Santa Ana", "Santa Clarita", "Scottsdale", "Seattle", "Spokane", "Stockton", 
               "Tacoma", "Tampa", "Toledo", "Tucson", "Tulsa", "Virginia Beach", "Washington", 
               "Wichita", "Winston Salem")

# Import factiva and lexisnexis files

media <- 
  
  map(cityname, ~{
    
      import_lexisnexis(.x) %>% 
      dplyr::select(1:9) %>% 
      group_by(Author) %>% 
      filter(as.numeric(Word_Count) > 100) %>% 
      distinct(Headline, .keep_all = TRUE) %>% 
      ungroup()
    
  })


# Tidy text 

tidy_text <- map(media, ~{
  
  str_tidytext(.x)
  
})


# Create media and lemmatized_articles list to allow for further text analysis

media <- 
  map(tidy_text, ~{
    rbind(.x[[1]])
  })

lemmatized_articles <- 
  map(tidy_text, ~{
    rbind(.x[[2]])
  })
