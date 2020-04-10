######################################### COMPILE ###############################

source("R/01_helper_functions.R")

# Distinguish between Glendale AZ and Glendale CA
cityname[[32]] = "Glendale AZ"
cityname[[33]] = "Glendale CA"

# Cast geometries as polygons and add city name as a variable

neighbourhoods <-
  map2(neighbourhoods, cityname, ~{
    .x %>% 
      mutate(
        #geometry = st_union(st_collection_extract(geometry, "MULTIPOLYGON")),
             city = .y)
  })

# Create a dataframe from the list

neighbourhoods_table <- 
  do.call(rbind, neighbourhoods)

# Create a region variable

northeast <- c("New York", "Philadelphia", "Boston", "Pittsburgh",
               "Newark", "Jersey City", "Buffalo", "Rochester")

midwest <- c("Chicago", "Columbus", "Indianapolis", "Detroit", 
             "Milwaukee", "Kansas City", "Omaha", "Minneapolis",
             "Wichita", "Cleveland", "Saint Paul", "Saint Louis", 
             "Cincinnati", "Lincoln", "Toledo", "Fort Wayne", "Madison",
             "Des Moines", "Grand Rapids")

south <- c("Houston", "San Antonio", "Dallas", "Austin", 
           "Jacksonville", "Fort Worth", "Charlotte", "Washington",
           "El Paso", "Nashville", "Memphis", "Oklahoma City", "Louisville",
           "Baltimore", "Atlanta", "Miami", "Raleigh", "Virginia Beach",
           "Tulsa", "Arlington", "Tampa", "New Orleans", "Corpus Christi",
           "Lexington", "Greensboro", "Plano", "Orlando", "Durham", 
           "Saint Petersburg", "Laredo", "Lubbock", "Winston Salem",
           "Norfolk", "Chesapeake", "Garland", "Irving", "Hialeah", "Richmond",
           "Baton Rouge", "Birmingham", "Fayetteville")

west <- c("Albuquerque", "Anaheim", "Anchorage", "Los Angeles", "Phoenix",
          "San Diego", "San Jose", "San Francisco", "Seattle", "Denver",
          "Portland", "Las Vegas", "Tucson", "Fresno", "Mesa", "Sacramento",
          "Colorado Springs", "Long Beach", "Oakland", "Bakersfield",  "Aurora",
          "Honolulu", "Santa Ana", "Riverside", "Stockton", "Henderson", "Irvine",
          "Chula Vista", "Chandler", "Scottsdale", "Reno", "Glendale AZ", "Gilbert",
          "North Las Vegas", "Fremont", "Boise", "Spokane", "Tacoma", "San Bernardino",
          "Modesto", "Fontana", "Santa Clarita", "Glendale CA", "Oxnard", "Moreno Valley",
          "Huntington Beach", "Salt Lake City")

neighbourhoods_table <- 
  neighbourhoods_table %>% 
  mutate(region = ifelse(city %in% northeast, "northeast",
                         ifelse(city %in% midwest, "midwest",
                                ifelse(city %in% south, "south",
                                       ifelse(city %in% west, "west", "error"))))) 

# Manipulate the media file for temporal analysis

media <-
  map2(media, cityname, ~{
    .x %>% 
      mutate(
        city = .y)
  })

# Reduce to only the variables we will use

media <- 
  map(media, ~{
    .x %>% 
      dplyr::select(c("Date", "sentiment", "city"))
  })

# Create a dataframe from the list

media_table <- 
  do.call(rbind, media)

# Add regional variable

media_table <- 
  media_table %>% 
  mutate(region = ifelse(city %in% northeast, "northeast",
                         ifelse(city %in% midwest, "midwest",
                                ifelse(city %in% south, "south",
                                       ifelse(city %in% west, "west", "error"))))) 
