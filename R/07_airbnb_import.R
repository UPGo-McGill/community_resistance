######################################### AIRBNB DATA IMPORT ###############################

source("R/01_helper_functions.R")

# Set up date range
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2019-12-31")

# Set up database connection
upgo_connect()

# Edit citynames file such that they will work with the database
  # There are two glendales 
  # Quebec needs an accent

cityname_canada <- c("Calgary", "Edmonton", "Gatineau", "Halifax", 
                     "Hamilton", "Kitchener", "London", "Montreal", 
                     "Oshawa", "Ottawa", "Québec", "Regina", 
                     "Saskatoon", "St. Catharines", "Toronto", 
                     "Vancouver", "Victoria", "Windsor", "Winnipeg")

cityname_us <- c("Albuquerque", "Anaheim", "Anchorage", "Arlington", "Atlanta", 
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
                 "Orlando", "Oxnard", "Philadelphia", "Phoenix", "Pittsburg", "Plano", 
                 "Portland", "Raleigh", "Reno", "Richmond", "Riverside", "Rochester", 
                 "Sacramento", "Saint Louis", "Saint Paul", "Saint Petersburg", "Salt Lake City", 
                 "San Antonio", "San Bernardino", "San Diego", "San Francisco", "San Jose", 
                 "Santa Ana", "Santa Clarita", "Scottsdale", "Seattle", "Spokane", "Stockton", 
                 "Tacoma", "Tampa", "Toledo", "Tucson", "Tulsa", "Virginia Beach", "Washington", 
                 "Wichita", "Winston Salem")

cityname = c(cityname_canada, cityname_us)

# Import property and daily files from the database
property <- 
  map(cityname, ~{
  property_all %>% 
      filter(city == .x &
              (country == "Canada" | country == "United States")) %>% 
      collect() %>% 
      filter(!is.na(listing_type)) %>% 
      dplyr::select(property_ID:longitude, ab_property:ha_host, bedrooms) %>% 
      strr_as_sf(102009)
})

daily <- 
  map(property, ~{
    daily_all %>%
        filter(property_ID %in% !!.x$property_ID) %>%
        collect() %>% 
      strr_expand() %>% 
      filter(date >=created, date - 30 <= scraped, status != "U")
  })

ML_property <- 
  map(property, ~{
    property_all %>% 
      filter(host_ID %in% !! .x$host_ID) %>% 
      collect()
  })

ML_daily <- 
  map(ML_property, ~{
    daily_all %>%
      filter(property_ID %in% !!.x$property_ID) %>%
      collect() %>% 
      strr_expand() %>% 
      filter(date >=created, date - 30 <= scraped, status != "U")
  })

upgo_disconnect()

# Run the raffle to assign a neighbourhood to each listing
property <-
  map(seq_along(property), ~{
   property[[.x]] %>% 
      strr_raffle(neighbourhoods[[.x]], neighbourhood, households) 
  })

# Add last twelve months revenue
property <-
  map(seq_along(property), ~{
    daily[[.x]] %>% 
      filter(date > (ymd(end_date) - lubridate::years(1)), status == "R") %>% 
      group_by(property_ID) %>% 
      summarize(revenue_LTM = sum(price)) %>% 
      dplyr::select(property_ID, revenue_LTM) %>% 
      left_join(property[[.x]], .) %>% 
      dplyr::select(-geometry, everything(), geometry)
    
  })

LTM_property <- 
  map(property, ~{
    .x %>% 
      filter(created <= end_date, scraped > (ymd(end_date) - lubridate::years(1)))
  })


# Process multilistings
EH_ML <- 
  map(ML_daily, ~{
    .x %>% 
      filter(listing_type == "Entire home/apt") %>% 
      group_by(listing_type, host_ID, date) %>% 
      count() %>% 
      ungroup() %>% 
      filter(n >= 2) %>% 
      mutate(ML = TRUE)
  })
 
PR_ML <- 
  map(ML_daily, ~{
    .x %>% 
      filter(listing_type == "Private room") %>% 
      group_by(listing_type, host_ID, date) %>% 
      count() %>% 
      ungroup() %>% 
      filter(n >= 3) %>% 
      mutate(PR_ML = TRUE)
  })

daily <- 
  map(seq_along(EH_ML), ~{
    EH_ML[[.x]] %>% 
      dplyr::select(-n) %>% 
      left_join(daily[[.x]], .)
  })

daily <- 
  map(seq_along(PR_ML), ~{
    PR_ML[[.x]] %>% 
      dplyr::select(-n) %>% 
      left_join(daily[[.x]], .) %>% 
      mutate(ML = if_else(is.na(ML), PR_ML, ML)) %>% 
      mutate(ML = if_else(is.na(ML), FALSE, ML)) %>% 
      dplyr::select(-PR_ML)
  })

rm(EH_ML, PR_ML)

# Calculate FREH and GH listings
FREH <- 
  map(daily, ~{
    .x %>% 
      strr_FREH("2017-01-01", end_date) %>% 
      filter(FREH == TRUE) %>% 
      dplyr::select(-FREH)
  })

GH <- map(property, ~{
  .x %>% 
    strr_ghost(start_date = "2017-01-01", end_date = end_date)
})

# Calculate principal residence fields
property <- 
  map(seq_along(property), ~{
    property[[.x]] %>% 
      strr_principal_residence(daily[[.x]], FREH[[.x]], GH[[.x]], 
                               start_date = end_date, end_date = end_date)
  })


