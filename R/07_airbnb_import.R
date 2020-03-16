######################################### AIRBNB DATA IMPORT ###############################

source("R/01_helper_functions.R")

# Set up date range
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2019-12-31")

# Set up database connection
upgo_connect()

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
      strr_expand() 
    })

host <- 
  map(property, ~{
    host_all %>% 
      filter(host_ID %in% !!.x$host_ID) %>% 
      collect() %>% 
      strr_expand()
  })

upgo_disconnect()

# Process multilistings
daily <-
  map(seq_along(daily), ~{
    strr_multi(daily[[.x]], host[[.x]])
  })

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

# Calculate FREH and GH listings
FREH <- 
  map(daily, ~{
    .x %>% 
      strr_FREH("2015-01-01", end_date) %>% 
      filter(FREH == TRUE) %>% 
      dplyr::select(-FREH)
  })

GH <- map(property, ~{
  .x %>% 
    strr_ghost(start_date = "2015-01-01", end_date = end_date)
})

# Calculate principal residence fields
property <- 
  map(seq_along(property), ~{
    property[[.x]] %>% 
      strr_principal_residence(daily[[.x]], FREH[[.x]], GH[[.x]], 
                               start_date = end_date, end_date = end_date, 
                               sensitivity = 0.5)
  })
