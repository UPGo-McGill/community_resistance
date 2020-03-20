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

plan(multiprocess)

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
  map2(daily, host, strr_multi)

# Run the raffle to assign a neighbourhood to each listing

property <-
  map2(property, neighbourhoods, strr_raffle, neighbourhood, households, 
       seed = 10)

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
  # Note that I have changed the start dates such that this does not have
  # to be rerun for the temporal analysis

FREH <- map(daily, strr_FREH, "2015-01-01", end_date)
GH <- map(property, strr_ghost, start_date = "2015-01-01", end_date = end_date)

# Calculate principal residence fields

property <- 
  map(seq_along(property), ~{
    strr_principal_residence(
      property[[.x]], daily[[.x]], FREH[[.x]], GH[[.x]], start_date, end_date,
      PR_10, 0.1)
  })

property <- 
  map(seq_along(property), ~{
    strr_principal_residence(
      property[[.x]], daily[[.x]], FREH[[.x]], GH[[.x]], start_date, end_date,
      PR_25, 0.25)
  })

property <- 
  map(seq_along(property), ~{
    strr_principal_residence(
      property[[.x]], daily[[.x]], FREH[[.x]], GH[[.x]], start_date, end_date,
      PR_50, 0.5)
  })
