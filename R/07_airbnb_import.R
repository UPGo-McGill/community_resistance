#### 07. AIRBNB DATA IMPORT ####################################################

source("R/01_helper_functions.R")

# Set up date range
start_date <- as.Date("2019-01-01")
end_date <- as.Date("2019-12-31")


## Import property and daily files from the database

upgo_connect()

property <- 
  map(cityname, ~{
  property_all %>% 
      filter(city == .x, country == "United States") %>% 
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


## Process multilistings

daily <-
  map2(daily, host, strr_multi)


## Run the raffle to assign a neighbourhood to each listing

property2 <-
  map2(property[1:30], neighbourhoods[1:30], strr_raffle, neighbourhood, 
       households, seed = 10)

property3 <-
  map2(property[31:60], neighbourhoods[31:60], strr_raffle, neighbourhood, 
       households, seed = 10)

property4 <-
  map2(property[61:90], neighbourhoods[61:90], strr_raffle, neighbourhood, 
       households, seed = 10)

property5 <-
  map2(property[91:115], neighbourhoods[91:115], strr_raffle, neighbourhood, 
       households, seed = 10)

property <- 
  c(property2, property3, property4, property5)

rm(property2, property3, property4, property5)


## Add last twelve months revenue

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
  map(property, filter, created <= end_date, 
      scraped > (ymd(end_date) - lubridate::years(1)))


## Calculate FREH and GH listings
  # Note that I have changed the start dates such that this does not have
  # to be rerun for the temporal analysis

FREH_1 <- map(daily[1:40], strr_FREH, "2015-01-01", end_date)
FREH_2 <- map(daily[41:80], strr_FREH, "2015-01-01", end_date)
FREH_3 <- map(daily[81:115], strr_FREH, "2015-01-01", end_date)

FREH <- c(FREH_1, FREH_2, FREH_3)
rm(FREH_1, FREH_2, FREH_3)

GH_1 <- map(property[1:40], strr_ghost, "2015-01-01", end_date)
GH_2 <- map(property[41:80], strr_ghost, "2015-01-01", end_date)
GH_3 <- map(property[81:115], strr_ghost, "2015-01-01", end_date)

GH <- c(GH_1, GH_2, GH_3)
rm(GH_1, GH_2, GH_3)


## Calculate principal residence fields

start_date <- as.Date("2019-01-01")

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

save(property, LTM_property, daily, host, FREH, GH, 
     file = "data/str_data.Rdata")
