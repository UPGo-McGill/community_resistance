#################################### AIRBNB BASIC FACTS ######################################

source("R/01_helper_functions.R")

# ADD PRINCIPAL RESIDENCE PCT OF TOTAL LISTINGS

airbnb <- tibble(city = character(0), neighbourhood_name = character(0), active_listings = numeric(0), 
                 active_listings_avg = numeric (0), EH_pct = numeric (0), revenue = numeric (0), 
                 GH = numeric (0), FREH = numeric (0),  housing_loss = numeric (0), 
                 revenue_10pct = numeric (0))
         

# Perform airbnb analysis for all neighbourhoods 

for (n in c(1:nrow(neighbourhoods))) {
  
  neighbourhood_property <- property %>% 
    st_transform(transform) %>% 
          st_join(neighbourhoods[n, "geometry"],
          join = st_within, left = FALSE)
  
  neighbourhood_daily <- daily %>% 
    filter(Property_ID %in% neighbourhood_property$Property_ID)
    
  airbnb[n, 1] <- cityname
  
  airbnb[n, 2] <- neighbourhoods$neighbourhood[n]
  
  # change this to the daily method? or use the property method? check with Davids recent scripts. Update all.
  airbnb[n, 3] <- neighbourhood_property %>% 
    filter(Created <= end_date,
           Scraped >= end_date) %>% 
    group_by(Property_ID) %>% 
    nrow()
  
  airbnb[n, 4] <- neighbourhood_daily %>% 
    filter(Date <= end_date & Date >= start_date) %>% 
    group_by(Date) %>% 
    summarize(Listings = n()) %>%
    summarise(mean_Listings = mean(Listings))
  
  airbnb[n, 5] <-   nrow(neighbourhood_daily %>% 
                          filter(Date == end_date) %>% 
                          group_by(Property_ID) %>% 
                          filter(Listing_Type == "Entire home/apt"))/
                   nrow(neighbourhood_daily %>% 
                          filter(Date == end_date))
  
  airbnb[n, 6] <-   neighbourhood_daily %>% 
                       filter(Date <= end_date & 
                       Date >= start_date &
                      Status == "R" ) %>%
                      summarise(sum_revenue = sum(Price, na.rm = TRUE))
  
  airbnb[n, 7] <-                     nrow(neighbourhood_daily %>% 
                                             filter(Date == end_date) %>% 
                                             inner_join(neighbourhood_property, .) %>% 
                                             filter(GH == TRUE))
  
  airbnb[n, 8] <-                   nrow(neighbourhood_daily %>% 
                                           filter(Date == end_date) %>% 
                                           inner_join(neighbourhood_property, .) %>% 
                                           filter(FREH == TRUE))

  # rework this from GH list... cant ijust take the ones that are in neighbourhood_property
  temp <- strr_ghost(neighbourhood_property, Property_ID, Airbnb_HID, Created, Scraped, start_date,
                                      end_date, listing_type = Listing_Type) %>% 
                             filter(date == end_date) %>% 
                             group_by(ghost_ID) %>% 
                             summarize(n = sum(housing_units)) %>% 
                             ungroup()
  
  airbnb[n, 9] <- ifelse(nrow(temp) == 0, 0, temp %>% 
                            summarize(GH_housing_loss = sum(n))) %>% 
                            as.numeric() +
                   nrow(neighbourhood_daily %>% 
                            filter(Date == end_date) %>% 
                            inner_join(neighbourhood_property, .) %>% 
                            filter(FREH == TRUE)) 
  
  airbnb[n, 10] <- neighbourhood_daily %>%
                      filter(Date >= start_date, Date <= end_date, Status == "R") %>%
                      group_by(Airbnb_HID) %>%
                      summarize(rev = sum(Price)) %>%
                      filter(rev > 0) %>%
                  summarize(
                      `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)))
  
  
  rm(neighbourhood_property, neighbourhood_daily, temp)
}

# Export as a table
save(airbnb, file = ".Rdata")
