#################################### AIRBNB BASIC FACTS ######################################

source("R/01_import_general/01_helper_functions.R")

airbnb <- tibble(city = character(0), neighbourhood_name = character(0), active_listings = numeric(0), 
                 active_listings_avg = numeric (0), EH_pct = numeric (0), revenue = numeric (0), 
                 GH = numeric (0), FREH = numeric (0),  housing_loss = numeric (0), 
                 revenue_10pct = numeric (0))
         

# Perform airbnb analysis for all neighbourhoods 

n = 1

repeat{
  
  neighbourhood_property <- property %>% 
    st_transform(26918) %>% 
          st_join(neighbourhoods[n, "geometry"],
          join = st_within, left = FALSE)
  
  neighbourhood_daily <- daily %>% 
    filter(Property_ID %in% neighbourhood_property$Property_ID)
    
  airbnb[n, 1] <- cityname
  
  airbnb[n, 2] <- neighbourhoods$neighbourhood[n]
  
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
  
  airbnb[n, 7] <-  neighbourhood_property %>% 
                      filter(GH == TRUE) %>% 
                      nrow()
  
  airbnb[n, 8] <- neighbourhood_property %>% 
                      filter(FREH == TRUE) %>% 
                      nrow()
  
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
    
  n = n+1
  
  rm(neighbourhood_property, neighbourhood_daily, temp)
  
  if (n > nrow(neighbourhoods)) {
    break
  }
}

# Export as a table
save(airbnb, file = "airbnb/san_fran_no_buffer.Rdata")
