#################################### AIRBNB BASIC FACTS ######################################

source("R/import_and_compile/01_helper_functions.R")

airbnb <- tibble(city = character(0), active_listings = numeric(0), active_listings_avg = numeric (0), EH_pct = numeric (0), 
                 revenue = numeric (0), GH = numeric (0), FREH = numeric (0),  housing_loss = numeric (0), revenue_10pct = numeric (0))
         

# Perform airbnb analysis for all neighbourhoods (currently for the city, need to filter to neighbourhoods)
n = 1

repeat{
  
  airbnb[n, 1] <- cityname
  
  airbnb[n, 2] <- daily %>% 
    filter(Date == end_date) %>% 
    group_by(Property_ID) %>% 
    nrow()
  
  airbnb[n, 3] <-   daily %>% 
    filter(Date <= end_date & Date >= start_date) %>% 
    group_by(Date) %>% 
    summarize(Listings = n()) %>%
    summarise(mean_Listings = mean(Listings))
  
  airbnb[n,4] <-   nrow(daily %>% 
                          filter(Date == end_date) %>% 
                          group_by(Property_ID) %>% 
                          filter(Listing_Type == "Entire home/apt"))/
                   nrow(daily %>% 
                          filter(Date == end_date))
  
  airbnb[n, 5] <-   daily %>% 
                       filter(Date <= end_date & 
                       Date >= start_date &
                      Status == "R" ) %>%
                      summarise(sum_revenue = sum(Price, na.rm = TRUE))
  
  airbnb[n, 6] <-  property %>% 
                      filter(GH == TRUE) %>% 
                      nrow()
  
  airbnb[n, 7] <- property %>% 
                      filter(FREH == TRUE) %>% 
                      nrow()
  
  airbnb[n, 8] <-  st_drop_geometry(strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, start_date,
                                               end_date, listing_type = Listing_Type) %>% 
                                      filter(date == end_date) %>% 
                                      group_by(ghost_ID) %>% 
                                      summarize(n = sum(housing_units)) %>% 
                                      ungroup() %>% 
                                      summarize(GH_housing_loss = sum(n))) +
    nrow(daily %>% 
           filter(Date == end_date) %>% 
           inner_join(property, .) %>% 
           filter(FREH == TRUE)) 
  
  airbnb[n, 9] <- daily %>%
                      filter(Date >= start_date, Date <= end_date, Status == "R") %>%
                      group_by(Airbnb_HID) %>%
                      summarize(rev = sum(Price)) %>%
                      filter(rev > 0) %>%
                  summarize(
                      `Top 10%` = sum(rev[rev > quantile(rev, c(0.90))] / sum(rev)))
    
  n = n+1
  
  if (n > length(neighbourhoods)) {
    break
  }
}

# Export as a table
write_csv(airbnb, "txt_files/airbnb/montreal.csv")
