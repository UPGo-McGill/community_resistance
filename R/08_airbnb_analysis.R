property <- map(property_hold, ~{
  
  .x[1:500,]})

end_date <- as.Date("2019-06-15")

# active_listings on end date
neighbourhoods[[1]] <- property[[1]] %>% 
  filter(housing == TRUE) %>% 
  filter(created <= end_date,
         scraped >= end_date) %>% 
  group_by(neighbourhood) %>% 
  tally() %>% 
  st_drop_geometry() %>% 
  left_join(neighbourhoods[[1]], .) %>% 
  mutate(active_listings = n) %>% 
  dplyr::select(-n)

# active_listings_avg over the past twelve months
neighbourhoods[[1]] <- daily[[1]] %>% 
  filter(date <= end_date &
           date >= start_date) %>% 
  left_join(property[[1]] %>% 
              dplyr::select(c("property_ID", "neighbourhood"))) %>% 
  group_by(neighbourhood,date) %>% 
  summarize(listings = n()) %>% 
  summarize(active_listings_avg = mean(listings)) %>% 
  left_join(neighbourhoods[[1]], .)

# active_listings_yoy growth rate from one year to the next
neighbourhoods[[1]] <- property[[1]] %>% 
  filter(housing == TRUE) %>% 
  filter(created <= ymd(end_date) - years(1),
         scraped >= ymd(end_date) - years(1)) %>% 
  group_by(neighbourhood) %>% 
  tally() %>% 
  st_drop_geometry() %>% 
  left_join(neighbourhoods[[1]], .) %>% 
  mutate(active_listings_prev = n, 
         active_listings_yoy = active_listings/active_listings_prev) %>% 
  dplyr::select(-c(n, active_listings_prev))


# EH and EH_pct - distributin of listing types on end date
neighbourhoods[[1]] <- property[[1]] %>% 
  filter(housing == TRUE, 
         listing_type == "Entire home/apt",
         created <= end_date,
         scraped >= end_date) %>% 
  group_by(neighbourhood) %>% 
  tally() %>% 
  st_drop_geometry() %>% 
  left_join(neighbourhoods[[1]], .) %>% 
  mutate(EH = n, 
         EH_pct = n/active_listings) %>% 
  dplyr::select(-n)

# revenue_LTM and the average revenue per listing over the past twelve months
neighbourhoods[[1]] <- LTM_property[[1]] %>% 
  group_by(neighbourhood) %>% 
  summarize(revenue_LTM = sum(revenue_LTM, na.rm = TRUE)) %>% 
  st_drop_geometry() %>% 
  left_join(neighbourhoods[[1]], .) %>% 
  mutate(revenue_LTM_per_listing = revenue_LTM/active_listings_avg)

# GH in terms of listing count (not housing units) on the end date
neighbourhoods[[1]] <- GH[[1]] %>% 
  filter(date == start_date) %>%
  dplyr::select(ghost_ID, listing_count, housing_units, property_IDs) %>% 
  st_centroid() %>% 
  strr_raffle(neighbourhoods[[1]], neighbourhood, households) %>% 
  group_by(neighbourhood) %>% 
  summarize(GH = sum(listing_count)) %>% 
  st_drop_geometry() %>% 
  left_join(neighbourhoods[[1]], .) %>% 
  mutate(GH_pct = GH/active_listings)

# FREH status on the end date, but taken as a percentage in terms of active listings
  # over the past year since the calculation invovles the past year as a time frame
neighbourhoods[[1]] <- property[[1]] %>% 
  dplyr::select(c("property_ID", "neighbourhood")) %>% 
  right_join(FREH[[1]] %>% 
           filter(date == end_date) %>% 
             dplyr::select(property_ID)) %>% 
  group_by(neighbourhood) %>% 
  count() %>% 
  st_drop_geometry() %>% 
  left_join(neighbourhoods[[1]], .) %>% 
  mutate(FREH = n, 
         FREH_pct = n/active_listings_avg) %>% 
  dplyr::select(-n)
  

# MLs on the end date
neighbourhoods[[1]] <- daily[[1]] %>% 
  filter(date == end_date) %>% 
  filter(ML == TRUE) %>% 
  left_join(property[[1]] %>% 
              dplyr::select(c("property_ID", "neighbourhood"))) %>% 
  group_by(neighbourhood) %>% 
  count() %>% 
  left_join(neighbourhoods[[1]], .) %>% 
  mutate(ML = n, 
         ML_pct = ML/active_listings) %>% 
  dplyr::select(-n)

# housing_loss

# revenue_10pct - the distribution of revenue earned in terms of top 10% of hosts
neighbourhoods[[1]] <- daily[[1]] %>% 
  filter(date >= start_date, 
         date <= end_date, 
         status == "R") %>% 
  left_join(property[[1]] %>% 
              dplyr::select(property_ID, neighbourhood)) %>% 
  group_by(neighbourhood, host_ID) %>% 
  summarize(revenue = sum(price)) %>% 
  filter(revenue > 0) %>% 
  summarize(
    revenue_10pct = sum(revenue[revenue > quantile(revenue, c(0.90))] / sum(revenue))) %>% 
  left_join(neighbourhoods[[1]], .)


# principal_res?
