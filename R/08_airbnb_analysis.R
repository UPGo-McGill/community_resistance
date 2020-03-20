#################################### AIRBNB ANALYSIS ######################################

source("R/01_helper_functions.R")

# active_listings on end date
neighbourhoods <- future_map(seq_along(neighbourhoods), ~{
  property[[.x]] %>% 
    filter(housing == TRUE) %>% 
    filter(created <= end_date,
           scraped >= end_date) %>% 
    group_by(neighbourhood) %>% 
    tally() %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(active_listings = n) %>% 
    dplyr::select(-n)
})


# active_listings_avg over the past twelve months
neighbourhoods <- future_map(seq_along(neighbourhoods), ~{
  daily[[.x]] %>% 
    filter(date <= end_date &
             date >= start_date) %>% 
    left_join(property[[.x]] %>% 
                dplyr::select(c("property_ID", "neighbourhood"))) %>% 
    group_by(neighbourhood,date) %>% 
    summarize(listings = n()) %>% 
    summarize(active_listings_avg = mean(listings)) %>% 
    left_join(neighbourhoods[[.x]], .)
})


# active_listings_yoy growth rate from one year to the next
neighbourhoods <- future_map(seq_along(neighbourhoods), ~{
  property[[.x]] %>% 
    filter(housing == TRUE) %>% 
    filter(created <= ymd(end_date) - years(1),
           scraped >= ymd(end_date) - years(1)) %>% 
    group_by(neighbourhood) %>% 
    tally() %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(active_listings_prev = n, 
           active_listings_yoy = active_listings/active_listings_prev) %>% 
    dplyr::select(-c(n, active_listings_prev))
})


# EH and EH_pct - distribution of listing types on end date
neighbourhoods <- future_map(seq_along(neighbourhoods), ~{
  property[[.x]] %>% 
    filter(housing == TRUE, 
           listing_type == "Entire home/apt",
           created <= end_date,
           scraped >= end_date) %>% 
    group_by(neighbourhood) %>% 
    tally() %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(EH = n, 
           EH_pct = n/active_listings) %>% 
    dplyr::select(-n) 
  })


# revenue_LTM and the average revenue per listing over the past twelve months
neighbourhoods <- future_map(seq_along(neighbourhoods), ~{
  LTM_property[[.x]] %>% 
    group_by(neighbourhood) %>% 
    summarize(revenue_LTM = sum(revenue_LTM, na.rm = TRUE)) %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(revenue_LTM_per_listing = revenue_LTM/active_listings_avg)
})

  
# GH in terms of listing count (not housing units) on the end date
neighbourhoods <- future_map(seq_along(neighbourhoods), ~{ 
  GH[[.x]] %>% 
    filter(date == end_date) %>%
    dplyr::select(ghost_ID, listing_count, housing_units, property_IDs) %>% 
    st_centroid() %>% 
    strr_raffle(neighbourhoods[[.x]], neighbourhood, households) %>% 
    group_by(neighbourhood) %>% 
    summarize(GH = sum(listing_count)) %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(GH_pct = GH/active_listings)
}) 


# FREH status on the end date, but taken as a percentage in terms of active listings
  # over the past year since the calculation involves the past year as a time frame
neighbourhoods <- future_map(seq_along(neighbourhoods), ~{ 
  property[[.x]] %>% 
    dplyr::select(c("property_ID", "neighbourhood")) %>% 
    right_join(FREH[[.x]] %>% 
                 filter(date == end_date, FREH == TRUE) %>% 
                 dplyr::select(property_ID)) %>% 
    group_by(neighbourhood) %>% 
    count() %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(FREH = n, 
           FREH_pct = n/active_listings_avg) %>% 
    dplyr::select(-n)
})


# MLs on the end date
neighbourhoods <- map(seq_along(neighbourhoods), ~{ 
  daily[[.x]] %>% 
    filter(date == end_date) %>% 
    filter(multi == TRUE) %>% 
    left_join(property[[.x]] %>% 
                dplyr::select(c("property_ID", "neighbourhood"))) %>% 
    group_by(neighbourhood) %>% 
    count() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(ML = n, 
           ML_pct = ML/active_listings) %>% 
    dplyr::select(-n)
})


# housing_loss
neighbourhoods <- map(seq_along(neighbourhoods), ~{ 
  GH[[.x]] %>% 
    filter(date == end_date) %>%
    dplyr::select(ghost_ID, listing_count, housing_units, property_IDs) %>% 
    st_centroid() %>% 
    strr_raffle(neighbourhoods[[.x]], neighbourhood, households) %>% 
    group_by(neighbourhood) %>% 
    summarize(GH_housing = sum(housing_units)) %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(housing_loss = FREH + GH_housing,
           housing_loss_pct_households = housing_loss/households)
})


# revenue_10pct - the distribution of revenue earned in terms of top 10% of hosts
neighbourhoods <- map(seq_along(neighbourhoods), ~{ 
  daily[[.x]] %>% 
    filter(date >= start_date, 
           date <= end_date, 
           status == "R") %>% 
    left_join(property[[.x]] %>% 
                dplyr::select(property_ID, neighbourhood)) %>% 
    group_by(neighbourhood, host_ID) %>% 
    summarize(revenue = sum(price)) %>% 
    filter(revenue > 0) %>% 
    summarize(
      revenue_10pct = sum(revenue[revenue > quantile(revenue, c(0.90))] / sum(revenue))) %>% 
    left_join(neighbourhoods[[.x]], .)
})


# principal_res on the end date

neighbourhoods <- map(seq_along(neighbourhoods), ~{
  property[[.x]] %>% 
    filter(PR_10 == TRUE) %>% 
    group_by(neighbourhood) %>% 
    count() %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(PR_10 = n, 
           PR_10_ct = PR_10/active_listings) %>% 
    dplyr::select(-n)
})

neighbourhoods <- map(seq_along(neighbourhoods), ~{
  property[[.x]] %>% 
    filter(PR_25 == TRUE) %>% 
    group_by(neighbourhood) %>% 
    count() %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(PR_25 = n, 
           PR_25_ct = PR_25/active_listings) %>% 
    dplyr::select(-n)
})

neighbourhoods <- map(seq_along(neighbourhoods), ~{
  property[[.x]] %>% 
    filter(PR_50 == TRUE) %>% 
    group_by(neighbourhood) %>% 
    count() %>% 
    st_drop_geometry() %>% 
    left_join(neighbourhoods[[.x]], .) %>% 
    mutate(PR_50 = n, 
           PR_50_ct = PR_50/active_listings) %>% 
    dplyr::select(-n)
})

save(neighbourhoods, file = "neighbourhoods_temp.Rdata")  





