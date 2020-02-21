#################################### TEMPORAL ANALYSIS ######################################

source("R/01_helper_functions.R")

# Get sentiment for each article (done previously in script 06_media_analysis.R)
  # and group by year to calculate average sentiment by city per year

media <- 
  map(media, ~{
  .x %>% 
  mutate(year = year(.x$Date))
})

temporal <- 
  map(media, ~{
    .x %>% 
      group_by(year) %>% 
      mutate(sentiment_year = mean(sentiment)) %>% 
      dplyr::select(year, sentiment_year) %>% 
      distinct()
  })

temporal <- 
  map(seq_along(media), ~{
    media[[.x]] %>% 
      group_by(year) %>% 
      count() %>% 
      mutate(articles = n) %>% 
      dplyr::select(-n) %>% 
    left_join(temporal[[.x]])
  })

# Only look at properties within the cities
property <- 
  map(property, ~{
  .x %>% 
    filter(!is.na(neighbourhood))
})

daily <- 
  map(seq_along(daily), ~{
    daily[[.x]] %>% 
  filter(property_ID %in% property[[.x]]$property_ID)
  })

# Set up dates to retrieve Airbnb data from the past five years
temporal <- 
  map(temporal, ~{  
    .x %>% 
      mutate(ref_date = as.Date(paste(year, "06-30", sep = "-")),
             year_end = as.Date(paste(year, "12-31", sep = "-")),
             year_start = as.Date(paste(year, "01-01", sep = "-")))
    })


# active listings on reference date
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(active_listings = 
               property %>% 
               filter(housing == TRUE) %>% 
               filter(created <= ref_date,
                      scraped >= ref_date) %>% 
               nrow()) 
  })

# EH and EH_pct
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(
        EH = 
          property %>% 
          filter(housing == TRUE, 
                 listing_type == "Entire home/apt") %>% 
          filter(created <= ref_date,
                 scraped >= ref_date) %>% 
          nrow(), 
        EH_pct = 
          EH/active_listings)
  })
  

# ML and ML_pct
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(
        ML = 
          daily %>% 
          filter(date == ref_date) %>% 
          filter(multi == TRUE) %>% 
          left_join(property) %>% 
          nrow(), 
        ML_pct = 
          ML/active_listings)
  })


# LTM revenue
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(
        revenue = 
          daily %>% 
          filter(date >= year_start,
                 date <= year_end,
                 status == "R") %>% 
          group_by(property_ID) %>% 
          summarize(revenue = sum(price)) %>% 
          summarize(LTM_rev = sum(revenue)) %>% 
          as.String() %>% 
          as.numeric(), 
        revenue_per_listing = 
          revenue/active_listings)
  })


# revenue_10pct - the distribution of revenue earned in terms of top 10% of hosts
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(
        revenue_10pct = 
          daily %>% 
          filter(date >= year_start, 
                 date <= year_end, 
                 status == "R") %>% 
          left_join(property) %>% 
          group_by(host_ID) %>% 
          summarize(revenue = sum(price)) %>% 
          filter(revenue > 0) %>% 
          summarize(
            revenue_10pct = sum(revenue[revenue > quantile(revenue, c(0.90))] / sum(revenue))) %>% 
          as.String())
  })



# GH in terms of listing count (not housing units) on the reference date
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(GH_listing =
               ifelse(nrow(GH %>% 
                             filter(date == ref_date)) == 0, 
                      0,
                      GH %>% 
                        filter(date == ref_date) %>% 
                        summarize(GH = sum(listing_count)) %>% 
                        st_drop_geometry() %>% 
                        as.String()) %>% 
               as.numeric(), 
             GH_pct = GH_listing/active_listings)
  })

            

# FREH status on the end date since it looks at activity over the past twelve months
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(FREH_count = 
               FREH %>% 
               filter(date == year_end) %>% 
               nrow(), 
             FREH_pct = FREH_count/active_listings)  
  })


# housing_loss
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(housing_loss =
               ifelse(nrow(GH %>% 
                             filter(date == ref_date)) == 0, 
                      0,
                      GH %>% 
                        filter(date == ref_date) %>% 
                        summarize(GH = sum(housing_units)) %>% 
                        st_drop_geometry() %>% 
                        as.String()) %>% 
               as.numeric() +
               FREH_count)
  })


# principal_res
temporal <- 
  map(temporal, ~{
    .x %>% 
      mutate(PR = 
               strr_principal_residence(property, daily, FREH, GH, ref_date, ref_date, 
                                        sensitivity = 0.5) %>% 
               filter(created <= ref_date,
                      scraped >= ref_date) %>% 
               filter(principal_residence == TRUE) %>% 
               nrow(),
             PR_pct = PR/active_listings)
  })

