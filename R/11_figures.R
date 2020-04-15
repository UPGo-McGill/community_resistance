################################################# FIGURES ##############################################################################

source("R/01_helper_functions.R")

load("data/modeling_data.Rdata")

################################### AIRBNB ANALYSIS ########################################

cities_table %>% 
  dplyr::select(c("city", "active_listings_avg")) %>% 
  arrange(desc(active_listings_avg))

cities_table %>% 
  dplyr::select(c("region", "city", "active_listings_avg")) %>% 
  group_by(region) %>% 
  summarize(active_listings_avg_region = sum(active_listings_avg, na.rm = TRUE)) %>% 
  arrange(desc(active_listings_avg_region))

cities_table %>% 
  mutate(active_listings_per_household = active_listings_avg/households) %>% 
  dplyr::select(c("city", "active_listings_per_household")) %>% 
  arrange(desc(active_listings_per_household))

cities_table %>% 
  dplyr::select(c("region", "city", "active_listings_avg", "households")) %>% 
  group_by(region) %>% 
  summarize(active_listings_avg_region = sum(active_listings_avg, na.rm = TRUE), 
            households_region = sum(households, na.rm = TRUE)) %>% 
  mutate(active_listings_per_household = active_listings_avg_region/households_region) %>% 
  dplyr::select(c("region", "active_listings_per_household")) %>% 
  arrange(desc(active_listings_per_household))

cities_table %>% 
  dplyr::select(c("city", "revenue_LTM")) %>% 
  arrange(desc(revenue_LTM))

cities_table %>% 
  st_drop_geometry() %>% 
  dplyr::select(c("region", "city", "revenue_LTM")) %>% 
  group_by(region) %>% 
  summarize(revenue_LTM_region = sum(as.numeric(revenue_LTM, na.rm = TRUE))) %>% 
  arrange(desc(revenue_LTM_region))

cities_table %>% 
  dplyr::select(c("city", "revenue_LTM_per_listing")) %>% 
  arrange(desc(revenue_LTM_per_listing))

cities_table %>% 
  st_drop_geometry() %>% 
  dplyr::select(c("region", "city", "revenue_LTM", "active_listings_avg")) %>% 
  group_by(region) %>% 
  summarize(revenue_LTM_per_listing_region = sum(as.numeric(revenue_LTM, na.rm = TRUE))/sum(active_listings_avg, na.rm = TRUE)) %>% 
  arrange(desc(revenue_LTM_per_listing_region))

cities_table %>% 
  #filter(active_listings_avg > 5000) %>% 
  dplyr::select(c("city", "active_listings_yoy")) %>% 
  arrange(desc(active_listings_yoy))

cities_table %>% 
  st_drop_geometry() %>% 
  dplyr::select(c("region", "city", "active_listings_prev", "active_listings")) %>% 
  group_by(region) %>% 
  summarize(active_listings_yoy_region = sum(as.numeric(active_listings, na.rm = TRUE))/sum(active_listings_prev, na.rm = TRUE)) %>% 
  arrange(desc(active_listings_yoy_region))

cities_table %>% 
  dplyr::select(c("city", "housing_loss")) %>% 
  arrange(desc(housing_loss))

cities_table %>% 
  st_drop_geometry() %>% 
  dplyr::select(c("region", "city", "housing_loss")) %>% 
  group_by(region) %>% 
  summarize(housing_loss_region = sum(housing_loss, na.rm = TRUE))%>% 
  arrange(desc(housing_loss_region))

cities_table %>% 
  dplyr::select(c("city", "housing_loss_pct_households")) %>% 
  arrange(desc(housing_loss_pct_households))

cities_table %>% 
  st_drop_geometry() %>% 
  dplyr::select(c("region", "city", "housing_loss", "households")) %>% 
  group_by(region) %>% 
  summarize(housing_loss_pct_households_region = sum(housing_loss, na.rm = TRUE)/sum(households, na.rm = TRUE))%>% 
  arrange(desc(housing_loss_pct_households_region))



################################### MEDIA ANALYSIS ###########################################

##### NUMBER OF ARTICLES

media_table$Date <- as.Date(media_table$Date)

media_table$month_yr <- (format(as.Date(media_table$Date), "%Y-%m"))

media_table$month_yr <- as.Date(paste(media_table$month_yr, "-15", sep = ""))

# Country-wide number of articles over time

media_table %>% 
  filter(Date >= "2015-01-01" &
        Date <= "2019-12-31") %>% 
  group_by(month_yr) %>% 
  count() %>%
  ggplot(aes(x = month_yr, y = n)) +
  geom_area() +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/5,
    alpha = 1/2, fill = "orange")

# Regional number of articles over time

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  group_by(region, month_yr) %>% 
  count() %>%
  ggplot(aes(x = month_yr, y = n)) +
  geom_area() +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/5,
    alpha = 1/2, fill = "orange") +
  facet_wrap(vars(region))

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  group_by(region, month_yr) %>% 
  count() %>%
  ggplot(aes(x = month_yr, y = n, fill = region)) +
  geom_area()

# Number of articles by city

cities_media <- 
  media_table %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(desc(n))

cities <- 
  cities_media[1:4,1] %>% 
  do.call(paste, .)

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  count() %>%
  ggplot(aes(x = month_yr, y = n)) +
  #geom_area() +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/5,
    alpha = 1/2, fill = "orange") +
  facet_wrap(vars(city)) 

cities <- 
  cities_media[5:13,1] %>% 
  do.call(paste, .)

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  count() %>%
  ggplot(aes(x = month_yr, y = n)) +
  geom_area(aes(alpha = 0.2)) +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/5,
    alpha = 1/2, fill = "orange") +
  facet_wrap(vars(city)) 

##### SENTIMENT

# Country-wide sentiment of articles over time

media_table %>% 
  filter(Date >= "2015-01-01") %>% 
  ggplot(aes(Date, sentiment)) +
  #geom_point() +
  geom_smooth(span = 0.8) +
  geom_smooth(method = lm, se = FALSE, 
              color = "black")

# Regional sentiment of articles over time

media_table %>% 
  filter(Date >= "2015-01-01") %>% 
ggplot(aes(Date, sentiment)) +
  #geom_point() +
  geom_smooth(span = 0.8) +
  geom_smooth(method = lm, se = FALSE, 
              color = "black") +
  facet_wrap(vars(region))

# City sentiment of articles over time

cities_PUMAs <- 
  neighbourhoods_table%>% 
  group_by(city) %>% 
  st_drop_geometry() %>% 
  count()%>% 
  filter(n >=3) %>% 
  dplyr::select(city) %>% 
  do.call(paste, .)

cities <- 
  c("Salt Lake City", "Houston", "Corpus Christi",
    "Mesa", "Oakland", "New Orleans", "San Francisco", 
    "Fresno", "Detroit", "Honolulu", "Miami", 
    "Philadelphia", "Washington", "Wichita", "Seattle", "Baltimore")

cities_media <- 
  media_table %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(desc(n))

cities <- 
  cities_media[1:16,1] %>% 
  do.call(paste, .)

media_table %>% 
  filter(Date >= "2015-01-01") %>% 
  filter(city %in% cities) %>% 
  ggplot(aes(Date, sentiment)) +
  geom_smooth() +
#  geom_smooth(method = lm, se = FALSE, 
            #  color = "black") +
  facet_wrap(vars(city))


##### COMMUNITY RESISTANCE INDEX

# Country CRI over time

media_table %>% 
  filter(Date >= "2015-01-01") %>% 
  group_by(month_yr) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE),
            articles = n()) %>% 
  mutate(CRI = -1 * sentiment * articles) %>% 
  ggplot(aes(month_yr, CRI)) +
  geom_smooth()

# Region CRI over time

media_table %>% 
  filter(Date >= "2015-01-01") %>% 
  group_by(region, month_yr) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE),
            articles = n()) %>% 
  mutate(CRI = -1 * sentiment * articles) %>% 
  ggplot(aes(month_yr, CRI)) +
  geom_smooth() +
  facet_wrap(vars(region))

# City CRI over time

cities_media <- 
  media_table %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(desc(n))

cities <- 
  cities_media[1:4,1] %>% 
  do.call(paste, .)

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE),
            articles = n()) %>% 
  mutate(CRI = -1 * sentiment * articles) %>% 
  ggplot(aes(month_yr, CRI)) +
  geom_smooth() +
  facet_wrap(vars(city))

cities <- 
  cities_media[5:20,1] %>% 
  do.call(paste, .)

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE),
            articles = n()) %>% 
  mutate(CRI = -1 * sentiment * articles) %>% 
  ggplot(aes(month_yr, CRI)) +
  geom_smooth() +
  facet_wrap(vars(city))


################################# SIGNIFICANT AND INSIGNIFICANT VARIABLE PLOTTING #########################
# CRI versus significant variable

###### CRI PER CITY #######
# Map of actual/predicted CRI for a few cities 


############################################# BIVARIATE MAPPING ################################################################
  # Visualizing two variables at the same time using a bivariate colour scale
data <- 
  neighbourhoods_table %>%  
  filter(city == "Miami") %>% 
  filter(!is.na(CRI)) %>% 
  filter(!is.na(housing_loss_pct_households)) %>% 
  st_as_sf() 

# uantiles_CRI <- c(0, 0.02, 0.2, 1)

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$housing_loss_pct_households, 
                 # quantiles_var1 = quantiles_CRI, 
                  title = "Housing Loss and Community Resistance",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing White") %>% 
  plot()

