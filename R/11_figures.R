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

# Visualizing STR activity

cities_table %>% 
  st_cast("POINT") %>% 
  ggplot() + 
  geom_sf(aes(size = active_listings, colour = region))

cities_table %>% 
  st_cast("POINT") %>% 
  ggplot() + 
  geom_sf(aes(size = housing_loss, colour = region))

cities_table %>% 
  st_cast("POINT") %>% 
  ggplot() + 
  geom_sf(aes(size = revenue_LTM, colour = region))

cities_table %>% 
  group_by(region) %>% 
  ggplot() + 
  geom_violin(aes(x = region, y = active_listings))

cities_table %>% 
  group_by(region) %>% 
  ggplot() + 
  geom_violin(aes(x = region, y = housing_loss))

cities_table %>% 
  mutate(country = "United States") %>% 
  ggplot() + 
  geom_violin(aes(x = country, y = active_listings))

cities_table %>% 
  mutate(country = "United States") %>% 
  ggplot() + 
  geom_violin(aes(x = country, y = housing_loss))

cities_table %>% 
  mutate(country = "United States") %>% 
  ggplot() + 
  geom_violin(aes(x = country, y = revenue_LTM))


cities_table %>% 
  group_by(region) %>% 
  ggplot() + 
  geom_violin(aes(y = region, x = housing_loss))


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

cities_table %>% 
  st_cast("POINT") %>% 
  ggplot() + 
  geom_sf(aes(size = n, colour = region))

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
  facet_grid(vars(region))

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


############################################# BIVARIATE MAPPING ################################################################
# Visualizing two variables at the same time using a bivariate colour scale

# What cities to include? 3 for each variable
  # Top 15 cities with media attention
cities_media <- 
  media_table %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  dplyr::select(city) %>% 
  do.call(paste, .)

# But also those with the highest instances of the variable

# Housing_loss_pct_households 
  # New Orleans (most housing loss), Jersey City (high housing loss), San Fran (media)
cities_housing_loss <- 
  cities_table %>% 
    arrange(desc(housing_loss_pct_households)) %>% 
    dplyr::select(city) %>% 
    st_drop_geometry() %>% 
    do.call(paste, .)

data <- 
  neighbourhoods_table %>%  
  filter(city == "Chicago") %>% 
  filter(!is.na(CRI)) %>% 
  filter(housing_loss_pct_households >= 0) %>% 
  st_as_sf() 

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$housing_loss_pct_households, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Housing Loss and Community Resistance in San Francisco",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Housing Loss") %>% 
  plot()


# Active_listings
cities_active_listings <- 
  cities_table %>% 
  arrange(desc(active_listings)) %>% 
  dplyr::select(city) %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

data <- 
  neighbourhoods_table %>%  
  filter(city == "Miami") %>% 
  filter(!is.na(CRI)) %>% 
  filter(active_listings >= 0) %>% 
  st_as_sf() %>% 
  mutate(active_listings_inverse = 1/active_listings)

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$active_listings_inverse, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Active Listings and Community Resistance in Houston",
                  xlab = "Increasing CRI", 
                  ylab = "Decreasing STR Listings") %>% 
  plot()

# Non_mover_pct_pop
cities_non_mover <- 
  cities_table %>% 
  arrange(desc(non_mover_pct_pop)) %>% 
  dplyr::select(city) %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

data <- 
  neighbourhoods_table %>%  
  filter(city == "Las Vegas") %>% 
  filter(!is.na(CRI)) %>% 
  filter(non_mover_pct_pop >= 0) %>% 
  st_as_sf() 

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$non_mover_pct_pop, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Non Movers and Community Resistance in Austin",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Non-Movers") %>% 
  plot()


# Owner_occupied_pct_households
cities_OO <- 
  cities_table %>% 
  arrange((owner_occupied_pct_household)) %>% 
  dplyr::select(city) %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

data <- 
  neighbourhoods_table %>%  
  filter(city == "New York") %>% 
  filter(!is.na(CRI)) %>% 
  filter(owner_occupied_pct_household >= 0) %>% 
  st_as_sf() 

#quantiles_CRI <- c(-20, -2.5, 0.2, 2)

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$rental_pct_household, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Rental Households and Community Resistance in Seattle",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Rental Households") %>% 
  plot()

# Now, to show the absense of a relationship
# Active listings yoy 
cities_yoy <- 
  cities_table %>% 
  filter(active_listings>1000) %>% 
  arrange(desc(active_listings_yoy)) %>% 
  dplyr::select(city) %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

data <- 
  neighbourhoods_table %>%  
  filter(city == "Las Vegas") %>% 
  filter(!is.na(CRI)) %>% 
  filter(!is.na(active_listings_yoy)) %>% 
  st_as_sf() 

#quantiles_CRI <- c(-20, -2.5, 0.2, 2)

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$rental_pct_household, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "STR Growth Rates and Community Resistance in Las Vegas",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing STR Growth Rates") %>% 
  plot()

# Med income 
cities_income <- 
  cities_table %>% 
  arrange(desc(med_income)) %>% 
  dplyr::select(city) %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

data <- 
  neighbourhoods_table %>%  
  filter(city == "Detroit") %>% 
  filter(!is.na(CRI)) %>% 
  st_as_sf() 

#quantiles_CRI <- c(-20, -2.5, 0.2, 2)

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$med_income, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Median Income and Community Resistance in Detroit",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Median Income") %>% 
  plot()

################################# SIGNIFICANT AND INSIGNIFICANT VARIABLE PLOTTING #########################
# CRI versus significant variable

neighbourhoods_table %>% 
  ggplot(aes(x = owner_occupied_pct_household, y = CRI)) +
  geom_point() +
  geom_smooth(method = "lm")


