################################################# FIGURES ##############################################################################

source("R/01_helper_functions.R")


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
    geom = 'area', method = 'loess', span = 1/3,
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
    geom = 'area', method = 'loess', span = 1/3,
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
  geom_area() +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/3,
    alpha = 1/2, fill = "orange") +
  facet_wrap(vars(city)) 

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  count() %>%
  ggplot(aes(x = month_yr, y = n, fill = city)) +
  geom_area()

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













# Certain words over time?
  # eviction, gentrification, displacement

# CRI over time by region


# CRI grouped by city

neighbourhoods_table %>% 
  group_by(city) %>% 
  summarise(mean = mean(CRI, na.rm = TRUE)) %>% 
  st_drop_geometry() %>% 
  ggplot(aes(mean)) +
  geom_bar()

# CRI over time by entire country

###### SIGNIFICANT AND INSIGNIFICANT VARIABLE PLOTTING ##########
# CRI versus significant variable

###### CRI PER CITY #######
# Map of actual/predicted CRI for a few cities 


############################################# BIVARIATE MAPPING ################################################################
  # Visualizing two variables at the same time using a bivariate colour scale
data <- airbnb_neighbourhoods %>%  
  filter(city == "New Orleans") %>% 
  mutate(CRI = CRI/max(CRI)) %>% 
  st_as_sf() 

quantiles_CRI <- c(0, 0.02, 0.2, 1)

# Specify data, variables, title, labels, and quantiles (optional)
bivariate_mapping(data = data,
                  var1 = data$CRI, 
                  var2 = data$white_z, 
                  quantiles_var1 = quantiles_CRI, 
                  title = "Housing Loss and Community Resistance",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing White") %>% 
  plot()

