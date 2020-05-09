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

colour_scale <- 
  tibble(
  "3 - 3" = "#3F2949", # high var1, high var2
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low var1, high var2
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium var1, medium var2
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high var1, low var2
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0")

cities_table %>% 
  mutate(country = "") %>% 
  ggplot(aes(x = country, y = active_listings_avg)) + 
  geom_violin(fill = "#3F2949",
              colour = "#CABED0",
              lwd = 0.25,
              scale = "count", 
              adjust = 1,
              trim = TRUE,
              draw_quantiles = c(0.25, 0.75),
              linetype = "dashed") +
  geom_violin(fill = "transparent",
              colour = "#CABED0",
              lwd = 1.25,
              scale = "count", 
              adjust = 1,
              trim = TRUE,
              draw_quantiles = c(0.5)) +
  xlab("") +
  ylab("Number of listings\n") +
  ggtitle("STR market size by active listings in 2019 per city in the United States") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

cities_table %>% 
  mutate(country = "") %>% 
  ggplot(aes(x = country, y = housing_loss)) + 
  geom_violin(fill = "#3F2949",
              colour = "#CABED0",
              alpha = 1,
              lwd = 0.25,
              scale = "count", 
              adjust = 1,
              trim = TRUE,
              draw_quantiles = c(0.25, 0.75),
              linetype = "dashed") +
  geom_violin(fill = "transparent",
              colour = "#CABED0",
              lwd = 1.25,
              scale = "count", 
              adjust = 1,
              trim = TRUE,
              draw_quantiles = c(0.5)) +
  xlab("") +
  ylab("Housing lost to STRs\n") +
  ggtitle("STR-induced housing loss in 2019 per city in the United States") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

cities_table %>% 
  mutate(country = "") %>% 
  ggplot(aes(x = country, y = revenue_LTM)) + 
  geom_violin(fill = "#3F2949",
              colour = "#CABED0",
              lwd = 0.25,
              scale = "count", 
              adjust = 1,
              trim = TRUE,
              draw_quantiles = c(0.25, 0.75),
              linetype = "dashed") +
  geom_violin(fill = "transparent",
              colour = "#CABED0",
              lwd = 1.25,
              scale = "count", 
              adjust = 1,
              trim = TRUE,
              draw_quantiles = c(0.5)) +
  xlab("") +
  ylab("Revenue\n") +
  ggtitle("Revenues generated from STRs in 2019 by city in the United States") +
  scale_y_continuous(labels = paste0("$", c(0, 250, 500, 750), "M"),
                     breaks = 10^6 * c(0,250,500,750)) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

states_sf <- get_urbn_map("states", sf = TRUE)

counties_sf <- 
  get_urbn_map("counties", sf = TRUE)

cities_hawaii_alaska <- 
  cities_table %>% 
  filter(city == "Anchorage" |
           city == "Honolulu") %>% 
  mutate(county_name = 
           ifelse(city == "Anchorage",
                  "Anchorage Municipality",
                  "Honolulu County"))

cities_hawaii_alaska <- 
  cities_hawaii_alaska %>% 
  st_drop_geometry() %>% 
  left_join(counties_sf) %>%
  st_as_sf() 


cities_table %>%
  filter(city != "Anchorage" &
           city != "Honolulu") %>% 
  ggplot() +
  geom_sf(data = states_sf, 
          colour = alpha("#3F2949", 0.75),
          fill = "transparent") +
  geom_sf(data = cities_table %>%
                    filter(city != "Anchorage" &
                          city != "Honolulu") %>%
            st_cast("POINT"),
          aes(size = active_listings, colour = region),
          alpha = 0.75) +
   geom_sf(data = cities_hawaii_alaska %>% 
             st_centroid(),
           aes(size = active_listings, colour = region),
           alpha = 0.75) +
  coord_sf(datum = NA) +
  theme_minimal() +
  scale_color_manual(values = c("Midwest" = "#435786", 
                                "Northeast" = "#77324C",
                                "South" = "#89A1C8", 
                                "West" = "#BC7C8F")) +
  ggtitle("Active STR listings throughout the United States") +
  xlab("") +
  ylab("") +
  labs(colour = "Region",
       size = "Active listings") +
  scale_size_continuous(labels = comma,
                        range = c(3, 10)) +
  scale_alpha(guide = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.75, size = 3))) + 
  guides(size=guide_legend(override.aes=list(colour="#CABED0"))) +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.7), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

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
  geom_area(fill = "#CABED0", alpha = 0.5) +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/2,
    alpha = 0.5, fill = "#3F2949") +
  xlab("\nDate") +
  ylab("Number of news articles\n") +
  ggtitle("STR discourse throughout the United States over time") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

cities_table %>%
  filter(city != "Anchorage" &
           city != "Honolulu") %>% 
  ggplot() +
  geom_sf(data = states_sf, 
          colour = alpha("#3F2949", 0.75),
          fill = "transparent") +
  geom_sf(data = cities_table %>%
            filter(city != "Anchorage" &
                     city != "Honolulu") %>%
            st_cast("POINT"),
          aes(size = n, colour = region),
          alpha = 0.75) +
  geom_sf(data = cities_hawaii_alaska %>% 
            st_centroid(),
          aes(size = n, colour = region),
          alpha = 0.75) +
  coord_sf(datum = NA) +
  theme_minimal() +
  scale_color_manual(values = c("Midwest" = "#435786", 
                                "Northeast" = "#77324C",
                                "South" = "#89A1C8", 
                                "West" = "#BC7C8F")) +
  ggtitle("STR discourse throughout the United States") +
  xlab("") +
  ylab("") +
  labs(colour = "Region",
       size = "Number of news articles") +
  scale_size_continuous(labels = comma,
                        range = c(3, 10)) +
  scale_alpha(guide = "none") +
  guides(colour = guide_legend(override.aes = list(alpha = 0.75, size = 3))) + 
  guides(size= guide_legend(override.aes = list(colour = "#CABED0"))) +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.7), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) 

# Regional number of articles over time

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  group_by(region, month_yr) %>% 
  count() %>%
  ggplot(aes(x = month_yr, y = n)) +
  geom_area(fill = "#CABED0", alpha = 0.5) +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/5,
    alpha = 0.5, fill = "#3F2949") +
  facet_grid(vars(region)) +
  xlab("\nDate") +
  ylab("Number of news articles\n") +
  ggtitle("STR discourse by region throughout the United States over time") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


# Number of articles by city

cities_media <- 
  media_table %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(desc(n))

cities <- 
  cities_media[1:4,1] %>% 
  do.call(paste, .)

data <- 
  media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  count() 

data$city <- factor(data$city, levels = c("New York",
                                          "San Francisco",
                                          "Washington",
                                          "Los Angeles"))
  
data %>%
  ggplot(aes(x = month_yr, y = n)) +
  geom_area(fill = "#CABED0", alpha = 0.5) +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/5,
    alpha = 0.5, fill = "#3F2949") +
  facet_grid(vars(city)) +
  xlab("\nDate") +
  ylab("Number of news articles\n") +
  ggtitle("STR discourse by city throughout the United States over time\n") +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank())

cities <- 
  cities_media[5:13,1] %>% 
  do.call(paste, .)

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(n)

data <- 
  media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  count() 

data$city <- factor(data$city, levels = c("Chicago",
                                          "Boston",
                                          "Miami", 
                                          "Seattle",
                                          "Austin", 
                                          "Las Vegas",
                                          "Houston",
                                          "Philadelphia",
                                          "San Diego"))
data %>%
  ggplot(aes(x = month_yr, y = n)) +
  geom_area(fill = "#CABED0", alpha = 0.5) +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/5,
    alpha = 0.5, fill = "#3F2949") +
  facet_grid(vars(city)) +
  xlab("\nDate") +
  ylab("Number of news articles\n") +
  ggtitle("STR discourse by city throughout the United States over time\n") +
  scale_y_continuous(breaks = c(0, 50, 100), labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank())

##### SENTIMENT

# Country-wide sentiment of articles over time

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  ggplot(aes(Date, sentiment)) +
  #geom_point() +
  geom_smooth(span = 0.8, 
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 2) +
  geom_smooth(method = lm, 
              se = FALSE, 
              color = "#806A8A", 
              lwd = 1,
              linetype = "dotted") + 
  xlab("\nDate") +
  ylab("Sentiment\n") +
  ggtitle("The sentiment of STR discourse throughout the United States over time\n") +
  scale_y_continuous(breaks = 0, labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Regional sentiment of articles over time

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
ggplot(aes(Date, sentiment)) +
  #geom_point() +
  geom_smooth(span = 0.8, 
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 1) +
  facet_grid(vars(region)) +
  xlab("\nDate") +
  ylab("Sentiment\n") +
  ggtitle("The sentiment of STR discourse by region throughout the United States over time\n") +
  scale_y_continuous(breaks = 0, labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# City sentiment of articles over time

cities_media <- 
  media_table %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(desc(n))

cities <- 
  cities_media[1:4,1] %>% 
  do.call(paste, .)

data <- 
  media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities)

data$city <- factor(data$city, levels = c("New York",
                                          "San Francisco",
                                          "Washington",
                                          "Los Angeles"))

data %>% 
  ggplot(aes(Date, sentiment)) +
  geom_smooth(span = 0.8, 
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 1) +
  facet_grid(vars(city)) +
  xlab("\nDate") +
  ylab("Sentiment\n") +
  ggtitle("The sentiment of STR discourse by city throughout the United States over time\n") +
  scale_y_continuous(breaks = 0, labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

cities <- 
  cities_media[5:13,1] %>% 
  do.call(paste, .)

data <- 
  media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities)

data$city <- factor(data$city, levels = c("Chicago",
                                          "Boston",
                                          "Miami", 
                                          "Seattle",
                                          "Austin", 
                                          "Las Vegas",
                                          "Houston",
                                          "Philadelphia",
                                          "San Diego"))

data %>% 
  ggplot(aes(Date, sentiment)) +
  geom_smooth(span = 0.8, 
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 1) +
  facet_grid(vars(city)) +
  xlab("\nDate") +
  ylab("Sentiment\n") +
  ggtitle("The sentiment of STR discourse by city throughout the United States over time\n") +
  scale_y_continuous(breaks = 0, labels = comma) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


##### COMMUNITY RESISTANCE INDEX

# Country CRI over time

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  group_by(month_yr) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE),
            articles = n()) %>% 
  mutate(CRI = -1 * sentiment * articles) %>% 
  ggplot(aes(month_yr, CRI)) +
 # geom_point() +
  geom_smooth(span = 0.8, 
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 2) +
  geom_smooth(method = lm, 
              se = FALSE, 
              color = "#806A8A", 
              lwd = 1,
              linetype = "dotted") + 
  xlab("\nDate") +
  ylab("CSI\n") +
  ggtitle("The community sentiment index throughout the United States over time\n") +
  scale_y_continuous(breaks = pretty_breaks(), labels = number_format(accuracy = 1)) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Region CRI over time

media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  group_by(region, month_yr) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE),
            articles = n()) %>% 
  mutate(CRI = -1 * sentiment * articles) %>% 
  ggplot(aes(month_yr, CRI)) +
  geom_smooth(span = 0.8, 
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 1) +
  facet_grid(vars(region)) +
  xlab("\nDate") +
  ylab("CSI\n") +
  ggtitle("The community sentiment index by region throughout the United States over time\n") +
  scale_y_continuous(breaks = c(-4, -2, 0, 2, 4), labels = number_format(accuracy = 1)) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank())

# City CRI over time

cities_media <- 
  media_table %>% 
  group_by(city) %>% 
  count() %>% 
  arrange(desc(n))

cities <- 
  cities_media[1:4,1] %>% 
  do.call(paste, .)

data <- 
  media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE),
            articles = n()) %>% 
  mutate(CRI = -1 * sentiment * articles) 
  
data$city <- factor(data$city, levels = c("New York",
                                          "San Francisco",
                                          "Washington",
                                          "Los Angeles"))
data %>% 
  ggplot(aes(month_yr, CRI)) +
  geom_smooth(span = 0.8, 
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 1) +
  facet_grid(vars(city)) +
  xlab("\nDate") +
  ylab("CSI\n") +
  ggtitle("The community sentiment index by city throughout the United States over time\n") +
  scale_y_continuous(breaks = c(-2, 0, 2), 
                     labels = number_format(accuracy = 1)) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank())

cities <- 
  cities_media[5:13,1] %>% 
  do.call(paste, .)

data <- 
  media_table %>% 
  filter(Date >= "2015-01-01" &
           Date <= "2019-12-31") %>% 
  filter(city %in% cities) %>% 
  group_by(city, month_yr) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE),
            articles = n()) %>% 
  mutate(CRI = -1 * sentiment * articles) 

data$city <- factor(data$city, levels = c("Chicago",
                                          "Boston",
                                          "Miami", 
                                          "Seattle",
                                          "Austin", 
                                          "Las Vegas",
                                          "Houston",
                                          "Philadelphia",
                                          "San Diego"))

data %>% 
  ggplot(aes(month_yr, CRI)) +
  geom_smooth(span = 0.8, 
              color = "#3F2949", 
              fill = "#CABED0", 
              lwd = 1) +
  facet_grid(vars(city)) +
  xlab("\nDate") +
  ylab("CSI\n") +
  ggtitle("The community sentiment index by city throughout the United States over time\n") +
  scale_y_continuous(breaks = c(-0.25, 0, 0.25), 
                     labels = number_format(accuracy = 0.01)) +
  theme_minimal() +
  theme(text = element_text(family = "Helvetica Light", size = 14),
        plot.title = element_text(hjust = 0.5), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank())

## CRI

cities_table %>% 
  dplyr::select(c(city, CRI)) %>% 
  st_drop_geometry() %>% 
  arrange(desc(CRI)) %>% view()

neighbourhoods_table %>% 
  dplyr::select(c(city, neighbourhood, CRI)) %>% 
  st_drop_geometry() %>% 
  arrange((CRI)) %>% view()

neighbourhoods_table %>% 
  dplyr::select(c(city, neighbourhood, CRI)) %>% 
  st_drop_geometry() %>% 
  arrange(desc(CRI)) %>% view()


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

# HOUSING LOSS
  # New Orleans (most housing loss), Jersey City (high housing loss), San Fran (media)
  cities_table %>% 
    arrange(desc(housing_loss_pct_households)) %>% 
    dplyr::select(city) %>% 
    st_drop_geometry() %>% 
    do.call(paste, .)

# Specify data, variables, title, labels, and quantiles (optional)
  # New Orleans
data <- 
  neighbourhoods_table %>%  
  filter(city == "New Orleans") %>% 
  filter(!is.na(CRI)) %>% 
  filter(housing_loss_pct_households >= 0) %>% 
  st_as_sf() 

water_LA <- 
  st_read("data/tiger_la_water_CENSUS_2006/tiger_la_water_CENSUS_2006.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009)

streets_neworleans <-
  (getbb("New Orleans") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

bivariate_mapping(data = data,
                  streets = streets_neworleans,
                  water = water_LA,
                  buffer = 5000,
                  var1 = data$CRI, 
                  var2 = data$housing_loss_pct_households, 
                  title = "Housing Loss and Community Sentiment in New Orleans",
                  xlab = "Increasing CSI", 
                  ylab = "Increasing Housing Loss") %>% 
  plot()

  # Jersey City
data <- 
  neighbourhoods_table %>%  
  filter(city == "Jersey City") %>% 
  filter(!is.na(CRI)) %>% 
  filter(housing_loss_pct_households >= 0) %>% 
  st_as_sf() 

water_NJ <- 
  st_read("data/nhdwaterbody2002shp/nhdwaterbody2002.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_set_precision(1000000) %>% 
  st_make_valid() 

streets_jersey <-
  (getbb("Jersey City") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

bivariate_mapping(data = data,
                  streets = streets_jersey,
                  water = water_NJ,
                  buffer = 2000,
                  var1 = data$CRI, 
                  var2 = data$housing_loss_pct_households, 
                  title = "Housing Loss and Community Sentiment in Jersey City",
                  xlab = "Increasing CSI", 
                  ylab = "Increasing Housing Loss") %>% 
  plot()


  # San Francisco
data_hold <- 
  neighbourhoods_table %>% 
  filter(city == "San Francisco") %>% 
  filter(!is.na(CRI)) %>% 
  filter(housing_loss_pct_households >= 0)
  
data_hold <- 
  data_hold[-c(4,5),]
  
data <- 
  neighbourhoods_table %>%  
  filter(city == "San Francisco") %>% 
  filter(!is.na(CRI)) %>% 
  filter(housing_loss_pct_households >= 0) %>% 
  st_as_sf() %>% 
  st_intersection(st_buffer(st_as_sfc(st_bbox(data_hold)), 7000))

rm(data_hold)

water_CA <- 
  st_read("data/Shape_CA/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_CA_ocean <- 
  st_read("data/Shape_CA/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm()

water_CA <- 
  rbind(water_CA_ocean %>% dplyr::select("geometry"),
        water_CA %>% dplyr::select("geometry"))

streets_sanfran <-
  (getbb("San Francisco") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

bivariate_mapping(data = data,
                  streets = streets_sanfran,
                  water = water_CA,
                  buffer = 5000,
                  var1 = data$CRI, 
                  var2 = data$housing_loss_pct_households, 
                  title = "Housing Loss and Community Sentiment in San Francisco",
                  xlab = "Increasing CSI", 
                  ylab = "Increasing Housing Loss") %>% 
  plot()


### ACTIVE LISTINGS
cities_active_listings <- 
  cities_table %>% 
  arrange(desc(active_listings)) %>% 
  dplyr::select(city) %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

  # New York
data <- 
  neighbourhoods_table %>%  
  filter(city == "New York") %>% 
  filter(!is.na(CRI)) %>% 
  filter(active_listings >= 0) %>% 
  st_as_sf() %>% 
  mutate(active_listings_inverse = 1/active_listings)

streets_newyork <-
  (getbb("New York City") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_NY <- 
  st_read("data/Shape_NY/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_NY_ocean <- 
  st_read("data/Shape_NY/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm()

water_NY <- 
  rbind(water_NY_ocean %>% dplyr::select("geometry"),
        water_NY %>% dplyr::select("geometry"))


bivariate_mapping(data = data,
                  streets = streets_newyork,
                  water = water_NY,
                  buffer = 5000,
                  var1 = data$CRI, 
                  var2 = data$active_listings_inverse, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Active Listings and Community Resistance in New York City",
                  xlab = "Increasing CRI", 
                  ylab = "Decreasing STR Listings") %>% 
  plot()


# Houston
data <- 
  neighbourhoods_table %>%  
  filter(city == "Houston") %>% 
  filter(!is.na(CRI)) %>% 
  filter(active_listings >= 0) %>% 
  st_as_sf() %>% 
  mutate(active_listings_inverse = 1/active_listings)

streets_houston <-
  (getbb("Houston") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_TX <- 
  st_read("data/Shape_TX/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_TX_ocean <- 
  st_read("data/Shape_TX/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_make_valid()

water_TX <- 
  rbind(water_TX_ocean %>% dplyr::select("geometry"),
        water_TX %>% dplyr::select("geometry"))


bivariate_mapping(data = data,
                  streets = streets_houston,
                  water = water_TX,
                  buffer = 10000,
                  var1 = data$CRI, 
                  var2 = data$active_listings_inverse, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Active Listings and Community Resistance in Houston",
                  xlab = "Increasing CRI", 
                  ylab = "Decreasing STR Listings") %>% 
  plot()


  # Los Angeles
data <- 
  neighbourhoods_table %>%  
  filter(city == "Los Angeles") %>% 
  filter(!is.na(CRI)) %>% 
  filter(active_listings >= 0) %>% 
  st_as_sf() %>% 
  mutate(active_listings_inverse = 1/active_listings)

streets_losangeles <-
  (getbb("Los Angeles") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(memsize = 4e9) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_CA <- 
  st_read("data/Shape_CA/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_CA_ocean <- 
  st_read("data/Shape_CA/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm()

water_CA <- 
  rbind(water_CA_ocean %>% dplyr::select("geometry"),
        water_CA %>% dplyr::select("geometry"))

bivariate_mapping(data = data,
                  streets = streets_losangeles,
                  water = water_CA,
                  buffer = 5000,
                  var1 = data$CRI, 
                  var2 = data$active_listings_inverse, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Active Listings and Community Resistance in Los Angeles",
                  xlab = "Increasing CRI", 
                  ylab = "Decreasing STR Listings") %>% 
  plot()



### NON MOVER
cities_non_mover <- 
  cities_table %>% 
  arrange(desc(non_mover_pct_pop)) %>% 
  dplyr::select(city) %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

  # Chicago
data <- 
  neighbourhoods_table %>%  
  filter(city == "Chicago") %>% 
  filter(!is.na(CRI)) %>% 
  filter(non_mover_pct_pop >= 0) %>% 
  st_as_sf() 

streets_chicago <-
  (getbb("Chicago") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_IL <- 
  st_read("data/Shape_IL/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_IL_lake <- 
  st_read("data/hydro_p_LakeMichigan/hydro_p_LakeMichigan.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm()

water_IL <- 
  rbind(water_IL_lake %>% dplyr::select("geometry"),
        water_IL %>% dplyr::select("geometry"))

bivariate_mapping(data = data,
                  streets = streets_chicago,
                  water = water_IL,
                  buffer = 8000,
                  var1 = data$CRI, 
                  var2 = data$non_mover_pct_pop, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Non Movers and Community Resistance in Chicago",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Non-Movers") %>% 
  plot()



# Washington
data <- 
  neighbourhoods_table %>%  
  filter(city == "Washington") %>% 
  filter(!is.na(CRI)) %>% 
  filter(non_mover_pct_pop >= 0) %>% 
  st_as_sf() 

streets_washington <-
  (getbb("Washington DC") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_DC <- 
  st_read("data/Shape_DC/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_DC_ocean <- 
  st_read("data/Shape_DC/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm()

water_DC <- 
  rbind(water_DC_ocean %>% dplyr::select("geometry"),
        water_DC %>% dplyr::select("geometry"))

bivariate_mapping(data = data,
                  streets = streets_washington,
                  water = water_DC,
                  buffer = 3000,
                  var1 = data$CRI, 
                  var2 = data$non_mover_pct_pop, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Non Movers and Community Resistance in Washington",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Non-Movers") %>% 
  plot()

  # Austin
data <- 
  neighbourhoods_table %>%  
  filter(city == "Austin") %>% 
  filter(!is.na(CRI)) %>% 
  filter(non_mover_pct_pop >= 0) %>% 
  st_as_sf() 

streets_austin <-
  (getbb("Austin") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_TX <- 
  st_read("data/Shape_TX/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_TX_ocean <- 
  st_read("data/Shape_TX/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_make_valid()

water_TX_supplement <- 
  st_read("data/Surface_Water-shp/474c9f1b-1005-42f0-a925-3be3426b11c5202046-1-aqovxe.5877.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_make_valid()

water_TX <- 
  rbind(water_TX_supplement %>% dplyr::select("geometry"),
        water_TX_ocean %>% dplyr::select("geometry"))

bivariate_mapping(data = data,
                  streets = streets_austin,
                  water = water_TX,
                  buffer = 5000,
                  var1 = data$CRI, 
                  var2 = data$non_mover_pct_pop, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Non Movers and Community Resistance in Austin",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Non-Movers") %>% 
  plot()


# OWNER_OCCUPIED
cities_OO <- 
  cities_table %>% 
  arrange((owner_occupied_pct_household)) %>% 
  dplyr::select(city) %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

  # San Diego
data <- 
  neighbourhoods_table %>%  
  filter(city == "San Diego") %>% 
  filter(!is.na(CRI)) %>% 
  filter(owner_occupied_pct_household >= 0) %>% 
  st_as_sf() 

streets_sandiego <-
  (getbb("San Diego") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_CA <- 
  st_read("data/Shape_CA/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_CA_ocean <- 
  st_read("data/Shape_CA/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_make_valid()

water_CA_ocean2 <- 
  st_read("data/ne_10m_ocean/ne_10m_ocean.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_make_valid()

water_CA <- 
  rbind(water_CA_ocean %>% dplyr::select("geometry"),
        water_CA_ocean2 %>% dplyr::select("geometry"),
        water_CA %>% dplyr::select("geometry"))

bivariate_mapping(data = data,
                  streets = streets_sandiego,
                  water = water_CA,
                  buffer = 5000,
                  var1 = data$CRI, 
                  var2 = data$rental_pct_household, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Rental Households and Community Resistance in San Diego",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Rental Households") %>% 
  plot()

# Buffalo
data <- 
  neighbourhoods_table %>%  
  filter(city == "Buffalo") %>% 
  filter(!is.na(CRI)) %>% 
  filter(owner_occupied_pct_household >= 0) %>% 
  st_as_sf() 

streets_buffalo <-
  (getbb("Buffalo") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_NY <- 
  st_read("data/Shape_NY/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_NY_ocean <- 
  st_read("data/Shape_NY/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm()

water_NY <- 
  rbind(water_NY_ocean %>% dplyr::select("geometry"),
        water_NY %>% dplyr::select("geometry"))

bivariate_mapping(data = data,
                  streets = streets_buffalo,
                  water = water_NY,
                  buffer = 5000,
                  var1 = data$CRI, 
                  var2 = data$rental_pct_household, 
                  #quantiles_var1 = quantiles_CRI,
                  #quantiles_var2 = c(0, 0.00001, 0.001, 1), 
                  title = "Rental Households and Community Resistance in Buffalo",
                  xlab = "Increasing CRI", 
                  ylab = "Increasing Rental Households") %>% 
  plot()

# Seattle
data <- 
  neighbourhoods_table %>%  
  filter(city == "Seattle") %>% 
  filter(!is.na(CRI)) %>% 
  filter(owner_occupied_pct_household >= 0) %>% 
  st_as_sf() 

streets_seattle <-
  (getbb("Seattle") * c(1.01, 0.99, 0.99, 1.01)) %>%
  opq(timeout = 200) %>%
  add_osm_feature(key = "highway", value = c("primary", "secondary",
                                             "tertiary", "motorway")) %>%
  osmdata_sf()

water_WA <- 
  st_read("data/Shape_WA/NHDWaterbody.shx") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm() %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_make_valid() 

water_WA_ocean <- 
  st_read("data/Shape_WA/NHDArea.shp") %>% 
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_zm()

water_WA <- 
  rbind(water_WA_ocean %>% dplyr::select("geometry"),
        water_WA %>% dplyr::select("geometry"))

bivariate_mapping(data = data,
                  streets = streets_seattle,
                  water = water_WA,
                  buffer = 5000,
                  var1 = data$CRI, 
                  var2 = data$rental_pct_household, 
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



