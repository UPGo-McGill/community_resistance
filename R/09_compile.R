######################################### COMPILE ###############################

source("R/01_helper_functions.R")

load("data/media.Rdata")
load("data/neighbourhoods.Rdata")
load("data/cityname.Rdata")
load("data/neighbourhoods_updated.Rdata")

# Distinguish between Glendale AZ and Glendale CA
cityname[[32]] = "Glendale AZ"
cityname[[33]] = "Glendale CA"

# Cast geometries as polygons and add city name as a variable

neighbourhoods <-
  map2(neighbourhoods, cityname, ~{
    .x %>% 
      mutate(
        #geometry = st_union(st_collection_extract(geometry, "MULTIPOLYGON")),
             city = .y)
  })

# Create a dataframe from the list

neighbourhoods_table <- 
  do.call(rbind, neighbourhoods)

# Pare down city geometries in accordance with city_info_full_changes

neighbourhoods_updated_table <- 
  do.call(rbind, neighbourhoods_updated)

neighbourhoods_table <- 
  neighbourhoods_table %>% 
  filter(neighbourhood %in% neighbourhoods_updated_table$neighbourhood)

# Add housing loss as a percent of STR listings

neighbourhoods_table <- 
  neighbourhoods_table %>% 
  mutate(housing_loss_pct_listings = housing_loss/active_listings)

# Create a region variable

northeast <- c("New York", "Philadelphia", "Boston", "Pittsburgh",
               "Newark", "Jersey City", "Buffalo", "Rochester")

midwest <- c("Chicago", "Columbus", "Indianapolis", "Detroit", 
             "Milwaukee", "Kansas City", "Omaha", "Minneapolis",
             "Wichita", "Cleveland", "Saint Paul", "Saint Louis", 
             "Cincinnati", "Lincoln", "Toledo", "Fort Wayne", "Madison",
             "Des Moines", "Grand Rapids")

south <- c("Houston", "San Antonio", "Dallas", "Austin", 
           "Jacksonville", "Fort Worth", "Charlotte", "Washington",
           "El Paso", "Nashville", "Memphis", "Oklahoma City", "Louisville",
           "Baltimore", "Atlanta", "Miami", "Raleigh", "Virginia Beach",
           "Tulsa", "Arlington", "Tampa", "New Orleans", "Corpus Christi",
           "Lexington", "Greensboro", "Plano", "Orlando", "Durham", 
           "Saint Petersburg", "Laredo", "Lubbock", "Winston Salem",
           "Norfolk", "Chesapeake", "Garland", "Irving", "Hialeah", "Richmond",
           "Baton Rouge", "Birmingham", "Fayetteville")

west <- c("Albuquerque", "Anaheim", "Anchorage", "Los Angeles", "Phoenix",
          "San Diego", "San Jose", "San Francisco", "Seattle", "Denver",
          "Portland", "Las Vegas", "Tucson", "Fresno", "Mesa", "Sacramento",
          "Colorado Springs", "Long Beach", "Oakland", "Bakersfield",  "Aurora",
          "Honolulu", "Santa Ana", "Riverside", "Stockton", "Henderson", "Irvine",
          "Chula Vista", "Chandler", "Scottsdale", "Reno", "Glendale AZ", "Gilbert",
          "North Las Vegas", "Fremont", "Boise", "Spokane", "Tacoma", "San Bernardino",
          "Modesto", "Fontana", "Santa Clarita", "Glendale CA", "Oxnard", "Moreno Valley",
          "Huntington Beach", "Salt Lake City")

neighbourhoods_table <- 
  neighbourhoods_table %>% 
  mutate(region = ifelse(city %in% northeast, "Northeast",
                         ifelse(city %in% midwest, "Midwest",
                                ifelse(city %in% south, "South",
                                       ifelse(city %in% west, "West", "error"))))) 

# Manipulate the media file for temporal analysis

media <-
  map2(media, cityname, ~{
    .x %>% 
      mutate(
        city = .y)
  })

# Reduce to only the variables we will use

media <- 
  map(media, ~{
    .x %>% 
      dplyr::select(c("Date", "sentiment", "city"))
  })

# Create a dataframe from the list

media_table <- 
  do.call(rbind, media)

# Add regional variable

media_table <- 
  media_table %>% 
  mutate(region = ifelse(city %in% northeast, "Northeast",
                         ifelse(city %in% midwest, "Midwest",
                                ifelse(city %in% south, "South",
                                       ifelse(city %in% west, "West", "error"))))) 


# Create city-wide table for modeling

cities_table <- 
  neighbourhoods_table %>% 
  group_by(city) %>% 
  summarize(families = sum(families * population, na.rm = TRUE),
            med_income = sum(med_income * households)/sum(households, na.rm = TRUE),
            population = sum(population, na.rm = TRUE),
            households = sum(households, na.rm = TRUE), 
            university_education = sum(university_education, na.rm = TRUE), 
            housing_need = sum(housing_need, na.rm = TRUE), 
            non_mover = sum(non_mover, na.rm = TRUE), 
            owner_occupied = sum(owner_occupied, na.rm = TRUE),
            rental = sum(rental, na.rm = TRUE),
            language = sum(language, na.rm = TRUE), 
            citizen = sum(citizen, na.rm = TRUE),
            white = sum(white, na.rm = TRUE), 
            low_income = sum(low_income, na.rm = TRUE),
            lone_parent = sum(lone_parent, na.rm = TRUE))

cities_table <- 
  cities_table %>% 
  mutate(university_education_pct_pop = university_education/population,
         housing_need_pct_household = housing_need/households, 
         non_mover_pct_pop = non_mover/population,
         owner_occupied_pct_household = owner_occupied/households,
         rental_pct_household = rental/households,
         language_pct_pop = language / population,
         citizen_pct_pop = citizen / population, 
         white_pct_pop = white / population,
         low_income_pct_pop = low_income/population,
         lone_parent_pct_families = lone_parent/families)

cities_table <- 
  cities_table %>% 
  mutate_at(
    .vars = 
      c("population", "households", "med_income", "university_education", 
        "housing_need", "non_mover", "owner_occupied", "rental", "language", 
        "citizen", "white", "low_income", "lone_parent", 
        "university_education_pct_pop", "housing_need_pct_household", 
        "non_mover_pct_pop", "owner_occupied_pct_household", 
        "rental_pct_household", "language_pct_pop", "citizen_pct_pop", 
        "white_pct_pop", "low_income_pct_pop", "lone_parent_pct_families"),
    .funs = list(`z` = ~{(.- mean(., na.rm = TRUE))/sd(., na.rm = TRUE)})) 

cities_table <- 
  media_table %>% 
  group_by(city) %>% 
  summarize(sentiment = mean(sentiment, na.rm = TRUE)) %>% 
  left_join(cities_table, .)
    
cities_table <- 
  media_table %>% 
  group_by(city) %>% 
  count() %>% 
  left_join(cities_table, .) %>% 
  mutate(CRI = -1 * n * sentiment)

cities_table <- 
  media_table %>% 
  filter(Date >= "2018-01-01") %>% 
  group_by(city) %>% 
  summarize(sentiment_2yr = mean(sentiment, na.rm = TRUE)) %>% 
  left_join(cities_table, .)

cities_table <- 
  media_table %>% 
  filter(Date >= "2018-01-01") %>% 
  group_by(city) %>% 
  count() %>% 
  left_join(cities_table, .) %>% 
  mutate(CRI_2yr = -1 * n * sentiment_2yr)

cities_table <- 
  media_table %>% 
  filter(Date >= "2019-01-01") %>% 
  group_by(city) %>% 
  summarize(sentiment_1yr = mean(sentiment, na.rm = TRUE)) %>% 
  left_join(cities_table, .)

cities_table <- 
  media_table %>% 
  filter(Date >= "2019-01-01") %>% 
  group_by(city) %>% 
  count() %>% 
  left_join(cities_table, .) %>% 
  mutate(CRI_1yr = -1 * n * sentiment_1yr)

 cities_table <- 
  neighbourhoods_table %>%
  group_by(city) %>% 
  summarize(active_listings = sum(active_listings, na.rm = TRUE),
            active_listings_avg = sum(active_listings_avg, na.rm = TRUE),
            EH = sum(EH, na.rm = TRUE),
            revenue_LTM = sum(revenue_LTM, na.rm = TRUE),
            GH = sum(GH, na.rm = TRUE),
            FREH = sum(FREH, na.rm = TRUE), 
            ML = sum(ML, na.rm = TRUE),
            GH_housing = sum(GH_housing, na.rm = TRUE),
            housing_loss = sum(housing_loss, na.rm = TRUE), 
            PR_10 = sum(PR_10, na.rm = TRUE),
            PR_25 = sum(PR_25, na.rm = TRUE),
            PR_50 = sum(PR_50, na.rm = TRUE)) %>% 
   st_drop_geometry() %>% 
   left_join(cities_table, .)

 cities_table <- 
   neighbourhoods_table %>%
   mutate(active_listings_prev = active_listings/active_listings_yoy) %>% 
   group_by(city) %>% 
   summarize(active_listings_prev = sum(active_listings_prev, na.rm = TRUE)) %>% 
   st_drop_geometry() %>% 
   left_join(cities_table, .) %>% 
   mutate(active_listings_yoy = active_listings/active_listings_prev)

 cities_table <- 
   neighbourhoods_table %>% 
   mutate(revenue_10pct_absolute = revenue_LTM * revenue_10pct) %>% 
   group_by(city) %>% 
   summarize(revenue_10pct = sum(revenue_10pct_absolute, na.rm = TRUE)/sum(revenue_LTM, na.rm = TRUE)) %>% 
   st_drop_geometry() %>% 
   left_join(cities_table, .)

cities_table <- 
  cities_table %>% 
  mutate(EH_pct = EH/active_listings,
         revenue_LTM_per_listing = revenue_LTM/active_listings_avg,
         GH_pct = GH/active_listings,
         FREH_pct = FREH/active_listings_avg,
         ML_pct = ML/active_listings,
         housing_loss_pct_households = housing_loss/households,
         housing_loss_pct_listings = housing_loss/active_listings,
         PR_10_pct = PR_10/active_listings,
         PR_25_pct = PR_25/active_listings,
         PR_50_pct = PR_50/active_listings)
 

cities_table <- 
  cities_table %>% 
  mutate(region = ifelse(city %in% northeast, "northeast",
                         ifelse(city %in% midwest, "midwest",
                                ifelse(city %in% south, "south",
                                       ifelse(city %in% west, "west", "error"))))) 

# Add city-level measures to the neigbhourhood table

neighbourhoods_table <- 
  neighbourhoods_table %>% 
  left_join(cities_table %>% st_drop_geometry(), 
            by = "city", suffix = c("", "_city"))


save(media_table, neighbourhoods_table, cities_table, file = "data/modeling_data.Rdata")

