######################################### AIRBNB DATA IMPORT ###############################

source("R/01_helper_functions.R")

# Set up date range
start_date <- "2018-05-01"
end_date <- "2019-04-30"

# Set up database connection. This requires a VPN.
con <- RPostgres::dbConnect(RPostgres::Postgres(), dbname = "airdna")

property_db <- tbl(con, "property")
daily_db <- tbl(con, "daily")

# Import property and daily files from the database
property <-
  property_db %>%
  filter(city == cityname, created <= end_date,
        scraped >= start_date) %>%
 collect()

daily <-
  daily_db %>%
  filter(property_ID %in% !!property$property_ID,
         start_date >= start_date) %>%
  collect()

# Expand the daily file
daily <- daily %>% 
  strr_expand_daily()

# Set columns, select housing, and transform geometries
property <- property %>% 
  select(c("property_ID", "listing_title", "property_type", "listing_type",
           "created", "scraped", "latitude", "longitude", "ab_property",
           "ab_host", "ha_property", "ha_host")) %>% 
  set_names(c("Property_ID", "Listing_Title", "Property_Type", "Listing_Type",
              "Created", "Scraped", "Latitude", "Longitude", "Airbnb_PID",
              "Airbnb_HID", "HomeAway_PID", "HomeAway_HID")) %>% 
  arrange(Property_ID) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(transform) %>% 
  filter(Property_Type %in% c(
    "House", "Private room in house", "Apartment", "Cabin",
    "Entire condominium", "Townhouse", "Condominium", "Entire apartment",
    "Private room", "Loft", "Place", "Entire house", "Villa", "Guesthouse",
    "Private room in apartment", "Guest suite", "Shared room in dorm",
    "Chalet", "Dorm", "Entire chalet", "Shared room in loft", "Cottage",
    "Resort", "Serviced apartment", "Other", "Bungalow", "Farm stay",
    "Private room in villa", "Entire loft", "Entire villa",
    "Private room in guesthouse", "Island", "Entire cabin", "Vacation home",
    "Entire bungalow", "Earth house", "Nature lodge", "In-law",
    "Entire guest suite", "Shared room in apartment", "Private room in loft",
    "Tiny house", "Castle", "Earth House", "Private room in condominium",
    "Entire place", "Shared room", "Hut", "Private room in guest suite",
    "Private room in townhouse", "Timeshare", "Entire townhouse",
    "Shared room in house", "Entire guesthouse", "Shared room in condominium",
    "Cave", "Private room in cabin", "Dome house",
    "Private room in vacation home", "Private room in dorm",
    "Entire serviced apartment", "Private room in bungalow",
    "Private room in serviced apartment", "Entire Floor", "Entire earth house",
    "Entire castle", "Shared room in chalet", "Shared room in bungalow",
    "Shared room in townhouse", "Entire cottage", "Private room in castle",
    "Private room in chalet", "Private room in nature lodge", "Entire in-law",
    "Shared room in guesthouse", "Casa particular", "Serviced flat", "Minsu",
    "Entire timeshare", "Shared room in timeshare", "Entire vacation home",
    "Entire nature lodge", "Entire island", "Private room in in-law",
    "Shared room in serviced apartment", "Shared room in cabin", "Entire dorm",
    "Entire cave", "Private room in timeshare", "Shared room in guest suite",
    "Private room in cave", "Entire tiny house",
    "Private room in casa particular (cuba)", "Casa particular (cuba)",
    "Private room in cottage", "Private room in tiny house",
    "Entire casa particular", "")) %>% 
  select(-Property_Type)

daily <- daily %>% 
  select(c("property_ID", "date", "status", "price")) %>%
  set_names(c("Property_ID", "Date", "Status", "Price")) %>%
  filter(!is.na(Status)) %>%
  arrange(Property_ID, Date)
  
# Trim listings
property <-
  property %>% 
  filter(Property_ID %in% daily$Property_ID)

# Join property and daily file
daily <- inner_join(daily, st_drop_geometry(property)) %>% 
  select(Property_ID, Date, Status, Price, Airbnb_PID, HomeAway_PID, Airbnb_HID,
         HomeAway_HID, Listing_Type, Created, Scraped)

daily <- daily %>% 
  filter(Date >= Created, Date <= Scraped + 30)

# Find FREH listings
daily_FREH <- strr_FREH(daily, start_date = end_date, end_date = end_date)

daily_FREH <- daily_FREH %>% 
  filter(FREH == TRUE) %>% 
  select(Property_ID) %>% 
  distinct()

property <- property %>% 
  mutate(FREH = property$Property_ID %in% daily_FREH$Property_ID)

rm(daily_FREH)

# Identify ghost hotels
GH_list <-
  strr_ghost(property, Property_ID, Airbnb_HID, Created, Scraped, start_date,
             end_date, listing_type = Listing_Type) %>% 
  pull(property_IDs) %>%
  unlist() %>%
  unique()

property <-
  property %>% 
  mutate(GH = if_else(Property_ID %in% GH_list, TRUE, FALSE))

rm(GH_list)

# Save files
save(property, file = ".Rdata")
save(daily, file = ".Rdata")
