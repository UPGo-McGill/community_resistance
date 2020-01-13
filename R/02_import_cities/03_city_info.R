######################################### CITY INFORMATION INPUT ########################################################

source("R/01_import_general/01_helper_functions.R")

# Enter city and country name.
# Assign the appropriate census tracts data 
cityname <- ""
country <- "United States"
CTs <- CTs_us


# Use st_transform to 32618 for Canada or 26918 for the United States
transform <- ifelse(country == "Canada", 32618, 26918)


# Upload required geometries. Ensure that there is a field titled neighbourhood in the geometries file
# US - PUMAS
neighbourhoods <- pumas("NY", class = "sf") %>% 
  st_transform(transform) %>%
  as_tibble() %>% 
  st_as_sf() %>%
  mutate(neighbourhood = NAMELSAD10) %>% 
  select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
         -AWATER10, -INTPTLAT10, -INTPTLON10) %>% 
  select(CODE_ID = PUMACE10, neighbourhood, geometry)%>%
  filter(str_detect(neighbourhood, "NYC-"))

# Otherwise import from downloaded shapefiles
neighbourhoods <-
  read_sf(dsn = "Data", layer = "")%>%
  st_transform(transform) %>% 
  select(CODE_ID = slug, neighbourhood = display_na, geometry)

# Edit the neighbourhood names
# Run for Toronto
neighbourhoods <- neighbourhoods %>% 
  separate(neighbourhood, into = c("neighbourhood", NA), sep = "[(]")
# Run for all neighbourhoods to remove punctuation
neighbourhoods$neighbourhood <- neighbourhoods$neighbourhood %>% 
  gsub("[[:punct:]]", " ", .)
