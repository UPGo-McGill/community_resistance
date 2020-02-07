######################################### CITY INFORMATION INPUT ########################################################

source("R/01_helper_functions.R")

# List of city names
cityname <- c("Montreal", "Toronto", "Vancouver",
              "Los Angeles", "Miami", "New Orleans", 
              "New York", "San Francisco", "Washington")

# List of neighbourhoods with geometries
neighbourhoods_canada <- list("Montreal" = read_sf(dsn = "data/neighbourhoods", layer = "montreal" )%>%
                                st_transform(102009) %>% 
                                dplyr::select(CODE_ID = CODEID, neighbourhood = NOM, geometry) ,
                              "Toronto" = read_sf(dsn = "data/neighbourhoods", layer = "toronto" )%>%
                                st_transform(102009) %>% 
                                dplyr::select(CODE_ID = AREA_S_CD, neighbourhood = AREA_NAME, geometry),
                              "Vancouver" = read_sf(dsn = "data/neighbourhoods", layer = "vancouver" )%>%
                                st_transform(102009) %>% 
                                dplyr::select(CODE_ID = MAPID, neighbourhood = NAME, geometry))

neighbourhoods_us <- list("Los Angeles" = pumas("CA", class = "sf") %>% 
                         st_transform(102009) %>%
                         mutate(neighbourhood = NAMELSAD10) %>% 
                         dplyr::select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
                                       -AWATER10, -INTPTLAT10, -INTPTLON10) %>% 
                         dplyr::select(CODE_ID = PUMACE10, neighbourhood, geometry)%>%
                         filter(str_detect(neighbourhood, "Angeles")),
                       "Miami" = pumas("FL", class = "sf") %>% 
                         st_transform(102009) %>%
                         mutate(neighbourhood = NAMELSAD10) %>% 
                         dplyr::select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
                                       -AWATER10, -INTPTLAT10, -INTPTLON10) %>% 
                         dplyr::select(CODE_ID = PUMACE10, neighbourhood, geometry)%>%
                         filter(str_detect(neighbourhood, "Miami")), 
                       "New Orleans" = pumas("LA", class = "sf") %>% 
                         st_transform(102009) %>%
                         mutate(neighbourhood = NAMELSAD10) %>% 
                         dplyr::select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
                                       -AWATER10, -INTPTLAT10, -INTPTLON10) %>% 
                         dplyr::select(CODE_ID = PUMACE10, neighbourhood, geometry)%>%
                         filter(str_detect(neighbourhood, "New Orleans")), 
                      "New York" = pumas("NY", class = "sf") %>% 
                         st_transform(102009) %>%
                         mutate(neighbourhood = NAMELSAD10) %>% 
                         dplyr::select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
                                       -AWATER10, -INTPTLAT10, -INTPTLON10) %>% 
                         dplyr::select(CODE_ID = PUMACE10, neighbourhood, geometry)%>%
                         filter(str_detect(neighbourhood, "NYC")),
                      "San Francisco" = pumas("CA", class = "sf") %>% 
                        st_transform(102009) %>%
                        mutate(neighbourhood = NAMELSAD10) %>% 
                        dplyr::select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
                                      -AWATER10, -INTPTLAT10, -INTPTLON10) %>% 
                        dplyr::select(CODE_ID = PUMACE10, neighbourhood, geometry)%>%
                        filter(str_detect(neighbourhood, "San Fran")), 
                      "Washington" = pumas("DC", class = "sf") %>% 
                        st_transform(102009) %>%
                        mutate(neighbourhood = NAMELSAD10) %>% 
                        dplyr::select(-GEOID10, -NAMELSAD10, -STATEFP10, -MTFCC10, -FUNCSTAT10, -ALAND10,
                                      -AWATER10, -INTPTLAT10, -INTPTLON10) %>% 
                        dplyr::select(CODE_ID = PUMACE10, neighbourhood, geometry))

neighbourhoods <- append(neighbourhoods_canada, neighbourhoods_us)

rm(neighbourhoods_canada, neighbourhoods_us)

