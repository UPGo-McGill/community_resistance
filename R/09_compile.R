######################################### COMPILE ###############################

source("R/01_helper_functions.R")

# Cast geometries as polygons and add city name as a variable

neighbourhoods <-
  map2(neighbourhoods, cityname, ~{
    .x %>% 
      mutate(
        #geometry = st_union(st_collection_extract(geometry, "MULTIPOLYGON")),
             city = .y)
  })


neighbourhoods_table <- 
  do.call(rbind, neighbourhoods)


# # Add country field
# neighbourhoods_table <- 
#   st_join(neighbourhoods_table %>% 
#             st_as_sf() %>% 
#             st_transform(102009), nation() %>% 
#                                   st_as_sf() %>% 
#                                   st_transform(102009) %>% 
#                                   dplyr::select(NAME), 
#          left = TRUE) %>% 
#   mutate(country = ifelse(is.na(NAME), "Canada", "United States")) %>% 
#   dplyr::select(-NAME)
#  
# 

