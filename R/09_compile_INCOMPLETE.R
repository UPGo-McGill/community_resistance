######################################### COMPILE ###############################

source("R/01_helper_functions.R")

# Cast geometries as polygons and add city naem as a variable

neighbourhoods <- map(seq_along(neighbourhoods), ~{
  neighbourhoods[[.x]] %>% 
    st_cast("POLYGON") %>% 
    mutate(city = cityname[[.x]])
})

# Create a datatable from the neighbourhood list

neighbourhoods_table <- rbindlist(neighbourhoods) 

# Add country field
 
neighbourhoods_table <- 
  st_join(neighbourhoods_table %>% 
            st_as_sf() %>% 
            st_transform(102009), us_boundary %>% dplyr::select(NAME), 
         left = TRUE) %>% 
  mutate(country = ifelse(is.na(NAME), "Canada", "United States")) %>% 
  dplyr::select(-NAME)
 
