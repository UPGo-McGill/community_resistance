######################################### MEDIA ANALYSIS ########################################################

source("R/01_import_general/01_helper_functions.R")

############################### 1 - NAMED ENTITY RECOGNITION AND GEOCODING ################################################################## 

ner <- 
  map(seq_along(cityname), ~{
    rbind(
      spacy_parse(media[[.x]]$Article) %>% 
        entity_extract(type = "named", concatenator = " ") %>% 
        filter(entity_type == "GPE" |
                 entity_type == "FAC" |
                 entity_type == "ORG" |
                 entity_type == "LOC" |
                 entity_type == "PERSON") %>% 
        filter(entity != cityname[.x]) %>% 
        filter(nchar(entity) > 2) %>% 
        dplyr::select(doc_id, entity),
      spacy_parse(media[[.x]]$Headline) %>% 
        entity_extract(type = "named", concatenator = " ") %>% 
        filter(entity_type == "GPE" |
                 entity_type == "FAC" |
                 entity_type == "ORG" |
                 entity_type == "LOC" |
                 entity_type == "PERSON") %>% 
        filter(entity != cityname[.x]) %>% 
        filter(nchar(entity) > 2) %>% 
        dplyr::select(doc_id, entity)) %>% 
      distinct() 
  })

# Collapse the named entity recognition to reduce processing times and number of api queries
ner_compressed <- 
  map_df(seq_along(cityname), ~{
    ner[[.x]] %>% 
      dplyr::select(entity)
    }) %>% 
  distinct()

# Query Google to geocode the locations for their latitude and longitude
ner_compressed$entity <- ner_compressed$entity %>% 
  gsub("[[:punct:]]", " ", .)

locations <- mutate_geocode(ner_compressed, entity)

# Save the locations so that this does not need to be rerun
save(locations, file = "locations.Rdata")

# Remove the locations that were not geocoded
locations <- locations %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(transform)

# Perform a join to associate each entity with each document id

ner_locations <- map(cityname, ~{
  ner[[.x]]
})


 ner_local[] %>% 
  inner_join(locations_local)



# Perform a spatial join to determine what locations fall into which neighbourhoods
locations_local <- locations_local %>%
  st_as_sf() %>% 
  st_transform(transform) %>% 
  st_join(neighbourhoods, join = st_intersects) %>% 
  filter(!is.na(neighbourhood))

locations_local$doc_id <- as.numeric(gsub("text", "",locations_local$doc_id))




