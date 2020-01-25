######################################### MEDIA ANALYSIS ########################################################

source("R/01_helper_functions.R")

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
  st_transform(102009)

# Perform a join to associate each entity with each document id
ner_locations <- map(seq_along(cityname), ~{
  inner_join(ner[[.x]], locations)
})


# Perform a spatial join to determine what locations fall into which neighbourhoods
ner_locations <- ner_locations %>%
  st_as_sf() %>% 
  st_transform(102009) %>% 
  st_join(neighbourhoods, join = st_intersects) %>% 
  filter(!is.na(neighbourhood))


ner_locations <- map(seq_along(cityname), ~{
  ner_locations[[.x]] %>%
    st_as_sf() %>% 
    st_transform(102009) %>% 
    st_join(neighbourhoods[[.x]], join = st_intersects) %>% 
    filter(!is.na(neighbourhood))
    })

for (n in seq_along(cityname)){

ner_locations[[n]]$doc_id <- as.numeric(gsub("text", "",ner_locations[[n]]$doc_id)) }


############################################ 2 - SENTIMENT ANALYSIS ##############################################################

# ANALYSIS
# ADD TO THE NEIGHBOURHOODS LIST INSTEAD OF CREATING A NEW DATAFRAME
