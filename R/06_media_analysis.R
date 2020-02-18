######################################### MEDIA ANALYSIS ########################################################

source("R/01_helper_functions.R")


############################################ 1 - SENTIMENT ANALYSIS ##############################################################

# Create dictionary using the QDAP dictionary as a starting point

dictionary <- c(SentimentDictionaryBinary(qdapDictionaries::positive.words,
                                          qdapDictionaries::negative.words))

machine_learning_synonyms = c("protest", "anti", "affordability", "mobilize", "mobilise",
                              "mobilization", "mobilisation", "oppose",  "resist", 
                              "opposition", "gentrification", "threaten", "rent", 
                              "expensive", "unaffordable", "eviction", "landlord",
                              "threat", "manifestation", "complaint", "disapprove",
                              "evict", "overtourism", "detriment", "ghost", "nuisance",
                              "consultation", "opponent",  "discrimination", "critic", 
                              "crisis", "shortage", "blame", "garbage", "noise", "complain", 
                              "concern", "coalition", "hostile", "hostility", "fairbnb", 
                              "activist", "activism", "displace", "illegal", "housing",
                              "market", "multiple", "listings", "disturbance", "damage", 
                              "threat", "residence", "hotel", "gap", "investment", "poor", 
                              "need", "rent hike", "community led", "housing stock", "nuisance",
                              "garbage", "noise", "party", "disrespect", "lockbox", "regulation",
                              "unregulate", "scarce", "fines", "fined", "shut")

dictionary[["negativeWords"]] = c(dictionary[["negativeWords"]], machine_learning_synonyms)

# Assign a score to each article

media <- map(seq_along(media), ~{
  
 media[[.x]] %>% 
    mutate(
      sentiment = (str_count(lemmatized_articles[[.x]]$lemmas, paste(dictionary[["positiveWords"]], collapse = '|'))*1 +
                     str_count(lemmatized_articles[[.x]]$lemmas, paste(dictionary[["negativeWords"]], collapse = '|'))*-1) /
        as.numeric(media[[.x]]$Word_Count))
})


############################### 2 - NAMED ENTITY RECOGNITION AND GEOCODING ################################################################## 

ner <- 
  map(media, ~{
    rbind(
      spacy_parse(.x$Article) %>% 
        entity_extract(type = "named", concatenator = " ") %>% 
        filter(entity_type == "GPE" |
                 entity_type == "FAC" |
                 entity_type == "ORG" |
                 entity_type == "LOC" |
                 entity_type == "PERSON") %>% 
        filter(entity != cityname[.x]) %>% 
        filter(nchar(entity) > 2) %>% 
        dplyr::select(doc_id, entity),
      spacy_parse(.x$Headline) %>% 
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
  map_df(ner, ~{
    .x %>% 
      dplyr::select(entity)
    }) %>% 
  distinct()

ner_compressed$entity <- ner_compressed$entity %>% 
  gsub("[[:punct:]]", " ", .)

# Load locations that have already been queried.
load("data/locations.Rdata")

# Find only the locations that have yet to be queried.
ner_compressed <- ner_compressed %>% 
  anti_join(locations)

# Query Google to geocode the locations for their latitude and longitude
locations_new <- mutate_geocode(ner_compressed, entity)

# Join with the existing locations
locations <- locations_new %>% 
  rbind(locations)

# Save the locations so that this does not need to be rerun
save(locations, file = "locations.Rdata")

# Remove the locations that were not geocoded
locations <- locations %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(102009)

# Perform a join to associate each entity with each document id
ner_locations <- map(ner, ~{
  inner_join(.x, locations)
})

# Perform a spatial join to determine what locations fall into which neighbourhoods

ner_locations <- map(seq_along(cityname), ~{
  ner_locations[[.x]] %>%
    st_as_sf() %>% 
    st_transform(102009) %>% 
    st_join(neighbourhoods[[.x]], join = st_intersects) %>% 
    filter(!is.na(neighbourhood))
    })

for (n in seq_along(cityname)) {

ner_locations[[n]]$doc_id <- as.numeric(gsub("text", "",ner_locations[[n]]$doc_id)) }


############################################## 3 - NEIGHBOURHOOD SENTIMENT #################################################

# Determine a community resistance index per neighbourhood
  # Dependent on the number of mentions per neighbhourhood and the average sentiment of the articles

for (n in seq_along(cityname)) {
  
  temp <- ner_locations[[n]] %>% 
    left_join(media[[n]] %>% dplyr::select(c("ID", "sentiment")), by = c("doc_id" = "ID")) %>% 
    dplyr::select(c("doc_id", "neighbourhood", "sentiment")) %>% 
    st_drop_geometry() %>% 
    distinct()
  
  neighbourhoods[[n]] <- 
    neighbourhoods[[n]] %>% 
    left_join(left_join(aggregate(temp$sentiment, list(temp$neighbourhood), mean),
                        temp %>% 
                          group_by(neighbourhood) %>% 
                          tally(), by = c("Group.1" = "neighbourhood")), by = c("neighbourhood" = "Group.1")) %>% 
    mutate(sentiment = x,
           media_count = n, 
           CRI = -1*sentiment*media_count) %>% 
    dplyr::select(-c("x", "n"))
  
  rm(temp)
  
}



