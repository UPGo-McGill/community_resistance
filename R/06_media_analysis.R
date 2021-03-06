#### 06. MEDIA ANALYSIS ########################################################

source("R/01_helper_functions.R")

### Sentiment analysis #########################################################

## Create dictionary using the QDAP dictionary as a starting point

dictionary <- 
  c(SentimentDictionaryBinary(qdapDictionaries::positive.words,
                                          qdapDictionaries::negative.words))

machine_learning_synonyms <- 
  c("protest", "anti", "affordability", "mobilize", "mobilise", "mobilization", 
    "mobilisation", "oppose",  "resist", "opposition", "gentrification", 
    "threaten", "rent", "expensive", "unaffordable", "eviction", "landlord",
    "threat", "manifestation", "complaint", "disapprove", "evict", 
    "overtourism", "detriment", "ghost", "nuisance", "consultation", "opponent",
    "discrimination", "critic", "crisis", "shortage", "blame", "garbage", 
    "noise", "complain", "concern", "coalition", "hostile", "hostility", 
    "fairbnb", "activist", "activism", "displace", "illegal", "housing", 
    "market", "multiple", "listings", "disturbance", "damage", "threat", 
    "residence", "hotel", "gap", "investment", "poor", "need", "rent hike", 
    "community led", "housing stock", "nuisance", "garbage", "noise", "party", 
    "disrespect", "lockbox", "regulation", "unregulate", "scarce", "fines", 
    "fined", "shut", "backlash", "homeless", "homelessness", "affordability")

dictionary[["negativeWords"]] <- 
  c(dictionary[["negativeWords"]], machine_learning_synonyms)


## Assign a score to each article

media <- 
  future_map2(media, lemmatized_articles, ~{
    .y <- 
      .y %>% 
      mutate(lemmas = unlist(map(lemmas, str_split, " "), recursive = FALSE),
             lemmas = map(lemmas, ~.x[str_detect(.x, "")]))
    .x %>% 
      mutate(
        sentiment = map_int(.y$lemmas, ~{
          sum(.x %in% dictionary[["positiveWords"]]) -
            sum(.x %in% dictionary[["negativeWords"]])
        }) / as.numeric(.x$Word_Count))
  }, .progress = TRUE)

rm(dictionary, machine_learning_synonyms)


### Named entity recognition and geocoding ##################################### 

# NER using spacy

ner <- 
  map(media, ~{
    rbind(
      spacy_parse(.x$Article) %>% 
        entity_extract(type = "named", concatenator = " ") %>% 
        filter(entity_type == "GPE" |
                 entity_type == "FAC" |
                 entity_type == "LOC" |
                 entity_type == "PERSON" |
                 entity_type == "ORG") %>% 
        filter(nchar(entity) > 2) %>% 
        dplyr::select(doc_id, entity),
      spacy_parse(.x$Headline) %>% 
        entity_extract(type = "named", concatenator = " ") %>% 
        filter(entity_type == "GPE" |
                 entity_type == "FAC" |
                 entity_type == "LOC" |
                 entity_type == "PERSON" |
                 entity_type == "ORG") %>% 
        filter(nchar(entity) > 2) %>% 
        dplyr::select(doc_id, entity)) %>% 
      distinct() 
  })

save(ner, file = "data/ner.Rdata")


# NER using alternate method

# ner <- 
#   lemma_intermediate[1:60] %>% 
#   future_map(ner_fun, .progress = TRUE)
# 
# ner_2 <- 
#   lemma_intermediate[61:80] %>% 
#   future_map(ner_fun, .progress = TRUE)
# 
# ner_3 <- 
#   lemma_intermediate[81:115] %>% 
#   map(~{
#     
#     no_docs <- 
#       ceiling(.x %>% group_by(doc_id) %>% n_groups() / 60)
#     
#     no_iterations <- 
#       ceiling(.x %>% group_by(doc_id) %>% n_groups() / no_docs)
#     
#     chunks <- 
#       map(1:no_iterations, function(x) {
#         i <- 1 + (x - 1) * no_docs
#         .x %>% filter(doc_id %in% c(i:(i + no_docs - 1)))
#       })
#     
#     chunks %>% 
#       future_map(ner_fun, .progress = TRUE) %>% 
#       bind_rows()
#   })
# 
# # Combine ner pieces  
# ner <- c(ner, ner_2, ner_3, ner_109)
# 
# # Clean the named entities by removing punctuation and making all lowercase
# ner <- map(ner, mutate, entity = tolower(gsub("[[:punct:]]", " ", entity)))
# 
# save(ner, file = "data/ner.Rdata")



# Collapse the named entity recognition to reduce API queries
ner_compressed <-
  map_df(ner, dplyr::select, entity) %>%
  distinct()

ner_compressed$entity <- ner_compressed$entity %>% 
  gsub("[[:punct:]]", " ", .)


# Load locations that have already been queried.
load("data/locations.Rdata")

# Find only the locations that have yet to be queried.
ner_compressed <-
  ner_compressed %>% 
  anti_join(locations)

# Query Google to geocode the locations for their latitude and longitude
# NOTE: IF THE API RUNS OUT DURING THE QUERY, JOIN THE OUTPUT (LOCATIONS_NEW) 
# WITH EXISTING LOCATIONS, SAVE LOCATIONS. 
# AND RERUN. 
locations_new <- 
  mutate_geocode(ner_compressed, entity)

# Join with the existing locations
locations <- 
  locations_new %>% 
  rbind(locations)

# Save the locations so that this does not need to be rerun
save(locations, file = "data/locations.Rdata")

# Remove the locations that were not geocoded
locations <- 
  locations %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(102009)

# Perform a join to associate each entity with each document id
ner_locations <- 
  future_map(ner, inner_join, locations, by = "entity", .progress = TRUE)

# Perform a spatial join to match locations to neighbourhoods
ner_locations <-
  future_map2(ner_locations, neighbourhoods, ~{
    .x %>%
      st_as_sf() %>% 
      st_transform(102009) %>% 
      st_join(.y) %>%
      filter(!is.na(neighbourhood))
    }, .progress = TRUE)


### Neighbourhood sentiment ####################################################

# Determine a community resistance index per neighbourhood
# Filter dates and rename variables according to what CRI you would like to calculate

for (n in seq_along(cityname)) {
  
  ner_locations[[n]]$doc_id <- 
    as.integer(ner_locations[[n]]$doc_id)
  
  media[[n]]$Date <- 
    as.Date(media[[n]]$Date)
  
  temp <- 
    ner_locations[[n]] %>% 
    dplyr::select(c("doc_id", "neighbourhood")) %>% 
    st_drop_geometry() %>% 
    inner_join(media[[n]] %>% 
                 filter(Date >= "2019-01-01") %>% 
                # filter(Date >= "2018-01-01") %>% 
                 dplyr::select(c("doc_id", "sentiment")), .) %>% 
    distinct()
  
  neighbourhoods[[n]] <- 
    neighbourhoods[[n]] %>% 
    mutate(sentiment_1yr = NA,
           media_count_1yr = NA,
           CRI_1yr = NA)
         
if (nrow(temp) != 0) {
  neighbourhoods[[n]] <- 
    neighbourhoods[[n]] %>% 
    left_join(left_join(aggregate(temp$sentiment, list(temp$neighbourhood), 
                                  mean),
                        temp %>% 
                          group_by(neighbourhood) %>% 
                          tally(), by = c("Group.1" = "neighbourhood")), 
              by = c("neighbourhood" = "Group.1")) %>% 
    mutate(sentiment_1yr = x,
           media_count_1yr = n, 
           CRI_1yr = -1 * sentiment_1yr * media_count_1yr) %>% 
    dplyr::select(-c("x", "n"))
}
  
  rm(temp)
  
}

save(neighbourhoods, file = "data/neighbourhoods_script_6.Rdata")
save(media, file = "data/media.Rdata")
file.remove("data/end_of_script_5.Rdata")

rm(lemma_intermediate, lemmatized_articles, locations, media, ner, 
   ner_compressed, ner_locations)
