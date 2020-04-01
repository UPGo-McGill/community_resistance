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

media <- 
  future_map2(media, lemmatized_articles, ~{
    .x %>% 
      mutate(
        sentiment = (str_count(.y$lemmas, paste(dictionary[["positiveWords"]], collapse = '|')) * 1 +
                       str_count(.y$lemmas, paste(dictionary[["negativeWords"]], collapse = '|')) * -1) /
          as.numeric(.x$Word_Count))
  })


############################### 2 - NAMED ENTITY RECOGNITION AND GEOCODING ################################################################## 

output <- 
  map(lemma_intermediate, ~{
    
# Isolate proper nouns
    
    .x <- 
      .x %>% 
      filter(upos == "PROPN") %>% 
      dplyr::select(c(doc_id, term_id, lemma)) %>% 
      mutate(mid = TRUE) %>% 
      mutate(end = TRUE)
    
# However, some proper nouns are more than one word.
# These have to be joined accordingly, based on the term id.
    
  for (i in 1:nrow(.x)) {
    .x[i,] <- 
       .x[i,] %>% 
        mutate(mid = 
                 ifelse(term_id ==
                              as.numeric(.x[i-1,] %>% 
                                         dplyr::select(term_id) + 1), 
                            TRUE, 
                            FALSE))
    }  
    
  for (i in 1:nrow(.x)) {
      .x[i,] <-
        .x[i,] %>%
        mutate(end = 
                 ifelse(.x[i,]$mid == TRUE &
                        .x[i+1,]$mid == FALSE,
                          TRUE,
                          FALSE))
    }
    
    .x <- 
      .x %>% 
      mutate(position = 
               case_when(end == TRUE ~ "end",
                         end == FALSE & mid == TRUE ~ "middle",
                         end == FALSE & mid == FALSE ~ "start",
                         is.na(mid) ~ "start")) %>% 
      dplyr::select(-c(mid, end)) 
    
    
    .x[nrow(.x), ]$position = 
      ifelse(.x[nrow(.x)-1, ]$position == "middle",
             "end", 
             "start")
    
# Note: this portion CANNOT be run in parallel in terms of i.
    for (i in nrow(.x):1) {
      if  (.x[i,]$position == "end" |
           .x[i,]$position == "middle") {
        .x[i-1,]$lemma = 
          paste(.x[i-1,]$lemma,
                .x[i,]$lemma)
      }
    }
    
# Create named entities table  
    ner <- 
      .x %>% 
      filter(position == "start") %>% 
      mutate(entity = lemma) %>% 
      filter(nchar(entity) > 2, 
             entity != "USA",
             entity != "U.S.",
             entity != "America",
             entity != "United States") %>%
      dplyr::select(c("doc_id", "entity"))    
    
    list(ner)
    
  })

ner <- 
  output %>% 
  map(~{.x[[1]]})

# Clean the named entities by removing punctuation and making all lowercase
output <- 
  map(ner, ~{
    .x$entity <- 
      .x$entity %>% 
      gsub("[[:punct:]]", " ", .) %>% 
      tolower()
  })

ner <-
  map2(ner, output, ~{
    .x %>% 
      mutate(entity = .y)
  })

# Collapse the named entity recognition to reduce processing times and number of api queries
ner_compressed <- 
  map_df(ner, ~{
    .x %>% 
      dplyr::select(entity)
  }) %>% 
  distinct()

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
save(locations, file = "locations.Rdata")

# Remove the locations that were not geocoded
locations <- 
  locations %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(102009)

# Perform a join to associate each entity with each document id
ner_locations <- 
  map(ner, ~{
  inner_join(.x, locations)
})

# Perform a spatial join to determine what locations fall into which neighbourhoods

ner_locations <- 
  map(seq_along(cityname), ~{
  ner_locations[[.x]] %>%
    st_as_sf() %>% 
    st_transform(102009) %>% 
    st_join(neighbourhoods[[.x]], join = st_intersects) %>% 
    filter(!is.na(neighbourhood))
})

for (n in seq_along(cityname)) {
  
  ner_locations[[n]]$doc_id <- 
    as.numeric(gsub("text", "", ner_locations[[n]]$doc_id)) }


############################################## 3 - NEIGHBOURHOOD SENTIMENT #################################################

# Determine a community resistance index per neighbourhood
# Dependent on the number of mentions per neighbhourhood and the average sentiment of the articles

for (n in seq_along(cityname)) {
  
  temp <- ner_locations[[n]] %>% 
    left_join(media[[n]] %>% dplyr::select(c("ID", "sentiment")), 
              by = c("doc_id" = "ID")) %>% 
    dplyr::select(c("doc_id", "neighbourhood", "sentiment")) %>% 
    st_drop_geometry() %>% 
    distinct()
  
  neighbourhoods[[n]] <- 
    neighbourhoods[[n]] %>% 
    left_join(left_join(aggregate(temp$sentiment, list(temp$neighbourhood), mean),
                        temp %>% 
                          group_by(neighbourhood) %>% 
                          tally(), by = c("Group.1" = "neighbourhood")), 
              by = c("neighbourhood" = "Group.1")) %>% 
    mutate(sentiment = x,
           media_count = n, 
           CRI = -1 * sentiment * media_count) %>% 
    dplyr::select(-c("x", "n"))
  
  rm(temp)
  
}



