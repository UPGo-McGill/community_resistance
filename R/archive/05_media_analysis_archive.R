######################################### COMMUNITY RESISTANCE INDEX ########################################################

source("R/01_import_general/01_helper_functions.R")

# GEOCODING

# Perform Named Entity Recognition to determine what articles mention what locations
# Remove the city name as one of the locations as this will geo-locate to a specific point within one borough
ner_local <- rbind(
  spacy_parse(media$Article) %>% 
    entity_extract(type = "named", concatenator = " ") %>% 
    filter(entity_type == "GPE" |
             entity_type == "FAC" |
             entity_type == "ORG" |
             entity_type == "LOC" |
             entity_type == "PERSON") %>% 
   # filter(entity != cityname) %>% 
    filter(nchar(entity) > 2) %>% 
    select(doc_id, entity),
  spacy_parse(media_local$Headline) %>% 
    entity_extract(type = "named", concatenator = " ") %>% 
    filter(entity_type == "GPE" |
             entity_type == "FAC" |
             entity_type == "ORG" |
             entity_type == "LOC" |
             entity_type == "PERSON") %>% 
    filter(entity != cityname) %>% 
    filter(nchar(entity) > 2) %>% 
    select(doc_id, entity)) %>% 
  distinct()

# Remove punctuation from the text
ner_local$entity <- ner_local$entity %>% 
  gsub("[[:punct:]]", " ", .)


# Collapse the named entity recognition to reduce processing times and number of api queries
ner_local_compressed <- ner_local %>% 
                        select(entity) %>% 
                        distinct()


# Query Google to geocode the locations for their latitude and longitude
locations <- mutate_geocode(ner_local_compressed, entity)

# Remove the locations that were not geocoded
locations_local <- locations_local %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(transform)

# Perform a join to associate each entity with each document id
locations_local <- ner_local %>% 
                   inner_join(locations_local)


# Save the locations so that this does not need to be rerun
save(locations_local, file = ".Rdata")

# Perform a spatial join to determine what locations fall into which neighbourhoods
locations_local <- locations_local %>%
  st_as_sf() %>% 
  st_transform(transform) %>% 
  st_join(neighbourhoods, join = st_intersects) %>% 
  filter(!is.na(neighbourhood))

locations_local$doc_id <- as.numeric(gsub("text", "",locations_local$doc_id))


# SENTIMENT ANALYSIS

# Set up community resistance words
community_resistance_words = c("protest", "anti", "community led", "affordability", 
                               "oppose",  "resist", "opposition", "gentrification", 
                               "threat", "manifestation", "complaint", "disapprove",
                               "evict", "overtourism", "detriment", "ghost", "nuisance",
                               "consultation", "opponent",  "discrimination", "critic", 
                               "crisis", "shortage", "blame", "garbage", "noise", "complain", 
                               "concern", "coalition", "hostile", "hostility", "fairbnb", 
                               "activist", "activism", "displace", "illegal", "affordable housing",
                               "housing stock", "multiple listings", "disturbance", "damage",
                               "protester", "riot", "accusation", "proliferation", "community", 
                               "unaffordability", "housing afford", "unaffordable", "criticize",
                               "resistance", "lawsuit", "desist", "objection", "plight", "gentrify", 
                               "gentrified", "socioeconomic", "inequality", "unfair", "competition",
                               "manifesto", "neighbour", "issue", "disappoint", "tenant", "eviction",
                               "hypertourism",  "negative", "inadequate", "disrespectful",
                               "discriminate", "racism", "bias", "illegal hotel", "criticise", 
                               "detrimental", "adverse", "enforcement",
                               "worsen", "drain", "party", "partying", "upset", "stag",
                               "fear", "safety", "problem", "toxic", "violent", "illegality", 
                               "illegaly", "violate", "housing", "disturb", "suffer", "loss")


# Calculate mentions
lemmatized_articles_local <- lemmatized_articles_local %>% 
  mutate(mentions = 
           str_count(lemmatized_articles_local$lemmas, paste(community_resistance_words, collapse="|")))

lemmatized_articles_NYT <- lemmatized_articles_NYT %>% 
  mutate(mentions = 
           str_count(lemmatized_articles_NYT$lemmas, paste(community_resistance_words, collapse="|")))

# Calculate mentions as a function of total word count
lemmatized_articles_local$doc_id <- as.numeric(lemmatized_articles_local$doc_id)

media_local <- media_local %>% 
  left_join(lemmatized_articles_local, by = c ("ID" = "doc_id")) %>% 
  select(-lemmas)

media_local <- media_local %>% 
  mutate(mentions_wc = mentions/as.numeric(Word_Count))

lemmatized_articles_NYT$doc_id <- as.numeric(lemmatized_articles_NYT$doc_id)

media_NYT <- media_NYT %>% 
  left_join(lemmatized_articles_NYT, by = c ("ID" = "doc_id")) %>% 
  select(-lemmas)

media_NYT <- media_NYT %>% 
  mutate(mentions_wc = mentions/as.numeric(Word_Count))


# Generate the community resistance table
neighbourhood_resistance <- tibble(city = character(0), neighbourhood_name = character(0), mentions_local = numeric(0), 
                                   opposition_local = numeric(0), opposition_local_weighted = numeric(0),
                                   mentions_NYT = numeric(0), opposition_NYT = numeric(0), opposition_NYT_weighted = numeric(0))

for (n in c(1:nrow(neighbourhoods))){
  
  neighbourhood_resistance[n, 1] <- cityname
  
  neighbourhood_resistance[n, 2] <- neighbourhoods$neighbourhood[n]
  
  neighbourhood_resistance[n,3] <- locations_local %>% 
    filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
    select("doc_id") %>% 
    st_drop_geometry() %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,4] <- 
    media_local %>% 
    filter(mentions_wc > 0) %>% 
    select("ID") %>% 
    inner_join(locations_local %>% 
                 filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                 select("doc_id") %>% 
                 st_drop_geometry() %>% 
                 distinct(), ., by = c("doc_id" = "ID")) %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,5] <- 
    media_local %>% 
    filter(mentions_wc > 0) %>% 
    inner_join(locations_local %>% 
                 filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                 select("doc_id") %>% 
                 st_drop_geometry() %>% 
                 distinct(), ., by = c("doc_id" = "ID")) %>%
    summarise(mentions_wc_avg = mean(mentions_wc, na.rm = TRUE))
  
  neighbourhood_resistance[n,6] <- locations_NYT %>% 
    filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
    select("doc_id") %>% 
    st_drop_geometry() %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,7] <- 
    media_NYT %>% 
    filter(mentions_wc > 0) %>% 
    select("ID") %>% 
    inner_join(locations_NYT %>% 
                 filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                 select("doc_id") %>% 
                 st_drop_geometry() %>% 
                 distinct(), ., by = c("doc_id" = "ID")) %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,8] <- 
    media_NYT %>% 
    filter(mentions_wc > 0) %>% 
    inner_join(locations_NYT %>% 
                 filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                 select("doc_id") %>% 
                 st_drop_geometry() %>% 
                 distinct(), ., by = c("doc_id" = "ID")) %>%
    summarise(mentions_wc_avg = mean(mentions_wc, na.rm = TRUE))
}

neighbourhood_resistance[is.nan(neighbourhood_resistance)] <- 0

# Calculate percent opposition and community index and community resistance index
neighbourhood_resistance <- neighbourhood_resistance %>% 
  mutate(opposition_local_pct = opposition_local/mentions_local,
         opposition_NYT_pct = opposition_NYT/mentions_NYT,
         CI = (mentions_local/
                 nrow(locations_local %>% 
                        select("doc_id") %>% 
                        st_drop_geometry() %>% 
                        distinct()) +
                 mentions_NYT/ 
                 nrow(locations_NYT %>% 
                        select("doc_id") %>% 
                        st_drop_geometry() %>% 
                        distinct()))/2,
         CRI = (opposition_local/
                  nrow(locations_local %>% 
                         select("doc_id") %>% 
                         st_drop_geometry() %>% 
                         distinct())*opposition_local_weighted +
                  opposition_NYT/ 
                  nrow(locations_NYT %>% 
                         select("doc_id") %>% 
                         st_drop_geometry() %>% 
                         distinct())*opposition_NYT_weighted)/2)

neighbourhood_resistance[is.nan(neighbourhood_resistance)] <- 0

# Add geometries to the table
neighbourhood_resistance <- neighbourhood_resistance %>% 
  left_join(neighbourhoods,  by = c("neighbourhood_name" = "neighbourhood")) %>% 
  select(1:12, "geometry")

# Export as a table
save(neighbourhood_resistance, file = ".Rdata")
