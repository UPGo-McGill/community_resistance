######################################### COMMUNITY RESISTANCE INDEX ###############################

source("R/01_import_general/01_helper_functions.R")

# Enter city name and upload required geometries. Ensure that there is a field titled neighbourhood.
cityname <- "Montreal"

neighbourhoods <-
  read_sf(dsn = "Data", layer = "montreal")%>%
  st_transform(3618) %>% 
  select(MUN_ID = MUNID, CODE_ID = CODEID, neighbourhood = NOM, geometry) 

# Initialize spaCy.
spacy_initialize()

# Perform Named Entity Recognition to determine what articles mention what locations.
# Remove the city name as one of the locations as this will geo-locate to a specific point within one borough.
ner_local <- rbind(
  spacy_parse(media_local$Article) %>% 
  entity_extract(type = "named", concatenator = " ") %>% 
  filter(entity_type == "GPE" |
           entity_type == "FAC" |
           entity_type == "ORG" |
           entity_type == "LOC" |
           entity_type == "PERSON") %>% 
  filter(entity != cityname) %>% 
  select(doc_id, entity),
  spacy_parse(media_local$Headline) %>% 
    entity_extract(type = "named", concatenator = " ") %>% 
    filter(entity_type == "GPE" |
             entity_type == "FAC" |
             entity_type == "ORG" |
             entity_type == "LOC" |
             entity_type == "PERSON") %>% 
    filter(entity != cityname) %>% 
    select(doc_id, entity))

ner_NYT <- rbind(
  spacy_parse(media_NYT$Article) %>% 
    entity_extract(type = "named", concatenator = " ") %>% 
    filter(entity_type == "GPE" |
             entity_type == "FAC" |
             entity_type == "ORG" |
             entity_type == "LOC" |
             entity_type == "PERSON") %>% 
    filter(entity != cityname) %>% 
    select(doc_id, entity),
  spacy_parse(media_NYT$Headline) %>% 
    entity_extract(type = "named", concatenator = " ") %>% 
    filter(entity_type == "GPE" |
             entity_type == "FAC" |
             entity_type == "ORG" |
             entity_type == "LOC" |
             entity_type == "PERSON") %>% 
    filter(entity != cityname) %>% 
    select(doc_id, entity))

# Query Google to geocode the locations for their latitude and longitude.
locations_local <- mutate_geocode(ner_local, entity)
locations_NYT <- mutate_geocode(ner_NYT, entity)

locations_local <- locations_local %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(32618)

locations_NYT <- locations_local %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(32618)

# Perform a spatial join to determine what locations fall into which neighbourhoods.
locations_local <- locations_local %>% 
  st_join( neighbourhoods,join = st_intersects) %>% 
  filter(!is.na(neighbourhood))

locations_NYT <- locations_NYT %>% 
  st_join( neighbourhoods,join = st_intersects) %>% 
  filter(!is.na(neighbourhood))

# Determine the community resistance index.
neighbourhood_resistance <- tibble(city = character(0), neighbourhood_name = character(0), mentions_local = numeric(0), opposition_local = numeric(0),
                                   mentions_NYT = numeric(0), opposition_NYT = numeric(0)) 

community_resistance_words = c("protest", "anti", "community led", "affordability", 
                               "oppose",  "resist", "opposition", "gentrification", 
                               "threat", "manifestation", "complaint", "disapprove",
                               "evict", "overtourism", "detriment", "ghost", "nuisance",
                               "consultation", "opponent",  "discrimination", "critic", 
                               "crisis", "shortage", "blame", "garbage", "noise", "complain", 
                               "concern", "coalition", "hostile", "hostility", "fairbnb", 
                               "activist", "activism", "displace", "illegal", "affordable housing",
                               "housing stock", "multiple listings", "disturbance", "damage")

# Perform query and sentiment analysis

n = 1

repeat{
  neighbourhood_resistance[n, 1] <- cityname
  neighbourhood_resistance[n, 2] <- neighbourhoods$neighbourhood[n]
  
  neighbourhood_resistance[n,3] <- test %>% 
    filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
    select("doc_id") %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,4] <- 
    media_local %>% 
    filter(str_detect(Article, paste(community_resistance_words, collapse="|"))) %>% 
    select("ID") %>% 
    inner_join(media_local %>% 
                 filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                                         neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                                                    pull(name), collapse = "|"))) %>% 
                 select("ID") %>% 
                 distinct(), .) %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,5] <- test %>% 
    filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
    select("doc_id") %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,6] <- 
    media_NYT %>% 
    filter(str_detect(Article, paste(community_resistance_words, collapse="|"))) %>% 
    select("ID") %>% 
    inner_join(media_NYT %>% 
                 filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                                         neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                                                    pull(name), collapse = "|"))) %>% 
                 select("ID") %>% 
                 distinct(), .) %>% 
    distinct() %>% 
    nrow()
  
  n = n+1
  
  if (n > nrow(neighbourhoods)) {
    break
  }
}


# Calculate percent opposition and community index and community resistance index
neighbourhood_resistance <- neighbourhood_resistance %>% 
  mutate(opposition_local_pct = opposition_local/mentions_local) %>% 
  mutate(opposition_NYT_pct = opposition_NYT/mentions_NYT) %>% 
  mutate(CI = (mentions_local/nrow(filter(media_local, str_detect(Article, 
                                                                  paste(neighbourhoods_tidy$names, collapse="|")))) +
                 mentions_NYT/nrow(filter(media_NYT, str_detect(Article, 
                                                                paste(neighbourhoods_tidy$names, collapse="|")))))/2) %>% 
  mutate(CRI = (opposition_local/nrow(filter(media_local, str_detect(Article, 
                                                                     paste(neighbourhoods_tidy$names, collapse="|")))) +
                  opposition_NYT/nrow(filter(media_NYT, str_detect(Article, 
                                                                   paste(neighbourhoods_tidy$names, collapse="|")))))/2)

# Export as a table
save(neighbourhood_resistance, file = "neighbourhood_resistance/montreal.Rdata")







