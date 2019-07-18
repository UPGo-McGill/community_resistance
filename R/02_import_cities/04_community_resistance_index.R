######################################### COMMUNITY RESISTANCE INDEX ########################################################

source("R/01_import_general/01_helper_functions.R")

# Enter city name and upload required geometries. Ensure that there is a field titled neighbourhood in the geometries file
cityname <- "Montreal"

neighbourhoods <-
  read_sf(dsn = "Data", layer = "montreal")%>%
  st_transform(32618) %>% 
  select(MUN_ID = MUNID, CODE_ID = CODEID, neighbourhood = NOM, geometry) 

# Perform Named Entity Recognition to determine what articles mention what locations
# Remove the city name as one of the locations as this will geo-locate to a specific point within one borough
ner_local <- rbind(
  spacy_parse(media_local$Article) %>% 
  entity_extract(type = "named", concatenator = " ") %>% 
  filter(entity_type == "GPE" |
           entity_type == "FAC" |
           entity_type == "ORG" |
           entity_type == "LOC" |
           entity_type == "PERSON") %>% 
  filter(entity != cityname) %>% 
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

ner_NYT <- rbind(
  spacy_parse(media_NYT$Article) %>% 
    entity_extract(type = "named", concatenator = " ") %>% 
    filter(entity_type == "GPE" |
             entity_type == "FAC" |
             entity_type == "ORG" |
             entity_type == "LOC" |
             entity_type == "PERSON") %>% 
    filter(entity != cityname) %>% 
    filter(nchar(entity) > 2) %>% 
    select(doc_id, entity),
  spacy_parse(media_NYT$Headline) %>% 
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

ner_NYT$entity <- ner_NYT$entity %>% 
  gsub("[[:punct:]]", " ", .)

neighbourhoods$neighbourhood <- neighbourhoods$neighbourhood %>% 
  gsub("[[:punct:]]", " ", .)

# Add the city name to the entity if it is part of any neighbourhood name
 n = 1
                                             
  repeat{
    
      ner_local$entity[n] <- 
           ifelse(any(grepl(ner_local$entity[n], neighbourhoods$neighbourhood)) == TRUE,
                  paste(ner_local$entity[n], cityname),
                  ner_local$entity[n])
                                               
      n = n + 1
                                               
  if (n > nrow(ner_local)) {
       break
      }
  }
 
 n = 1
 
 repeat{
   
   ner_NYT$entity[n] <- 
     ifelse(any(grepl(ner_NYT$entity[n], neighbourhoods$neighbourhood)) == TRUE,
            paste(ner_NYT$entity[n], cityname),
            ner_NYT$entity[n])
   
   n = n + 1
   
   if (n > nrow(ner_NYT)) {
     break
   }
 }
 
# Query Google to geocode the locations for their latitude and longitude
locations_local <- mutate_geocode(ner_local, entity)

locations_NYT <- mutate_geocode(ner_NYT, entity)

locations_local <- locations_local %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(32618)

locations_NYT <- locations_NYT %>% 
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c ("lon", "lat"), crs = 4326) %>% 
  st_transform(32618)

# Perform a spatial join to determine what locations fall into which neighbourhoods
locations_local <- locations_local %>% 
  st_join( neighbourhoods,join = st_intersects) %>% 
  filter(!is.na(neighbourhood))

locations_local$doc_id <- as.numeric(gsub("text", "",locations_local$doc_id))

locations_NYT <- locations_NYT %>% 
  st_join( neighbourhoods,join = st_intersects) %>% 
  filter(!is.na(neighbourhood))

locations_NYT$doc_id <- as.numeric(gsub("text", "",locations_NYT$doc_id))

# Set up community resistance words
# NEED TO UPDATE THIS

community_resistance_words = c("protest", "anti", "community led", "affordability", 
                               "oppose",  "resist", "opposition", "gentrification", 
                               "threat", "manifestation", "complaint", "disapprove",
                               "evict", "overtourism", "detriment", "ghost", "nuisance",
                               "consultation", "opponent",  "discrimination", "critic", 
                               "crisis", "shortage", "blame", "garbage", "noise", "complain", 
                               "concern", "coalition", "hostile", "hostility", "fairbnb", 
                               "activist", "activism", "displace", "illegal", "affordable housing",
                               "housing stock", "multiple listings", "disturbance", "damage")

# Calculate mentions as a function of total word count
# NEED TO UPDATE THIS

media_local <- media_local %>% 
  mutate(mentions = 
           (str_count(media_local$Article, paste(community_resistance_words, collapse="|")) +
              str_count(media_local$Headline, paste(community_resistance_words, collapse="|")))/as.numeric(Word_Count)) 

media_NYT <- media_NYT %>% 
  mutate(mentions = 
           (str_count(media_NYT$Article, paste(community_resistance_words, collapse="|")) +
           str_count(media_NYT$Headline, paste(community_resistance_words, collapse="|")))/as.numeric(Word_Count)) 

# Generate the community resistance table
# NEED TO UPDATE THIS (4, 5, 7, 8)
neighbourhood_resistance <- tibble(city = character(0), neighbourhood_name = character(0), mentions_local = numeric(0), 
                                   opposition_local = numeric(0), opposition_local_weighted = numeric(0),
                                   mentions_NYT = numeric(0), opposition_NYT = numeric(0), opposition_NYT_weighted = numeric(0))
n = 1

repeat{
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
    filter(mentions > 0) %>% 
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
    filter(mentions > 0) %>% 
    inner_join(locations_local %>% 
                 filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                 select("doc_id") %>% 
                 st_drop_geometry() %>% 
                 distinct(), ., by = c("doc_id" = "ID")) %>%
    summarise(mentions_avg = mean(mentions, na.rm = TRUE))
  
  
  neighbourhood_resistance[n,6] <- locations_NYT %>% 
    filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
    select("doc_id") %>% 
    st_drop_geometry() %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,7] <- 
    media_NYT %>% 
    filter(mentions > 0) %>% 
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
    filter(mentions > 0) %>% 
    inner_join(locations_NYT %>% 
                 filter(neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                 select("doc_id") %>% 
                 st_drop_geometry() %>% 
                 distinct(), ., by = c("doc_id" = "ID")) %>%
    summarise(mentions_avg = mean(mentions, na.rm = TRUE))
  
  n = n+1
  
  if (n > nrow(neighbourhoods)) {
    break
  }
}


# Calculate percent opposition and community index and community resistance index
neighbourhood_resistance <- neighbourhood_resistance %>% 
  mutate(opposition_local_pct = opposition_local/mentions_local) %>% 
  mutate(opposition_NYT_pct = opposition_NYT/mentions_NYT) %>% 
  mutate(CI = (mentions_local/
                 nrow(locations_local %>% 
                  select("doc_id") %>% 
                  st_drop_geometry() %>% 
                  distinct()) +
                 mentions_NYT/ 
                  nrow(locations_NYT %>% 
                         select("doc_id") %>% 
                         st_drop_geometry() %>% 
                          distinct()))/2) %>% 
  mutate(CRI = (opposition_local/
                  nrow(locations_local %>% 
                         select("doc_id") %>% 
                         st_drop_geometry() %>% 
                         distinct()) +
                  opposition_NYT/ 
                  nrow(locations_NYT %>% 
                         select("doc_id") %>% 
                         st_drop_geometry() %>% 
                         distinct()))/2)

# Export as a table
save(neighbourhood_resistance, file = "neighbourhood_resistance/montreal.Rdata")
