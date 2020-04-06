#### 05. MEDIA IMPORT ##########################################################

source("R/01_helper_functions.R")

library(future)
library(furrr)
plan(multiprocess, workers = 30)
options(future.globals.maxSize = +Inf)


### Import and preparation #####################################################

## Import lexisnexis files

media <- 
  map(cityname, ~{
    import_lexisnexis(.x) %>% 
      dplyr::select(1:9) %>% 
      group_by(Author) %>% 
      filter(as.numeric(Word_Count) > 100) %>% 
      distinct(Headline, .keep_all = TRUE) %>% 
      ungroup()
  })

# Assign a doc_id and text field for processing
media <- 
  future_map(media, ~{
    .x %>% mutate(doc_id = 1:n()) %>% rename(text = Article)
  })


## Reorder media, cityname and neighbourhoods from fewest to most media articles

media_order <- order(map_int(media, nrow))
cityname <- cityname[media_order]
media <- media[media_order]
neighbourhoods <- neighbourhoods[media_order]

# Save files for processing
save(media, neighbourhoods, cityname, media_order, 
     file = "data/lemma_prep.Rdata")


### Lemmatize articles #########################################################

model <- udpipe_download_model("english")

english <- udpipe_load_model(model)

# Tidy text
airbnb <- 
  c("airbnb", "homeshar", "home shar", "shortterm", "short term", "str ", 
    "strs", "guest", "shortstay", "short stay", "home stay", "homestay", 
    "hotel", "home share", "airbnb host", "host", "home sharing", "homeshare", 
    "homesharing", "timeshare", "letting", "shortterm rental", "longterm", 
    "rental", "legislation", "short term rental", "hotelization", 
    "legalization", "homeaway", "vrbo", "rent", "market", "tenant", "home", 
    "house", "apartment", "condo")


# Lemmatize the articles to refine results and prepare for NER
output_1 <- future_map(media[1:60], lemmatizer, .progress = TRUE)
output_2 <- future_map(media[61:90], lemmatizer, .progress = TRUE)
output_3 <- map(media[91:100], ~{
  .x %>% group_split(ceiling(doc_id / ceiling(nrow(.x) / 30)), keep = FALSE) %>% 
    future_map(lemmatizer, .progress = TRUE)})
output_4 <- map(media[101:110], ~{
  .x %>% group_split(ceiling(doc_id / ceiling(nrow(.x) / 30)), keep = FALSE) %>% 
    future_map(lemmatizer, .progress = TRUE)})
output_5 <- map(media[111:115], ~{
  .x %>% group_split(ceiling(doc_id / ceiling(nrow(.x) / 30)), keep = FALSE) %>% 
    future_map(lemmatizer, .progress = TRUE)})


## Consolidate then split up output and clean up

output_3 <- 
  output_3 %>% 
  map(~list(
    map_dfr(.x, function(x) x[[1]]),
    map_dfr(.x, function(x) x[[2]]),
    map_dfr(.x, function(x) x[[3]])
    ))

output_4 <- 
  output_4 %>% 
  map(~list(
    map_dfr(.x, function(x) x[[1]]),
    map_dfr(.x, function(x) x[[2]]),
    map_dfr(.x, function(x) x[[3]])
  ))

output_5 <- 
  output_5 %>% 
  map(~list(
    map_dfr(.x, function(x) x[[1]]),
    map_dfr(.x, function(x) x[[2]]),
    map_dfr(.x, function(x) x[[3]])
  ))


output <- c(output_1, output_2, output_3, output_4, output_5)
rm(output_1, output_2, output_3, output_4, output_5)

media <- map(output, ~.x[[1]])
lemmatized_articles <- map(output, ~.x[[2]])
lemma_intermediate <- map(output, ~.x[[3]])


## Clean up

rm(airbnb, english, model, output)

save(media, neighbourhoods, lemmatized_articles, lemma_intermediate, cityname,
     media_order, file = "data/end_of_script_5.Rdata")

