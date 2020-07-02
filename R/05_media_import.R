#### 05. MEDIA IMPORT ##########################################################

source("R/01_helper_functions.R")

library(future)
library(furrr)
plan(multiprocess)
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

# Tidy text using spacy

tidy_text <- 
  suppressMessages(
    future_map(media, str_tidytext)
  )


# Create media and lemmatized_articles list to allow for further text analysis
media <- 
  map(tidy_text, ~{
    rbind(.x[[1]])
  })
lemmatized_articles <- 
  map(tidy_text, ~{
    rbind(.x[[2]])
  })


# # Alternate method 
# model <- udpipe_download_model("english")
# 
# english <- udpipe_load_model(model)
# 
# # Tidy text
# airbnb <- 
#   c("airbnb", "homeshar", "home shar", "shortterm", "short term", "str ", 
#     "strs", "guest", "shortstay", "short stay", "home stay", "homestay", 
#     "hotel", "home share", "airbnb host", "host", "home sharing", "homeshare", 
#     "homesharing", "timeshare", "letting", "shortterm rental", "longterm", 
#     "rental", "legislation", "short term rental", "hotelization", 
#     "legalization", "homeaway", "vrbo", "rent", "market", "tenant", "home", 
#     "house", "apartment", "condo")
# 
# 
# # Lemmatize the articles to refine results and prepare for NER
# output <- 
#   map(media, ~{
#     .x %>% 
#       group_split(ceiling(doc_id / ceiling(nrow(.x) / 30)), keep = FALSE) %>% 
#       future_map(lemmatizer, .progress = TRUE)
#     })
# 
# 
# ## Consolidate then split up output and clean up
# 
# output <- 
#   output %>% 
#   map(~list(
#     map_dfr(.x, function(x) x[[1]]),
#     map_dfr(.x, function(x) x[[2]]),
#     map_dfr(.x, function(x) x[[3]])
#     ))
# 
# media <- map(output, ~.x[[1]])
# lemmatized_articles <- map(output, ~.x[[2]])
# lemma_intermediate <- map(output, ~.x[[3]])


# ## Clean up
# 
# rm(airbnb, english, model, output)

save(lemmatized_articles, file = "data/lemmatized_articles.Rdata")
save(cityname, media_order, file = "data/cityname.Rdata")

save(media, neighbourhoods, file = "data/end_of_script_5.Rdata")
file.remove("data/lemma_prep.Rdata")

