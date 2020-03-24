######################################### MEDIA IMPORT ###############################

source("R/01_helper_functions.R")
library(future)
plan(multiprocess, workers = 20)
library(furrr)

# Import lexisnexis files

media <- 
  
  map(cityname, ~{
    
    import_lexisnexis(.x) %>% 
      dplyr::select(1:9) %>% 
      group_by(Author) %>% 
      filter(as.numeric(Word_Count) > 100) %>% 
      distinct(Headline, .keep_all = TRUE) %>% 
      ungroup()
    
  })

# Tidy text
airbnb <- 
  c("airbnb", "homeshar", "home shar", "shortterm", "short term", "str ", 
    "strs", "guest", "shortstay", "short stay", "home stay", "homestay", 
    "hotel", "home share", "airbnb host", "host", "home sharing", "homeshare", 
    "homesharing", "timeshare", "letting", "shortterm rental", "longterm", 
    "rental", "legislation", "short term rental", "hotelization", 
    "legalization", "homeaway", "vrbo", "rent", "market", "tenant", "home", 
    "house", "apartment", "condo")


### USING UDPIPE ##########

model <- udpipe_download_model("english")

english <- udpipe_load_model(model)

# Assign a doc_id and text field for processing
media <- 
  future_map(media, ~{
    
    .x %>% 
      mutate(doc_id = 1:n(),
             text = Article)
    
  })


# Lemmatize the articles to refine results and prepare for 
# named entity recognition

output <- 
  future_map(media, ~{
  
    tryCatch({
      lem_initial <- 
        udpipe(.x, object = english) %>% 
        dplyr::select(c(doc_id, lemma))
      
      lem <- 
        lem_initial %>% 
        group_by(doc_id) %>% 
        filter(lemma != "-PRON-") %>%
        mutate(lemma = str_replace_all(lemma, "[^a-zA-Z0-9 ]", " ")) %>%
        filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
        mutate(lemma = strsplit(as.character(lemma), " ")) %>%
        unnest(lemma) %>%
        filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
        dplyr::summarise(lemmas = paste(as.character(lemma), collapse = " ")) %>%
        mutate(doc_id = as.numeric(paste(flatten(str_extract_all(doc_id, 
                                                                 "[[:digit:]]+"))))) %>%
        arrange(doc_id) %>%
        mutate_each(list(tolower)) %>%
        mutate(lemmas = str_squish(str_replace_all(lemmas, "[^a-zA-Z0-9 ]", " "))) %>%
        mutate(lemmas = gsub('\\b\\w{1,2}\\b','', lemmas))
      
      lem <- 
        lem %>% 
        mutate(mentions = 
                 str_count(lem$lemmas, paste(airbnb, collapse="|"))) %>% 
        filter(mentions > 2) %>%
        dplyr::select(-c(mentions)) 
      
      .x <- 
        .x %>% 
        mutate(relevant = doc_id %in% as.numeric(lem$doc_id)) %>% 
        filter(relevant == TRUE) %>% 
        dplyr::select(-relevant) %>% 
        mutate(ID = 1:n())
      
      lem <-
        lem %>%
        mutate(doc_id = 1:n())
      
      list(.x, lem, lem_initial)
    }, error = function(e)  vector("list", 3))
    
}, .progress = TRUE)


media <-
  map(output, ~{.x[[1]]})

lemmatized_articles <- 
  map(output, ~{.x[[2]]})

lemma_intermediate <- 
  map(output, ~.x[[3]])




### USING SPACY ###########
# # Parse and lemmatize text. Remove articles that mention STRs less than thrice.
# # Must be done in batches of 200 articles otherwise R aborts.
# 
# # I have been doing this in batches a few cities at a time.
#
# spacy_initialize()
# 
# for (n in 13:15) {
#   
#   lem_articles_temp = list()
#   media_temp = list()
#   
#   if(nrow(media[[n]]) < 200) {
#     
#     media_temp[[1]] <- 
#       media[[n]][1:nrow(media[[n]]),]
#     
#     media_temp[[1]] <- 
#       media_temp[[1]] %>% 
#       mutate(ID = 1:nrow(media[[n]]))
#     
#     spacy_articles <- 
#       spacy_parse(as.character(media_temp[[1]]$Article),
#                   pos = FALSE, entity = FALSE, tag = FALSE)
#     
#     spacy_articles <- 
#       spacy_articles %>% 
#       mutate(doc_id = as.numeric(str_remove(spacy_articles$doc_id, "text")))
#     
#     lem_articles_temp[[1]] <-
#       spacy_articles %>%
#       group_by(doc_id) %>%
#       filter(lemma != "-PRON-") %>%
#       mutate(lemma = str_replace_all(lemma, "[^a-zA-Z0-9 ]", " ")) %>%
#       filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
#       mutate(lemma = strsplit(as.character(lemma), " ")) %>%
#       unnest(lemma) %>%
#       filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
#       dplyr::summarise(lemmas = paste(as.character(lemma), collapse = " ")) %>%
#       mutate(doc_id = as.numeric(paste(flatten(str_extract_all(doc_id,"[[:digit:]]+"))))) %>%
#       arrange(doc_id) %>%
#       mutate_each(list(tolower)) %>%
#       mutate(lemmas = str_squish(str_replace_all(lemmas, "[^a-zA-Z0-9 ]", " "))) %>%
#       mutate(lemmas = gsub('\\b\\w{1,2}\\b','', lemmas))
#     
#     lem_articles_temp[[1]] <- 
#       lem_articles_temp[[1]] %>% 
#       mutate(mentions = 
#                str_count(lem_articles_temp[[1]]$lemmas, paste(airbnb, collapse="|"))) %>% 
#       filter(mentions > 2) %>%
#       dplyr::select(-c(mentions)) 
#     
#     media_temp[[1]] <- 
#       media_temp[[1]] %>% 
#       mutate(relevant = media_temp[[1]]$ID %in% as.numeric(lem_articles_temp[[1]]$doc_id)) %>% 
#       filter(relevant == TRUE) %>% 
#       dplyr::select(-relevant)
#     
#   } else {
#     
#     for (i in 1:floor(nrow(media[[n]])/200)) {
#       
#       media_temp[[i]] <- 
#         media[[n]][(i*200-199):(i*200),]
#       
#       media_temp[[i]] <- 
#         media_temp[[i]] %>% 
#         mutate(ID = 1:200)
#       
#       spacy_articles <- 
#         spacy_parse(as.character(media_temp[[i]]$Article),
#                     pos = FALSE, entity = FALSE, tag = FALSE)
#       
#       spacy_articles <- 
#         spacy_articles %>% 
#         mutate(doc_id = as.numeric(str_remove(spacy_articles$doc_id, "text")))
#       
#       lem_articles_temp[[i]] <-
#         spacy_articles %>%
#         group_by(doc_id) %>%
#         filter(lemma != "-PRON-") %>%
#         mutate(lemma = str_replace_all(lemma, "[^a-zA-Z0-9 ]", " ")) %>%
#         filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
#         mutate(lemma = strsplit(as.character(lemma), " ")) %>%
#         unnest(lemma) %>%
#         filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
#         dplyr::summarise(lemmas = paste(as.character(lemma), collapse = " ")) %>%
#         mutate(doc_id = as.numeric(paste(flatten(str_extract_all(doc_id,"[[:digit:]]+"))))) %>%
#         arrange(doc_id) %>%
#         mutate_each(list(tolower)) %>%
#         mutate(lemmas = str_squish(str_replace_all(lemmas, "[^a-zA-Z0-9 ]", " "))) %>%
#         mutate(lemmas = gsub('\\b\\w{1,2}\\b','', lemmas))
#       
#       lem_articles_temp[[i]] <- 
#         lem_articles_temp[[i]] %>% 
#         mutate(mentions = 
#                  str_count(lem_articles_temp[[i]]$lemmas, paste(airbnb, collapse="|"))) %>% 
#         filter(mentions > 2) %>%
#         dplyr::select(-c(mentions)) 
#       
#       media_temp[[i]] <- 
#         media_temp[[i]] %>% 
#         mutate(relevant = media_temp[[i]]$ID %in% as.numeric(lem_articles_temp[[i]]$doc_id)) %>% 
#         filter(relevant == TRUE) %>% 
#         dplyr::select(-relevant)
#       
#     }
#     
#     i = i + 1
#     
#     media_temp[[i]] <- 
#       media[[n]][(i*200-199):nrow(media[[n]]),]
#     
#     media_temp[[i]] <- 
#       media_temp[[i]] %>% 
#       mutate(ID = 1:nrow(media_temp[[i]]))
#     
#     spacy_articles <- 
#       spacy_parse(as.character(media_temp[[i]]$Article),
#                   pos = FALSE, entity = FALSE, tag = FALSE)
#     
#     spacy_articles <- 
#       spacy_articles %>% 
#       mutate(doc_id = as.numeric(str_remove(spacy_articles$doc_id, "text")))
#     
#     lem_articles_temp[[i]] <-
#       spacy_articles %>%
#       group_by(doc_id) %>%
#       filter(lemma != "-PRON-") %>%
#       mutate(lemma = str_replace_all(lemma, "[^a-zA-Z0-9 ]", " ")) %>%
#       filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
#       mutate(lemma = strsplit(as.character(lemma), " ")) %>%
#       unnest(lemma) %>%
#       filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
#       dplyr::summarise(lemmas = paste(as.character(lemma), collapse = " ")) %>%
#       mutate(doc_id = as.numeric(paste(flatten(str_extract_all(doc_id,"[[:digit:]]+"))))) %>%
#       arrange(doc_id) %>%
#       mutate_each(list(tolower)) %>%
#       mutate(lemmas = str_squish(str_replace_all(lemmas, "[^a-zA-Z0-9 ]", " "))) %>%
#       mutate(lemmas = gsub('\\b\\w{1,2}\\b','', lemmas))
#     
#     lem_articles_temp[[i]] <- 
#       lem_articles_temp[[i]] %>% 
#       mutate(mentions = 
#                str_count(lem_articles_temp[[i]]$lemmas, paste(airbnb, collapse="|"))) %>% 
#       filter(mentions > 2) %>%
#       dplyr::select(-c(mentions)) 
#     
#     media_temp[[i]] <- 
#       media_temp[[i]] %>% 
#       mutate(relevant = media_temp[[i]]$ID %in% as.numeric(lem_articles_temp[[i]]$doc_id)) %>% 
#       filter(relevant == TRUE) %>% 
#       dplyr::select(-relevant)
#     
#   }
#   
#   
# # Compile to the city level
#   
#   media_parsed[[n]] = do.call(rbind, media_temp)
#   
#   lemmatized_articles[[n]] = do.call(rbind, lem_articles_temp)
# 
# # Assign ID for further text processing
#   
#   media_parsed[[n]] <-
#     media_parsed[[n]] %>% 
#     mutate(ID = 1:nrow(media_parsed[[n]]))
#   
#   lemmatized_articles[[n]] <- 
#     lemmatized_articles[[n]] %>% 
#     mutate(doc_id = 1:nrow(lemmatized_articles[[n]]))
#   
# }
# 
# # Saving in case of crash
# 
# save(media_parsed, file = "data/media_parsed.Rdata")
# save(lemmatized_articles, file = "data/lemmatized_articles.Rdata")
# 
# # Set media_parsed as media and archive the original media file
# # media <- 
# #   media_parsed
# 
