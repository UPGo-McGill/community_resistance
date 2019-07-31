library(fastText)
library(fastrtext)
library(quanteda)
library(rword2vec)
library(spacyr)
library(tidytext)
library(tidyverse)
library(tibble)
library(tm)

data <- read.csv("media_synonyms.csv")

spacy_initialize()
spacy_articles <- spacy_parse(as.character(data$Article), 
                        pos = FALSE, entity = FALSE, tag = FALSE)

lemmatized_articles <- spacy_articles %>%
  group_by(doc_id) %>%
  filter(lemma != "-PRON-") %>%
  mutate(lemma = str_replace_all(lemma, "[^a-zA-Z0-9 ]", " ")) %>%
  filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
  mutate(lemma = strsplit(as.character(lemma), " ")) %>% 
  unnest(lemma) %>% 
  filter(!lemma %in% filter(stop_words, lexicon == "snowball")$word) %>%
  dplyr::summarise(lemmas = paste(as.character(lemma), collapse = " ")) %>%
  mutate(doc_id = as.numeric(paste(flatten(str_extract_all(doc_id,"[[:digit:]]+"))))) %>%
  arrange(doc_id) %>%
  mutate_each(list(tolower)) %>%
  mutate(lemmas = str_squish(str_replace_all(lemmas, "[^a-zA-Z0-9 ]", " "))) %>%
  mutate(lemmas = gsub('\\b\\w{1,2}\\b','', lemmas))

setdiff(1:nrow(data), lemmatized_articles$doc_id)

writeUtf8 <- function(data, filename) {
  utf8 <- enc2utf8(data)
  con <- file(filename, open = "w+", encoding = "UTF-8")
  writeLines(utf8, con = con)
  close(con)
}

writeUtf8(lemmatized_articles$lemmas, "articles.txt")

word2phrase(train_file = "articles.txt", output_file = "articles_bigram_vec.txt", min_count = 50, threshold = 15)
word2phrase(train_file = "articles_bigram_vec.txt", output_file = "articles_ngram_vec.txt", min_count = 50, threshold = 15)

readUtf8 <- function(filename) {
  con <- file(filename, open = "r+", encoding = "UTF-8")
  output <- readLines(filename)
  close(con)
  return(output)
}

articles_phrased <- str_trim(readUtf8("articles_ngram_vec.txt"))

list_params <- list(command = 'skipgram', 
                    dim = 50,
                    lr = 0.05,
                    epoch = 30,
                    #neg = 20,
                    #wordNgrams = c(1,2),
                    #minCount = 2,
                    ws = 5,
                    lrUpdateRate = 100,
                    thread = 1,
                    verbose = 2,
                    input = 'articles_ngram_vec.txt',
                    output = 'synonyms')

res <- fasttext_interface(list_params, 
                          path_output = 'synonym_logs.txt',
                          MilliSecs = 5,
                          remove_previous_file = TRUE,
                          print_process_time = TRUE)

synonym_log_plots <- plot_progress_logs(path = 'synonym_logs.txt', plot = TRUE)
synonym_model <- load_model('synonyms.bin')

struggle <- c('protest', 'anti', 'community led', 'affordability',
             'oppose',  'resist', 'opposition', 'gentrification',
             'threat', 'manifestation', 'complaint', 'disapprove',
             'evict', 'overtourism', 'detriment', 'ghost', 'nuisance',
             'consultation', 'opponent',  'discrimination', 'critic',
             'crisis', 'shortage', 'blame', 'garbage', 'noise', 'complain',
             'concern', 'coalition', 'hostile', 'hostility', 'fairbnb',
             'activist', 'activism', 'displace', 'illegal', 'affordable housing',
             'housing stock', 'multiple listings', 'disturbance', 'damage')

airbnb <- c('airbnb', 'homeshar', 'home shar', 'shortterm', 'short term', 'str ', 'strs')

get_nn_freq <- function(model, word_freqs, query, top_n) {
  data <- get_nn(model, query, top_n)
  nn_freq <- tibble(word = names(data),
                    cosine = data) %>%
    left_join(word_freqs, by = "word") %>%
    arrange(desc(cosine))
}

get_word_freq <- function(titles) {
  words <- titles %>%
    VectorSource() %>% 
    VCorpus() %>%
    TermDocumentMatrix() %>%
    as.matrix() %>%
    rowSums() %>%
    sort(decreasing = TRUE)
  
  word_freqs <- tibble(word = names(words), 
                       freq = words)
  return(word_freqs)
}

synonym_word_freqs <- get_word_freq(articles_phrased)

find_synonyms <- function(synonym_model, word_freqs, queries, n) {
  synonyms_df <- tibble(word = character(),
                        synonym = character(),
                        cosine = numeric(),
                        freq = numeric())
  
  
  counter <- 1
  for (q in queries) {
    synonyms_nn_freq <- get_nn_freq(synonym_model, word_freqs, q, n)
    synonyms_nn_freq$query <- q
    synonyms_nn_freq <- synonyms_nn_freq %>%
      select(word = query, synonym = word, cosine, freq)
    synonyms_df <- rbind(synonyms_df, synonyms_nn_freq)
    counter = counter + 1
    if (counter %% 10 == 0) {
      cat(paste0("Processed ", as.character(counter), " key words.\n"))
    }
  }
  return(synonyms_df)
}

airbnb_synonyms <- find_synonyms(synonym_model, synonym_word_freqs, airbnb, 5)
struggle_synonyms <- find_synonyms(synonym_model, synonym_word_freqs, struggle, 5)

airbnb_synonyms <- airbnb_synonyms %>% 
  mutate(synonym = str_replace_all(synonym, "_", " "))

struggle_synonyms <- struggle_synonyms %>% 
  mutate(synonym = str_replace_all(synonym, "_", " "))

write.csv(airbnb_synonyms, "airbnb_synonyms.csv", row.names = FALSE)
write.csv(struggle_synonyms, "struggle_synonyms.csv", row.names = FALSE)
