######################################### MEDIA IMPORT ###############################

source("R/01_import_general/01_helper_functions.R")

# NOTE: follow the instructions carefully. You must input file paths and make some adjustments 
  # depending on what files you would like to import.

############################# 1 - IMPORT NECESSARY FILES FROM THE NYT ############################################################################################################################
## 1.1 COMPLETE THIS SECTION IF LEXISNEXIS, OTHERWISE SKIP.
# Import LexisNexis txt file(s) with airbnb + city name from the New York Times
media_LN_NYT <- lnt_read("")

# If there is only one LexisNexis file, run the following.
media_LN_NYT <- media_LN_NYT@meta %>% 
  right_join(media_LN_NYT@articles, by = "ID") %>% 
  select(-c("Source_File", "Graphic", "ID"))

# If there is more than one file, repeat the following until all are imported
media2_LN_NYT <- lnt_read("") 

# If there is more than one file, merge and remove unnecessary files.
media_LN_NYT <- rbind(media_LN_NYT@meta %>% 
                        right_join(media_LN_NYT@articles, by = "ID"), 
                      media2_LN_NYT@meta %>% 
                        right_join(media2_LN_NYT@articles, by = "ID")) %>% 
  select(-c("Source_File", "Graphic", "ID"))

rm()

# Reformat the table
media_LN_NYT <- media_LN_NYT %>% 
  mutate(Source_ID = paste("LN", 1:nrow(media_LN_NYT))) %>% 
  select(9, 1:8) %>% 
  separate(Length, c("Word_Count", NA))

media_LN_NYT$Date <- as.character(media_LN_NYT$Date)


## 1.2 COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# 1.2.1 Import the source and corpus Factiva HTML files for airbnb + city name from the New York Times
source1_NYT <- FactivaSource("")
corpus1_NYT <- Corpus(source1_NYT, list(language = NA)) 

# If there is more than one Factiva file, repeat the following
source2_NYT <- FactivaSource("")
corpus2_NYT <- Corpus(source2_NYT, list(language = NA))

# If there is more than one file, merge all
corpus_NYT = tm:::c.VCorpus(corpus1_NYT, corpus2_NYT)

# Transform the corpus into a data table
media_FTV_NYT <- tibble(Source_ID = numeric(0), Newspaper = character(0), Date = character(0), 
                        Word_Count = numeric(0), Section = character(0), Author = character(0), 
                        Edition = character(0), Headline = character(0), Article = character(0))

for (n in c(1:length(corpus_NYT))) {
  
  media_FTV_NYT[n,1] = paste("FTV", n)
  media_FTV_NYT[n,2] = paste(corpus_NYT[[n]]$meta$origin, collapse="")
  media_FTV_NYT[n,3] = paste(as.character(corpus_NYT[[n]]$meta$datetimestamp), collapse = "")
  media_FTV_NYT[n,4] = paste(corpus_NYT[[n]]$meta$wordcount, collapse = "")
  media_FTV_NYT[n,5] = paste(corpus_NYT[[n]]$meta$section, collapse = "")
  media_FTV_NYT[n,6] = paste(corpus_NYT[[n]]$meta$author,collapse = "") 
  media_FTV_NYT[n,7] = paste(corpus_NYT[[n]]$meta$edition,collapse = "") 
  media_FTV_NYT[n,8] = paste(corpus_NYT[[n]]$meta$heading, collapse = "")
  media_FTV_NYT[n,9] = paste(corpus_NYT[[n]]$content, collapse = "")
  
}

# Remove unnecessary files
rm()


############################# 2 - IMPORT NECESSARY FILES FROM THE LOCAL PAPER ############################################################################################################################
## 2.1 COMPLETE THIS SECTION IF LEXISNEXIS, OTHERWISE SKIP.
# Import LexisNexis txt file(s) with airbnb + city name from the local newspaper
media1_LN_local <- lnt_read("")

# If there is only one LexisNexis file, run the following
media_LN_local <- media_LN_local@meta %>% 
  right_join(media_LN_local@articles, by = "ID") %>% 
  select(-c("Source_File", "Graphic", "ID"))

# If there is more than one file, repeat the following until all imported
media2_LN_local <- lnt_read("") 

# If there is more than one file, merge and remove unnecessary files.
media_LN_local <- rbind(media1_LN_local@meta %>% 
                          right_join(media1_LN_local@articles, by = "ID"), 
                        media2_LN_local@meta %>% 
                          right_join(media2_LN_local@articles, by = "ID")) %>% 
  select(-c("Source_File", "Graphic", "ID"))

rm()

# Reformat table
media_LN_local <- media_LN_local %>% 
  mutate(Source_ID = paste("LN", 1:nrow(media_LN_local))) %>% 
  select(9, 1:8) %>% 
  separate(Length, c("Word_Count", NA))

media_LN_local$Date <- as.character(media_LN_local$Date)


## 2.2 COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# Import the source and corpus Factiva HTML files for airbnb + city name from the local newspaper
source1_local <- FactivaSource("")
corpus1_local <- Corpus(source1_local, list(language = NA)) 

# If there is more than one Factiva file, repeat the following
source2_local <- FactivaSource("")
corpus2_local <- Corpus(source2_local, list(language = NA))

# If there is more than one file, merge
corpus_local = tm:::c.VCorpus(corpus1_local, corpus2_local)

# transform into a data table
media_FTV_local <- tibble(Source_ID = numeric(0), Newspaper = character(0), Date = character(0), 
                          Word_Count = numeric(0), Section = character(0), Author = character(0), 
                          Edition = character(0), Headline = character(0), Article = character(0))

for (n in c(1:length(corpus_local))) {
  
  media_FTV_local[n,1] = paste("FTV", n)
  media_FTV_local[n,2] = paste(corpus_local[[n]]$meta$origin, collapse="")
  media_FTV_local[n,3] = paste(as.character(corpus_local[[n]]$meta$datetimestamp), collapse = "")
  media_FTV_local[n,4] = paste(corpus_local[[n]]$meta$wordcount, collapse = "")
  media_FTV_local[n,5] = paste(corpus_local[[n]]$meta$section, collapse = "")
  media_FTV_local[n,6] = paste(corpus_local[[n]]$meta$author,collapse = "") 
  media_FTV_local[n,7] = paste(corpus_local[[n]]$meta$edition,collapse = "") 
  media_FTV_local[n,8] = paste(corpus_local[[n]]$meta$heading, collapse = "")
  media_FTV_local[n,9] = paste(corpus_local[[n]]$content, collapse = "")
  
}

# Remove original corpus and source files
rm()


######################### 3 - MERGE AND TIDY ##############################################################################################################################
# 3.1 IF USING BOTH FACTIVA AND LEXISNEXIS, MERGE AND REMOVE DUPLICATES
media_NYT <- rbind(media_FTV_NYT, media_LN_NYT) %>% 
  select(1:9) %>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()

media_local <- rbind(media_FTV_local, media_LN_local) %>% 
  select(1:9) %>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()

## 3.2 OTHERWISE RENAME THE DATAFRAME AS CITY
media_NYT <- media_LN_NYT %>% 
  select(1:9) 

media_local <- media_LN_local %>% 
  select(1:9)

## 3.3 ADD AN ID
# To allow for natural language processing
media_local <- media_local %>% 
  mutate(ID = 1:nrow(media_local))

media_NYT <- media_NYT %>% 
  mutate(ID = 1:nrow(media_NYT))


## 3.4 REMOVE IRRELEVANT ARTICLES
# Remove articles that only mention Airbnb once. These are more often than not just referencing the 
# sharing economy in another sense. 

# Initialize spaCy
spacy_initialize()

# Prepare articles for word search by removing stop words and lemmatizing
spacy_articles_local <- spacy_parse(as.character(media_local$Article),
                                    pos = FALSE, entity = FALSE, tag = FALSE)

lemmatized_articles_local <- spacy_articles_local %>%
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

spacy_articles_NYT <- spacy_parse(as.character(media_NYT$Article),
                                    pos = FALSE, entity = FALSE, tag = FALSE)

lemmatized_articles_NYT <- spacy_articles_NYT %>%
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

# Search for Airbnb mentions in the cleaned text
airbnb <- c("airbnb", "homeshar", "home shar", "shortterm", "short term", "str ", "strs", "guest",
            "shortstay", "short stay", "home stay", "homestay", "hotel", "home share", "airbnb host",
            "host", "home sharing", "homeshare", "homesharing", "timeshare", "letting", "shortterm rental",
            "longterm", "rental", "legislation", "short term rental", "hotelization", "legalization")

lemmatized_articles_local <- lemmatized_articles_local %>% 
  mutate(mentions = 
           str_count(lemmatized_articles_local$lemmas, paste(airbnb, collapse="|"))) %>% 
  filter(mentions > 1) %>%
  select(-c(mentions)) 

lemmatized_articles_NYT <- lemmatized_articles_NYT %>% 
  mutate(mentions = 
           str_count(lemmatized_articles_NYT$lemmas, paste(airbnb, collapse="|"))) %>% 
  filter(mentions > 1) %>%
  select(-c(mentions))

# Trim the original media files to include only those that mention Airbnb more than once
 media_local <- media_local %>% 
   mutate(relevant = media_local$ID %in% lemmatized_articles_local$doc_id) %>% 
   filter(relevant == TRUE) %>% 
   select(-relevant)

 media_NYT <- media_NYT %>% 
   mutate(relevant = media_NYT$ID %in% lemmatized_articles_NYT$doc_id) %>% 
   filter(relevant == TRUE) %>% 
   select(-relevant)

## 3.5 REASSIGN ID
# The following Named Entity Recognition and embedding model use an ID.
media_local <- media_local %>% 
  mutate(ID = 1:nrow(media_local))

media_NYT <- media_NYT %>% 
  mutate(ID = 1:nrow(media_NYT))

lemmatized_articles_local <- lemmatized_articles_local %>% 
  mutate(doc_id = 1:nrow(lemmatized_articles_local))

lemmatized_articles_NYT <- lemmatized_articles_NYT %>% 
  mutate(doc_id = 1:nrow(lemmatized_articles_NYT))

## 3.6 EXPORT
# export the table(s) as .csv so that this does not need to be rerun.
write_csv(media_local, "")
write_csv(media_NYT, "")



