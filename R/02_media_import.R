######################################### MEDIA IMPORT ###############################

source("R/01_helper_functions.R")

# must input file paths and make some adjustments depending on what files you would like to import

################################ 1 - INPUT NEIGHBOURHOOD NAMES ##############################################################################################################################
# set up neighbourhood names
neighbourhoods_tidy<- rbind(
  data_frame(neighbourhood = "plateau", names = c("plateau", "mile end")),
  data_frame(neighbourhood = "ville-marie", names = c("ville marie")),
  data_frame(neighbourhood = "old montreal", names = c("old port", "old montreal", "old montréal")),
  data_frame(neighbourhood = "hochelaga", names = c("hochelaga", "maisonneuve", "homa", "mercier")),
  data_frame(neighbourhood = "ndg", names = c("notre dame", "ndg", "cote des neiges", "côte des neiges")),
  data_frame(neighbourhood = "rosemont", names = c("little italy", "rosemont", "la petite patrie")),
  data_frame(neighbourhood = "saint henri", names = c("saint henri", "atwater market", "lachine canal")),
  data_frame(neighbourhood = "griffintown", names = c("griffintown")),
  data_frame(neighbourhood = "little burgundy", names = c("little burgundy")),
  data_frame(neighbourhood = "outremont", names = c("outremont")),
  data_frame(neighbourhood = "westmount", names = c("westmount")),
  data_frame(neighbourhood = "sud-ouest", names = c("sud ouest", "sudouest", "pointe saint charles", "pointe st charles", "charles")),
  data_frame(neighbourhood = "villeray", names = c("park ex", "mile ex", "villeray", "saint michel")),
  data_frame(neighbourhood = "lachine", names = c("lachine")))


############################# 2 - IMPORT NECESSARY FILES FROM THE NYT ############################################################################################################################
## 2.1 COMPLETE THIS SECTION IF LEXISNEXIS, OTHERWISE SKIP.
# import LexisNexis txt file(s) with airbnb + city name from the New York Times
media_LN_NYT <- lnt_read("txt_files/toronto_NYT/airbnb_toronto_NYT_LN.TXT")
media_LN_NYT <- media_LN_NYT@meta %>% 
  right_join(media_LN_NYT@articles, by = "ID") %>% 
  select(-c("Source_File", "Graphic", "ID"))

# if there is more than one file, repeat the following.
media2_LN_NYT <- lnt_read("txt_files/airbnb_toronto_2.TXT") 

# if there is more than one file, merge and remove files.
media_LN_NYT <- rbind(media1_LN_NYT@meta %>% 
                   right_join(media1_LN_NYT@articles, by = "ID"), 
                 media2_LN_NYT@meta %>% 
                   right_join(media2_LN_NYT@articles, by = "ID")) %>% 
  select(-c("Source_File", "Graphic", "ID"))
rm(media1_LN_NYT, media2_LN_NYT)

# reformat the table
media_LN_NYT <- media_LN_NYT %>% 
  mutate(Source_ID = paste("LN", 1:nrow(media_LN_NYT))) %>% 
  select(9, 1:8) %>% 
  separate(Length, c("Word_Count", NA))

media_LN_NYT$Date <- as.character(media_LN$Date)


## 2.2 COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# 2.2.1 import the source and corpus Factiva HTML files for airbnb + city name from the New York Times
source1_NYT <- FactivaSource("txt_files/toronto_NYT/airbnb_toronto_NYT_FTV.htm")
corpus_NYT <- Corpus(source1_NYT, list(language = NA)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(stripWhitespace)

# if there is more than one file, repeat the following.
source2_NYT <- FactivaSource("txt_files/Factiva.htm")
corpus2_NYT <- Corpus(source1_NYT, list(language = NA)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(stripWhitespace)

# if there is more than one file, merge
corpus_NYT = tm:::c.VCorpus(corpus1_NYT, corpus2_NYT)

# transform into a data table
media_FTV_NYT <- tibble(Source_ID = numeric(0), Newspaper = character(0), Date = character(0), 
                       Word_Count = numeric(0), Section = character(0), Author = character(0), 
                       Edition = character(0), Headline = character(0), Article = character(0))

n = 1

for (n in c(1:length(media_FTV_NYT))) {
  
  media_FTV_NYT[n,1] = paste("FTV", n)
  media_FTV_NYT[n,2] = paste(corpus_NYT[[n]]$meta$origin, collapse="")
  media_FTV_NYT[n,3] = paste(as.character(corpus_NYT[[n]]$meta$datetimestamp), collapse = "")
  media_FTV_NYT[n,4] = paste(corpus_NYT[[n]]$meta$wordcount, collapse = "")
  media_FTV_NYT[n,5] = paste(corpus_NYT[[n]]$meta$section, collapse = "")
  media_FTV_NYT[n,6] = paste(corpus_NYT[[n]]$meta$author,collapse = "") 
  media_FTV_NYT[n,7] = paste(corpus_NYT[[n]]$meta$edition,collapse = "") 
  media_FTV_NYT[n,8] = paste(corpus_NYT[[n]]$meta$heading, collapse = "")
  media_FTV_NYT[n,9] = paste(corpus_NYT[[n]]$content, collapse = "")
  
  n = n+1
}

rm(source1_NYT, source2_NYT, corpus1_NYT, corpus2_NYT, corpus_NYT)


############################# 3 - IMPORT NECESSARY FILES FROM THE LOCAL PAPER ############################################################################################################################
## 3.1 COMPLETE THIS SECTION IF LEXISNEXIS, OTHERWISE SKIP.
# import LexisNexis txt file(s) with airbnb + city name from the local newspaper
media_LN_local <- lnt_read("txt_files/montreal_local/airbnb_montreal_local_FTV.TXT")

# if there is only one file, run the following
media_LN_local <- media_LN_local@meta %>% 
 right_join(media_LN_local@articles, by = "ID") %>% 
 select(-c("Source_File", "Graphic", "ID"))

# if there is more than one file, repeat the following.
media2_LN_local <- lnt_read("txt_files/toronto_local/airbnb_toronto_local_LN_2.TXT") 

# if there is more than one file, merge and remove files.
media_LN_local <- rbind(media1_LN_local@meta %>% 
                   right_join(media1_LN_local@articles, by = "ID"), 
                 media2_LN_local@meta %>% 
                   right_join(media2_LN_local@articles, by = "ID")) %>% 
  select(-c("Source_File", "Graphic", "ID"))

rm(media1_LN_local, media2_LN_local)

# reformat table
media_LN_local <- media_LN_local %>% 
  mutate(Source_ID = paste("LN", 1:nrow(media_LN_local))) %>% 
  select(9, 1:8) %>% 
  separate(Length, c("Word_Count", NA))

media_LN_local$Date <- as.character(media_LN_local$Date)


## 3.2 COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# import the source and corpus Factiva HTML files for airbnb + city name from the local newspaper
source1_local <- FactivaSource("txt_files/toronto_local/airbnb_toronto_local_FTV_1.htm")
corpus1_local <- Corpus(source1_local, list(language = NA)) 

# if there is more than one file, repeat the following.
source8_local <- FactivaSource("txt_files/toronto_local/airbnb_toronto_local_FTV_8.htm")
corpus8_local <- Corpus(source8_local, list(language = NA))

# if there is more than one file, merge
corpus_local = tm:::c.VCorpus(corpus1_local, corpus2_local, corpus3_local, corpus4_local, corpus5_local,
                              corpus6_local, corpus7_local, corpus8_local)

# transform into a data table
media_FTV_local <- tibble(Source_ID = numeric(0), Newspaper = character(0), Date = character(0), 
                   Word_Count = numeric(0), Section = character(0), Author = character(0), 
                   Edition = character(0), Headline = character(0), Article = character(0))

n = 1

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
  
  n = n+1
}

rm(source1_local, source2_local, source3_local, source4_local, source5_local, source6_local,
   source7_local, source8_local, corpus1_local, corpus2_local, corpus3_local, corpus4_local, 
   corpus5_local, corpus6_local, corpus7_local, corpus8_local, corpus_local)


######################### 4 - MERGE AND TIDY ##############################################################################################################################
# 4.1 IF USING BOTH FACTIVA AND LEXISNEXIS, MERGE AND REMOVE DUPLICATES
media_NYT <- rbind(media_FTV_NYT, media_LN_NYT) %>% 
  mutate(ID = 1: (nrow(media_FTV_NYT) + nrow(media_LN_NYT))) %>% 
  select(10, 1:9) %>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()

media_local <- rbind(media_FTV_local, media_LN_local) %>% 
  mutate(ID = 1: (nrow(media_FTV_local) + nrow(media_LN_local))) %>% 
  select(10, 1:9) %>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()

## 4.2 OTHERWISE RENAME THE DATAFRAME AS CITY

media_NYT <- media_LN_NYT %>% 
  mutate(ID = 1: nrow(media_LN_NYT)) %>% 
  select(10, 1:9) 

media_local <- media_LN_local %>% 
  mutate(ID = 1: nrow(media_LN_local)) %>% 
  select(10, 1:9)

## 4.3 CLEAN TEXT
# make all lower space, remove accents, remove punctuation, and remove double spaces

media_NYT$Headline <- media_NYT$Headline %>% 
  str_to_lower() %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("—", " ") %>% 
  gsub("[[:punct:]]", " ", .)
media_NYT$Headline <- str_replace(gsub("\\s+", " ", str_trim(media_NYT$Headline)), "B", "b")

media_NYT$Article <- media_NYT$Article %>% 
  str_to_lower() %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("—", " ") %>% 
  gsub("[[:punct:]]", " ", .)
media_NYT$Article <- str_replace(gsub("\\s+", " ", str_trim(media_NYT$Article)), "B", "b")

media_local$Headline <- media_local$Headline %>% 
  str_to_lower() %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("—", " ") %>% 
  gsub("[[:punct:]]", " ", .)
media_local$Headline <- str_replace(gsub("\\s+", " ", str_trim(media_local$Headline)), "B", "b")

media_local$Article <- media_local$Article %>% 
  str_to_lower() %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("—", " ") %>% 
  gsub("[[:punct:]]", " ", .)
media_local$Article <- str_replace(gsub("\\s+", " ", str_trim(media_local$Article)), "B", "b")


## 4.4 REMOVE IRRELEVANT ARTICLES
# Remove articles that only mention Airbnb once. These are more often than not just referencing the 
# sharing economy in another sense. Remove duplicate articles.

airbnb <- c("airbnb", "homeshar", "home shar", "shortterm", "short term", "str ", "strs")

media_local <- media_local %>% 
  mutate(mentions = 
           str_count(media_local$Article, paste(airbnb, collapse="|")) +
           str_count(media_local$Headline, paste(airbnb, collapse="|"))) %>% 
  filter(mentions > 1) %>%
  select(-c(mentions)) %>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()

media_NYT <- media_NYT %>% 
  mutate(mentions = 
           str_count(media_NYT$Article, paste(airbnb, collapse="|")) +
           str_count(media_NYT$Headline, paste(airbnb, collapse="|"))) %>% 
  filter(mentions > 1) %>%
  select(-c(mentions))%>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()


## 4.5 EXPORT
# export the table(s) as .csv so that this does not need to be rerun.

write_csv(media_local, "txt_files/montreal_local/media_montreal_local.csv")
write_csv(media_NYT, "txt_files/montreal_NYT/media_montreal_NYT.csv")



