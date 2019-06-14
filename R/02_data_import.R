######################################### DATA IMPORT ###############################

source("R/01_helper_functions.R")

# must input file paths and make some adjustments depending on what files you would like to import

################################ 1 - INPUT NEIGHBOURHOOD NAMES ##############################################################################################################################
# set up neighbourhood names
neighbourhoods_tidy<- rbind(
  data_frame(neighbourhood = "kensington market", names = c("kensington")),
  data_frame(neighbourhood = "chinatown", names = c("chinatown")),
  data_frame(neighbourhood = "beaches", names = c("the beach", "woodbine", "queen street e",
                                                "queen e", "queen st e")),
  data_frame(neighbourhood = "danforth", names = c("danforth", "greektown")),
  data_frame(neighbourhood = "rosedale", names = c("rosedale")),
  data_frame(neighbourhood = "little italy", names = c("little italy")),
  data_frame(neighbourhood = "queen street west", names = c("queen street w", "queen w", "queen st w")),
  data_frame(neighbourhood = "leslieville", names = c("leslieville")),
  data_frame(neighbourhood = "cabbagetown", names = c("cabbagetown")),
  data_frame(neighbourhood = "lawrence park", names = c("lawrence")),
  data_frame(neighbourhood = "eglinton", names = c("eglinton")),
  data_frame(neighbourhood = "leaside", names = c("leaside")),
  data_frame(neighbourhood = "high park", names = c("high park", "bloor w", "bloor street w", "bloor st w")),
  data_frame(neighbourhood = "regent park", names = c("regent")))


############################# 2 - IMPORT NECESSARY FILES FROM THE NYT ############################################################################################################################
## 2.1 COMPLETE THIS SECTION IF LEXISNEXIS, OTHERWISE SKIP.
# import LexisNexis txt file(s) with airbnb + city name from the New York Times
media_LN_NYT <- lnt_read("txt_files/toronto_NYT/airbnb_toronto_NYT_LN.TXT")
media_LN_NYT <- media_LN_NYT@meta %>% 
  right_join(media_LN_NYT@articles, by = "ID") %>% 
  select(-c("Source_File", "Graphic", "ID"))

# if there is more than one file, repeat the following.
#media2_LN_NYT <- lnt_read("txt_files/airbnb_toronto_2.TXT") 

# if there is more than one file, merge.
#media_LN_NYT <- rbind(media1_LN_NYT@meta %>% 
#                   right_join(media1_LN_NYT@articles, by = "ID"), 
#                 media2_LN_NYT@meta %>% 
#                   right_join(media2_LN_NYT@articles, by = "ID")) %>% 
#  select(-c("Source_File", "Graphic", "ID"))

media_LN_NYT <- media_LN_NYT %>% 
  mutate(Source_ID = paste("LN", 1:nrow(media_LN_NYT))) %>% 
  select(9, 1:8) %>% 
  separate(Length, c("Word_Count", NA))

media_LN_NYT$Date <- as.character(media_LN$Date)

# rm(media1_LN_NYT, media2_LN_NYT)

## 2.2 COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# 2.2.1 import the source and corpus Factiva HTML files for airbnb + city name from the New York Times
source1_NYT <- FactivaSource("txt_files/toronto_NYT/airbnb_toronto_NYT_FTV.htm")
corpus_NYT <- Corpus(source1_NYT, list(language = NA)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(stripWhitespace)

# if there is more than one file, repeat the following.
#source2_NYT <- FactivaSource("txt_files/Factiva.htm")
#corpus2_NYT <- Corpus(source1_NYT, list(language = NA)) %>% 
#  tm_map(content_transformer(tolower)) %>% 
#  tm_map(content_transformer(removePunctuation)) %>% 
#  tm_map(stripWhitespace)

# if there is more than one file, merge
#corpus_NYT = tm:::c.VCorpus(corpus1_NYT, corpus2_NYT)

# transform into a data table
media_FTV_NYT <- tibble(Source_ID = numeric(0), Newspaper = character(0), Date = character(0), 
                       Word_Count = numeric(0), Section = character(0), Author = character(0), 
                       Edition = character(0), Headline = character(0), Article = character(0))

n = 1

for (n in c(1:length(media_NYT))) {
  
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
media1_LN_local <- lnt_read("txt_files/toronto_local/airbnb_toronto_local_LN_1.TXT")

# if there is only one file, run the following
media1_LN_local <- media1_LN_local@meta %>% 
 right_join(media1_LN_local@articles, by = "ID") %>% 
 select(-c("Source_File", "Graphic", "ID"))

# if there is more than one file, repeat the following.
media2_LN_local <- lnt_read("txt_files/toronto_local/airbnb_toronto_local_LN_2.TXT") 

# if there is more than one file, merge.
media_LN_local <- rbind(media1_LN_local@meta %>% 
                   right_join(media1_LN_local@articles, by = "ID"), 
                 media2_LN_local@meta %>% 
                   right_join(media2_LN_local@articles, by = "ID")) %>% 
  select(-c("Source_File", "Graphic", "ID"))

media_LN_local <- media_LN_local %>% 
  mutate(Source_ID = paste("LN", 1:nrow(city_LN_local))) %>% 
  select(9, 1:8) %>% 
  separate(Length, c("Word_Count", NA))

media_LN_local$Date <- as.character(media_LN_local$Date)

rm(media1_LN_local, media2_LN_local)

## 3.2 COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# import the source and corpus Factiva HTML files for airbnb + city name from the local newspaper
source1_local <- FactivaSource("txt_files/toronto_local/airbnb_toronto_local_FTV_1.htm")
corpus1_local <- Corpus(source1_local, list(language = NA)) 

# if there is more than one file, repeat the following.
source2_local <- FactivaSource("txt_files/toronto_local/airbnb_toronto_local_FTV_2.htm")
corpus2_local <- Corpus(source2_local, list(language = NA))

# if there is more than one file, merge
corpus_local = tm:::c.VCorpus(corpus1_local, corpus2_local, corpus3_local, corpus4_local, corpus5_local,
                              corpus6_local, corpus7_local)

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

# media_NYT <- media_LN_NYT
# media_local <- media_LN_local

## 4.3 CLEAN TEXT
# make all lower space, remove punctuation, and remove double spaces
media_NYT$Article <- str_to_lower(media_NYT$Article)
media_NYT$Article <- str_replace(media_NYT$Article, "—", " ")
media_NYT$Article <- gsub("[[:punct:]]", " ", media_NYT$Article)
media_NYT$Article <- str_replace(gsub("\\s+", " ", str_trim(media_NYT$Article)), "B", "b")

media_local$Article <- str_to_lower(media_local$Article)
media_local$Article <- str_replace(media_local$Article, "—", " ")
media_local$Article <- gsub("[[:punct:]]", " ", media_local$Article)
media_local$Article <- str_replace(gsub("\\s+", " ", str_trim(media_local$Article)), "B", "b")



