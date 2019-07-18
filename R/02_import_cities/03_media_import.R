######################################### MEDIA IMPORT ###############################

source("R/01_import_general/01_helper_functions.R")

# must input file paths and make some adjustments depending on what files you would like to import
# when complete, export table and run community resistance index
# repeat for all cities

############################# 1 - IMPORT NECESSARY FILES FROM THE NYT ############################################################################################################################
## 1.1 COMPLETE THIS SECTION IF LEXISNEXIS, OTHERWISE SKIP.
# import LexisNexis txt file(s) with airbnb + city name from the New York Times
media_LN_NYT <- lnt_read("txt_files/montreal/montreal_NYT_LN.TXT")

# if there is only one file, run the following.
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

media_LN_NYT$Date <- as.character(media_LN_NYT$Date)


## 1.2 COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# 1.2.1 import the source and corpus Factiva HTML files for airbnb + city name from the New York Times
source1_NYT <- FactivaSource("txt_files/montreal/montreal_NYT_FTV.htm")
corpus_NYT <- Corpus(source1_NYT, list(language = NA)) 

# if there is more than one file, repeat the following.
source2_NYT <- FactivaSource("txt_files/Factiva.htm")
corpus2_NYT <- Corpus(source1_NYT, list(language = NA))

# if there is more than one file, merge
corpus_NYT = tm:::c.VCorpus(corpus1_NYT, corpus2_NYT)

# transform into a data table
media_FTV_NYT <- tibble(Source_ID = numeric(0), Newspaper = character(0), Date = character(0), 
                        Word_Count = numeric(0), Section = character(0), Author = character(0), 
                        Edition = character(0), Headline = character(0), Article = character(0))

n = 1

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
  
  n = n+1
}

rm(source1_NYT, source2_NYT, corpus1_NYT, corpus2_NYT, corpus_NYT)


############################# 2 - IMPORT NECESSARY FILES FROM THE LOCAL PAPER ############################################################################################################################
## 2.1 COMPLETE THIS SECTION IF LEXISNEXIS, OTHERWISE SKIP.
# import LexisNexis txt file(s) with airbnb + city name from the local newspaper
media_LN_local <- lnt_read("txt_files/montreal/montreal_local_LN.TXT")

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


## 2.2 COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# import the source and corpus Factiva HTML files for airbnb + city name from the local newspaper
source1_local <- FactivaSource("txt_files/montreal/montreal_local_FTV_1.htm")
corpus1_local <- Corpus(source1_local, list(language = NA)) 

# if there is more than one file, repeat the following.
source3_local <- FactivaSource("txt_files/montreal/montreal_local_FTV_3.htm")
corpus3_local <- Corpus(source3_local, list(language = NA))

# if there is more than one file, merge
corpus_local = tm:::c.VCorpus(corpus1_local, corpus2_local, corpus3_local)

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

## 3.3 REMOVE IRRELEVANT ARTICLES
# Remove articles that only mention Airbnb once. These are more often than not just referencing the 
# sharing economy in another sense. 

# clean text temporarily to allow for easy search
media_NYT_temp <- media_NYT
media_local_temp <- media_local

media_NYT_temp$Headline <- media_NYT_temp$Headline %>% 
  str_to_lower() %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("—", " ") %>% 
  gsub("[[:punct:]]", " ", .)
media_NYT_temp$Headline <- str_replace(gsub("\\s+", " ", str_trim(media_NYT_temp$Headline)), "B", "b")

media_NYT_temp$Article <- media_NYT_temp$Article %>% 
  str_to_lower() %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("—", " ") %>% 
  gsub("[[:punct:]]", " ", .)
media_NYT_temp$Article <- str_replace(gsub("\\s+", " ", str_trim(media_NYT_temp$Article)), "B", "b")

media_local_temp$Headline <- media_local_temp$Headline %>% 
  str_to_lower() %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("—", " ") %>% 
  gsub("[[:punct:]]", " ", .)
media_local_temp$Headline <- str_replace(gsub("\\s+", " ", str_trim(media_local_temp$Headline)), "B", "b")

media_local_temp$Article <- media_local_temp$Article %>% 
  str_to_lower() %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("—", " ") %>% 
  gsub("[[:punct:]]", " ", .)
media_local_temp$Article <- str_replace(gsub("\\s+", " ", str_trim(media_local_temp$Article)), "B", "b")


# search for Airbnb mentions in the cleaned text
airbnb <- c("airbnb", "homeshar", "home shar", "shortterm", "short term", "str ", "strs", "guest",
            "shortstay", "short stay", "home stay", "homestay", "hotel")

media_local_temp <- media_local_temp %>% 
  mutate(mentions = 
           str_count(media_local_temp$Article, paste(airbnb, collapse="|")) +
           str_count(media_local_temp$Headline, paste(airbnb, collapse="|"))) %>% 
  filter(mentions > 1) %>%
  select(-c(mentions)) %>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()

media_NYT_temp <- media_NYT_temp %>% 
  mutate(mentions = 
           str_count(media_NYT$Article, paste(airbnb, collapse="|")) +
           str_count(media_NYT$Headline, paste(airbnb, collapse="|"))) %>% 
  filter(mentions > 1) %>%
  select(-c(mentions))%>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()

# trim the original media files to include only those that mention Airbnb more than once

 media_local <- media_local %>% 
   mutate(relevant = media_local$Source_ID %in% media_local_temp$Source_ID) %>% 
   filter(relevant == TRUE) %>% 
   select(-relevant)

 media_NYT <- media_NYT %>% 
   mutate(relevant = media_NYT$Source_ID %in% media_NYT_temp$Source_ID) %>% 
   filter(relevant == TRUE) %>% 
   select(-relevant)
 
rm(airbnb, media_local_temp, media_NYT_temp)


## 3.4 ADD AN ID
# The following Named Entity Recognition and embedding model use an ID.

media_local <- media_local %>% 
  mutate(ID = 1:nrow(media_local))

media_NYT <- media_NYT %>% 
  mutate(ID = 1:nrow(media_NYT))

## 3.5 EXPORT
# export the table(s) as .csv so that this does not need to be rerun.

write_csv(media_local, "txt_files/montreal/media_montreal_local.csv")
write_csv(media_NYT, "txt_files/montreal/media_montreal_NYT.csv")



