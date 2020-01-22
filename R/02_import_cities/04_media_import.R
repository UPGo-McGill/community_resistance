######################################### MEDIA IMPORT ###############################

source("R/01_import_general/01_helper_functions.R")

cityname <- "montreal"

# Import factiva files

media_FTV <- import_factiva(cityname)

# Import lexisnexis files

media_LN <- import_lexisnexis(cityname)

# Merge files

media <- rbind(media_FTV, media_LN) %>% 
  dplyr::select(1:9) %>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()

# Tidy text 

list <- str_tidytext(media)
media <- list [[1]]
lemmatized_articles <- list [[2]]

