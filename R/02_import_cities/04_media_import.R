######################################### MEDIA IMPORT ###############################

source("R/01_import_general/01_helper_functions.R")

# Import factiva and lexisnexis files

cityname <- c("montreal", "toronto")

media <- 
  
  map(cityname, ~{
    
    rbind(import_factiva(.x), 
          import_lexisnexis(.x)) %>% 
      dplyr::select(1:9) %>% 
      group_by(Author) %>% 
      distinct(Headline, .keep_all = TRUE) %>% 
      ungroup()
    
  })



# Tidy text 

  # how to save with unique names 

list <- str_tidytext(media[[1]])
media <- list [[1]]
lemmatized_articles <- list [[2]]



