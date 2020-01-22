######################################### MEDIA IMPORT ###############################

source("R/01_import_general/01_helper_functions.R")

cityname <- c("montreal", "toronto")


# Import factiva and lexisnexis files

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

tidy_text <- map(media, ~{
  
  str_tidytext(.x)
  
})

media <- tidy_text[[1]]

lemmatized_articles <- tidy_text[[2]]


