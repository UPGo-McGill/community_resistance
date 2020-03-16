######################################### MEDIA IMPORT ###############################

source("R/01_helper_functions.R")

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

tidy_text <- map(media, ~{
  
  str_tidytext(.x)
  
})


# Create media and lemmatized_articles list to allow for further text analysis

media <- 
  map(tidy_text, ~{
    rbind(.x[[1]])
  })

lemmatized_articles <- 
  map(tidy_text, ~{
    rbind(.x[[2]])
  })
