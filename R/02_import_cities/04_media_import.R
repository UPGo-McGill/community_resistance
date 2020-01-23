######################################### MEDIA IMPORT ###############################

source("R/01_import_general/01_helper_functions.R")

# will be specified in the city_info script (along with geometries)
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

seq_along(cityname)

# Create media and lemmatized_articles list to allow for further text analysis

media <- 
  map(seq_along(cityname), ~{
    rbind(tidy_text[[.x]][[1]])
  })

lemmatized_articles <- 
  map(seq_along(cityname), ~{
    rbind(tidy_text[[.x]][[2]])
  })
