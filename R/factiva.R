#### FACTIVA

library(tm.plugin.factiva)
source <- FactivaSource("txt_files/Factiva.htm")
corpus <- Corpus(source, list(language = NA)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removePunctuation))


city <- tibble(ID = numeric(0), Newspaper = character(0), Date = character(0), 
               Length = character(0), Section = character(0), Author = character(0), 
               Edition = character(0), Heading = character(0), Article = character(0))

is.na(corpus[[3]]$meta$edition)

n = 1

repeat{
  
  city[n,1] = n
  city[n,2] = corpus[[n]]$meta$origin
  city[n,3] = as.character(corpus[[n]]$meta$datetimestamp)
  city[n,4] = corpus[[n]]$meta$wordcount
  city[n,5] = corpus[[n]]$meta$section
  city[n,6] = corpus[[n]]$meta$author
  city[n,7] = corpus[[n]]$meta$edition
  city[n,8] = corpus[[n]]$meta$heading
  city[n,9] = paste(corpus[[n]]$content, collapse = "")
  
  n = n+1
  
  if (n > 42) {
    break
  }
}
# stops if there is no data - fix that
# figure out how to count how many elements are in a corpus

nfeat(corpus)

corpus[[n]]$meta$edition
