######################################### COMMUNITY RESISTANCE INDEX ###############################

source("R/01_import_and_compile_general/01_helper_functions.R")

# Outline variables to evaluate community resistance 
neighbourhood_resistance <- tibble(city = character(0), neighbourhood = character(0), mentions_local = numeric(0), opposition_local = numeric(0),
                                   mentions_NYT = numeric(0), opposition_NYT = numeric(0)) 

community_resistance_words = c("protest", "anti", "community led", "affordability", 
                               "oppose",  "resist", "opposition", "gentrification", 
                               "threat", "manifestation", "complaint", "disapprove",
                               "evict", "overtourism", "detriment", "ghost", "nuisance",
                               "consultation", "opponent",  "discrimination", "critic", 
                               "crisis", "shortage", "blame", "garbage", "noise", "complain", 
                               "concern", "coalition", "hostile", "hostility", "fairbnb", 
                               "activist", "activism", "displace", "illegal", "affordable housing",
                               "housing stock", "multiple listings", "disturbance", "damage")

# Perform query and sentiment analysis
n = 1
repeat{
  neighbourhood_resistance[n, 1] <- cityname
  neighbourhood_resistance[n, 2] <- neighbourhoods$neighbourhood[n]
  
  neighbourhood_resistance[n,3] <- media_local %>% 
    filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                            neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                                       pull(name), collapse = "|"))) %>% 
    select("ID") %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,4] <- 
    media_local %>% 
    filter(str_detect(Article, paste(community_resistance_words, collapse="|"))) %>% 
    select("ID") %>% 
    inner_join(media_local %>% 
                 filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                                         neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                                                    pull(name), collapse = "|"))) %>% 
                 select("ID") %>% 
                 distinct(), .) %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,5] <- media_NYT %>% 
    filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                            neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                                       pull(name), collapse = "|"))) %>% 
    select("ID") %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,6] <- 
    media_NYT %>% 
    filter(str_detect(Article, paste(community_resistance_words, collapse="|"))) %>% 
    select("ID") %>% 
    inner_join(media_NYT %>% 
                 filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                                         neighbourhood == neighbourhoods$neighbourhood[n]) %>% 
                                                    pull(name), collapse = "|"))) %>% 
                 select("ID") %>% 
                 distinct(), .) %>% 
    distinct() %>% 
    nrow()
  
  n = n+1
  
  if (n > nrow(neighbourhoods)) {
    break
  }
}


# Calculate percent opposition and community index and community resistance index
neighbourhood_resistance <- neighbourhood_resistance %>% 
  mutate(opposition_local_pct = opposition_local/mentions_local) %>% 
  mutate(opposition_NYT_pct = opposition_NYT/mentions_NYT) %>% 
  mutate(CI = (mentions_local/nrow(filter(media_local, str_detect(Article, 
                                                                  paste(neighbourhoods_tidy$names, collapse="|")))) +
                  mentions_NYT/nrow(filter(media_NYT, str_detect(Article, 
                                                                 paste(neighbourhoods_tidy$names, collapse="|")))))/2) %>% 
  mutate(CRI = (opposition_local/nrow(filter(media_local, str_detect(Article, 
                                                                     paste(neighbourhoods_tidy$names, collapse="|")))) +
                opposition_NYT/nrow(filter(media_NYT, str_detect(Article, 
                                                                 paste(neighbourhoods_tidy$names, collapse="|")))))/2)

# Export as a table
save(neighbourhood_resistance, file = "neighbourhood_resistance_montreal.Rdata")


