######################################### DATA ANALYSIS ###############################

source("R/01_helper_functions.R")

## COMMUNITY RESISTANCE INDEX
# Outline variables to evaluate community resistance 
neighbourhoods <- 
  neighbourhoods_tidy$neighbourhood %>% 
  unique()

neighbourhood_resistance <- tibble(neighbourhood = character(0), mentions_local = numeric(0), opposition_local = numeric(0),
                                   mentions_NYT = numeric(0), opposition_NYT = numeric(0)) 

community_resistance_words = c("protest", "anti", "community-led", "affordability", 
                               "oppose",  "resist", "resistance", "opposition", "gentrification", 
                               "threat", "threatening", "manifestation", "complaint", "disapprove",
                               "evict", "eviction", "overtourism", "detriment", "detrimental", "ghost",
                               "consultation", "opponent", "opponents", "discrimination", "critic", 
                               "critics", "crisis", "shortage", "blame")

# Perform query and sentiment analysis
n = 1
repeat{
  neighbourhood_resistance[n, 1] <- neighbourhoods[n]
  
  neighbourhood_resistance[n,2] <- city_local %>% 
    filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                            neighbourhood == neighbourhoods [n]) %>% 
                                       pull(names), collapse = "|"))) %>% 
    select("ID") %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,3] <- 
    city_local %>% 
    filter(str_detect(Article, paste(community_resistance_words, collapse="|"))) %>% 
    select("ID") %>% 
    inner_join(city_local %>% 
                 filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                                         neighbourhood == neighbourhoods [n]) %>% 
                                                    pull(names), collapse = "|"))) %>% 
                 select("ID") %>% 
                 distinct(), .) %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,4] <- city_NYT %>% 
    filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                            neighbourhood == neighbourhoods [n]) %>% 
                                       pull(names), collapse = "|"))) %>% 
    select("ID") %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,5] <- 
    city_NYT %>% 
    filter(str_detect(Article, paste(community_resistance_words, collapse="|"))) %>% 
    select("ID") %>% 
    inner_join(city_NYT %>% 
                 filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                                         neighbourhood == neighbourhoods [n]) %>% 
                                                    pull(names), collapse = "|"))) %>% 
                 select("ID") %>% 
                 distinct(), .) %>% 
    distinct() %>% 
    nrow()
  
  n = n+1
  
  if (n > length(neighbourhoods)) {
    break
  }
}

# Calculate percent opposition and community resistance index
neighbourhood_resistance <- neighbourhood_resistance %>% 
  mutate(opposition_local_pct = opposition_local/mentions_local) %>% 
  mutate(opposition_NYT_pct = opposition_NYT/mentions_NYT) %>% 
  mutate(CRI = (mentions_local + mentions_NYT*1.5 + opposition_local + opposition_NYT*1.5)*opposition_local_pct)

