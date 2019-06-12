######################################### DATA ANALYSIS ###############################

source("R/01_helper_functions.R")

## COMMUNITY RESISTANCE INDEX
# Outline variables to evaluate community resistance 
n = 1

neighbourhoods <- 
  neighbourhoods_tidy$neighbourhood %>% 
  unique()

neighbourhood_resistance <- tibble(neighbourhoods = character(0), mentions = numeric(0), opposition = numeric(0)) 

community_resistance_words = c("protest", "anti", "community-led", "affordability", 
                               "oppose",  "resist", "resistance", "opposition", "gentrification", 
                               "threat", "threatening", "manifestation", "complaint", "disapprove",
                               "evict", "eviction", "overtourism", "detriment", "detrimental", "ghost",
                               "consultation", "opponent", "opponents", "discrimination", "critic", 
                               "critics", "crisis", "shortage", "blame")

# Perform query and sentiment analysis
repeat{
  neighbourhood_resistance[n, 1] <- neighbourhoods[n]
  
  neighbourhood_resistance[n,2] <- city %>% 
    filter(str_detect(Article, paste(filter(neighbourhoods_tidy, 
                                            neighbourhood == neighbourhoods [n]) %>% 
                                       pull(names), collapse = "|"))) %>% 
    select("ID") %>% 
    distinct() %>% 
    nrow()
  
  neighbourhood_resistance[n,3] <- 
    city %>% 
    filter(str_detect(Article, paste(community_resistance_words, collapse="|"))) %>% 
    select("ID") %>% 
    inner_join(city %>% 
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

# Calculate percentage that is opposition
neighbourhood_resistance <- neighbourhood_resistance %>% 
  mutate(opposition_pct = opposition/mentions*100)

