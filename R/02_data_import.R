############## DATA IMPORT ###############################

source("R/01_helper_functions.R")

## INPUT NECESSARY VARIABLES - input required
# Set up neighbourhood names
neighbourhoods_tidy<- rbind(
  data_frame(neighbourhood = "kensington market", names = c("kensington")),
  data_frame(neighbourhood = "chinatown", names = c("chinatown")),
  data_frame(neighbourhood = "beaches", names = c("the beach", "the beaches", "queen street east", "queen street e", "
                                                  queen east", "queen e", "queen st e", "queen st east")),
  data_frame(neighbourhood = "danforth", names = c("danforth", "greektown")),
  data_frame(neighbourhood = "rosedale", names = c("rosedale")),
  data_frame(neighbourhood = "little italy", names = c("little italy")),
  data_frame(neighbourhood = "queen street west", names = c("queen street west", "queen west", "queen st west", "queen w", 
                                                            "queen st w", "queen st west")),
  data_frame(neighbourhood = "leslieville", names = c("leslieville")),
  data_frame(neighbourhood = "cabbagetown", names = c("cabbagetown")),
  data_frame(neighbourhood = "lawrence park", names = c("lawrence")),
  data_frame(neighbourhood = "eglinton", names = c("eglinton")),
  data_frame(neighbourhood = "leaside", names = c("leaside")),
  data_frame(neighbourhood = "high park", names = c("high park", "bloor west", "bloor street west", "bloor st west", 
                                                    "bloor w", "bloor st w", "bloor street w")),
  data_frame(neighbourhood = "regent park", names = c("regent")))


## IMPORT NECESSARY FILES - input required
# Import txt file(s) with airbnb + city name
city1 <- lnt_read("txt_files/airbnb_toronto_1.TXT")
city2 <- lnt_read("txt_files/airbnb_toronto_2.TXT")

city1 <- city1@meta %>% 
  right_join(city1@articles, by = "ID")
city2 <- city2@meta %>% 
  right_join(city2@articles, by = "ID")

city <- rbind(city1, city2)

rm(city1, city2)

## INPUT NO LONGER REQUIRED
## CLEAN TEXT
city$Article <- str_to_lower(city$Article)
city$Article <- gsub("[[:punct:]]", " ", city$Article)


## PERFORM ANALYSIS
# Set up variables to evaluate community resistance 
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

# Generate the table
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

