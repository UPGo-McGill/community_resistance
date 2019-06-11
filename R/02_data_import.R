############## DATA IMPORT ###############################
source("R/01_helper_functions.R")

# Set up neighbourhood names
neighbourhoods_tidy <- rbind(
  data_frame(neighbourhood = "williamsburg", names = c("williamsburg")),
  data_frame(neighbourhood = "park-slope", names = c("slope")),
  data_frame(neighbourhood = "bedford-stuyvesant", names = c("bedford", "stuyvesant", "bed-stuy", "bedstuy", "stuy")),
  data_frame(neighbourhood = "crown heights", names = c("crown")),
  data_frame(neighbourhood = "flatbush", names = c("flatbush")),
  data_frame(neighbourhood = "dumbo", names = c("dumbo")),
  data_frame(neighbourhood = "bushwick", names = c("bushwick")))
  
# Import txt file with airbnb + city name
city <- lnt_read("txt_files/airbnb_williamsburg.TXT") %>% 
  lnt_convert(to = "tidytext")

# Create a data table that calculates number of mentions and number of those that represent community opposition
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
                               "critics", "crisis", "shortage")

# incorporate new neighbourhood table somehow (for second and third column)

repeat{
  neighbourhood_resistance[n, 1] <- neighbourhoods[n]
  neighbourhood_resistance[n,2] <- city %>% 
    filter(Token %in% 
             (filter(neighbourhoods_tidy, neighbourhood == neighbourhoods [n]) %>% 
                         pull(names))) %>% 
    select("ID") %>% 
    distinct() %>% 
    nrow()
  neighbourhood_resistance[n,3] <- 
    city %>% 
    filter(Token %in% community_resistance_words) %>% 
    select("ID") %>% 
    inner_join(city %>% 
                 filter(Token %in% 
                          (filter(neighbourhoods_tidy, neighbourhood == neighbourhoods [n]) %>% 
                                  pull(names))) %>% 
                 select("ID") %>% 
                 distinct(), .) %>% 
    distinct() %>% 
    nrow()
  
  n = n+1
  
  if (n > length(neighbourhoods)) {
    break
  }
}

