######################################### DATA IMPORT ###############################

source("R/01_helper_functions.R")

################################ INPUT NECESSARY VARIABLES ##########################################
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


############################# IMPORT NECESSARY FILES ########################################
## COMPLETE THIS SECTION IF LEXISNEXIS, OTHERWISE SKIP.
# Import LexisNexis txt file(s) with airbnb + city name
city1_LN <- lnt_read("txt_files/airbnb_toronto_1.TXT")
city2_LN <- lnt_read("txt_files/airbnb_toronto_2.TXT") 

city_LN <- rbind(city1_LN@meta %>% 
                   right_join(city1_LN@articles, by = "ID"), 
                 city2_LN@meta %>% 
                   right_join(city2_LN@articles, by = "ID")) %>% 
  select(-c("Source_File", "Graphic", "ID"))

city_LN <- city_LN %>% 
  mutate(Source_ID = paste("LN", 1:nrow(city_LN))) %>% 
  select(9, 1:8) %>% 
  separate(Length, c("Word_Count", NA))

city_LN$Date <- as.character(city_LN$Date)

rm(city1_LN, city2_LN)

# Fix dates - get fucked up when merging

## COMPLETE THIS SECTION IF FACTIVA, OTHERWISE SKIP. 
# import the source and corpus Factiva HTML files
source1 <- FactivaSource("txt_files/Factiva.htm")
corpus1 <- Corpus(source1, list(language = NA)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(stripWhitespace)
source2 <- FactivaSource("txt_files/Factiva.htm")
corpus2 <- Corpus(source1, list(language = NA)) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(content_transformer(removePunctuation)) %>% 
  tm_map(stripWhitespace)

corpus = tm:::c.VCorpus(corpus1, corpus2)

# transform into a data table
city_FTV <- tibble(Source_ID = numeric(0), Newspaper = character(0), Date = character(0), 
               Word_Count = numeric(0), Section = character(0), Author = character(0), 
               Edition = character(0), Headline = character(0), Article = character(0))

n = 1

for (n in c(1:length(corpus))) {
  
  city_FTV[n,1] = paste("FTV", n)
  city_FTV[n,2] = paste(corpus[[n]]$meta$origin, collapse="")
  city_FTV[n,3] = paste(as.character(corpus[[n]]$meta$datetimestamp), collapse = "")
  city_FTV[n,4] = paste(corpus[[n]]$meta$wordcount, collapse = "")
  city_FTV[n,5] = paste(corpus[[n]]$meta$section, collapse = "")
  city_FTV[n,6] = paste(corpus[[n]]$meta$author,collapse = "") 
  city_FTV[n,7] = paste(corpus[[n]]$meta$edition,collapse = "") 
  city_FTV[n,8] = paste(corpus[[n]]$meta$heading, collapse = "")
  city_FTV[n,9] = paste(corpus[[n]]$content, collapse = "")
  
  n = n+1
}

rm(source, corpus)

## IF USING BOTH FACTIVA AND LEXISNEXIS, MERGE AND REMOVE DUPLICATES

city <- rbind(city_FTV, city_LN) %>% 
  mutate(ID = 1: (nrow(city_FTV) + nrow(city_LN))) %>% 
  select(10, 1:9) %>% 
  group_by(Author) %>% 
  distinct(Headline, .keep_all = TRUE) %>% 
  ungroup()


## OTHERWISE RENAME THE DATAFRAME AS CITY

# city <- city_LN

############################### INPUTS NO LONGER REQUIRED ########################################
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

