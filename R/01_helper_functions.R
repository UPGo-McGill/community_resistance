############# HELPER FUNCTIONS ##################

# How to use this code
# Run the helper functions
# Run media import and community resistance index (run for every city individually - export the tables)
# Run census import and social capital index (once for all cities)

# Install packages
library(tm.plugin.lexisnexis)
library(tm)
library(LexisNexisTools)
library(RCurl)
library(XML)
library(tidyverse)
library(stringr)
library(tm.plugin.factiva)
library(cancensus)
library(sf)
library(tidycensus)
library(purrr)
library(tigris)
options(tigris_use_cache = TRUE)

# Run Canadian census API key
options(cancensus.api_key = "CensusMapper_4be2cf3bf91d0cabf967f4934dbdc63b")
options(cancensus.cache_path = "~/Desktop/AirBnB Internship/AirBnB-GIT/community_resistance")

# Run US census API key
# census_api_key("dab993e99dc7faf74295fc559dc2e1764b60e6b9", install = TRUE, overwrite = TRUE)

