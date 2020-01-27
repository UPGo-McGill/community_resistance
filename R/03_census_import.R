######################################### CENSUS IMPORT  ###############################

source("R/01_helper_functions.R")

# Census import for all census tracts in Canada and the census tracts in the US in the following states
states <- unique(fips_codes$state)[1:51] 

########################################### 1 - CANADA #######################################

# 1.1 LIST CENSUS VARIABLES
variables_canada <- list_census_vectors("CA16")

# 1.2 IMPORT CENSUS VARIABLES FOR COUNTRY-WIDE AVERAGE
canada <-
  get_census(
    dataset = "CA16", regions = list(C = "Canada"),
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_524", 
                "v_CA16_3393", "v_CA16_3996", "v_CA16_2540", "v_CA16_497", "v_CA16_484"),
    geo_format = "sf") %>% 
  st_transform(102009) %>% 
  dplyr::select(GeoUID, Population, Households, contains("v_CA"))

names(canada) <- 
  c("Geo_UID", "population", "households", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupied", 
    "rental", "no_language", "citizen", "white", "low_income_pct_pop", "lone_parent", 
    "families", "geometry")

canada <- canada %>% 
  mutate(language = population - no_language)

canada <- canada %>% 
  mutate(
    lone_parent_pct_families = lone_parent / families,
    low_income = low_income_pct_pop/100*population,
    low_income_pct_pop = low_income_pct_pop/100,
    language = population - no_language) %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households}))


# 1.3 IMPORT CENSUS VARIABLES FOR ALL CMAs
CMAs_canada <-
  get_census(
    dataset = "CA16", regions = list(C = "Canada"), level = "CMA",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_524", 
                "v_CA16_3393", "v_CA16_3996", "v_CA16_2540", "v_CA16_497", "v_CA16_484"),
    geo_format = "sf") %>% 
  st_transform(102009) %>% 
  filter(Type == "CMA") %>% 
  dplyr::select(GeoUID, CMA_name = name, Population, Households, contains("v_CA")) 

CMAs_canada$CMA_name <- CMAs_canada$CMA_name %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("'", "") %>% 
  str_replace("`", "")

names(CMAs_canada) <- 
  c("CMA_UID", "CMA_name", "population", "households", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupied", 
    "rental", "no_language", "citizen", "white", "low_income_pct_pop", "lone_parent",
    "families", "geometry")

CMAs_canada <- CMAs_canada%>% 
  separate(CMA_name, into = c("CMA_name", NA), sep = "[(]") %>% 
  mutate(
    lone_parent_pct_families = lone_parent / families,
    low_income = low_income_pct_pop/100*population,
    low_income_pct_pop = low_income_pct_pop/100,
    language = population - no_language) %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households}))

CMAs_canada$CMA_name <- substr(CMAs_canada$CMA_name, 1, nchar(CMAs_canada$CMA_name) - 1)

# 1.4 IMPORT CENSUS VARIABLES FOR ALL CENSUS TRACTS
CTs_canada <-
  get_census(
    dataset = "CA16", regions = list(C = "Canada"), level = "CT",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_524", 
                "v_CA16_3393", "v_CA16_3996", "v_CA16_2540", "v_CA16_497", "v_CA16_484"),
    geo_format = "sf") %>% 
  st_transform(102009) %>% 
  filter(Type == "CT") %>% 
  dplyr::select(GeoUID, PR_UID, CMA_UID, Population, Households, contains("v_CA"))

CTs_canada <- CTs_canada %>% 
  inner_join(st_drop_geometry(dplyr::select(CMAs_canada, c(1,2))), by = "CMA_UID") %>% 
 dplyr::select(GeoUID, CMA_UID, CMA_name, everything()) 

names(CTs_canada) <- 
  c("Geo_UID", "CMA_UID", "CMA_name", "PR_UID", "population", "households", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupied", 
    "rental", "no_language", "citizen", "white", "low_income_pct_pop",
    "lone_parent", "families", "geometry")

CTs_canada <- CTs_canada%>% 
  mutate(
    lone_parent_pct_families = lone_parent / families,
    low_income = low_income_pct_pop/100*population,
    low_income_pct_pop = low_income_pct_pop/100,
    language = population - no_language) %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households})) %>% 
  dplyr::select(-c("no_language"))


# 1.5 Z SCORES
datalist = list()

for (n in c(1:nrow(CMAs_canada))) {
  
  CTs_canada_temp <- CTs_canada %>% 
    filter(CMA_UID == as.numeric(st_drop_geometry(CMAs_canada[n, 1]))) %>% 
    mutate_at(
      .vars = c("population", "households", "med_income",
                "university_education", "housing_need", "non_mover", "owner_occupied", 
                "rental", "language", "citizen", "white", "low_income", "lone_parent", 
                "university_education_pct_pop", "housing_need_pct_household", "non_mover_pct_pop", 
                "owner_occupied_pct_household", "rental_pct_household", "language_pct_pop", 
                "citizen_pct_pop", "white_pct_pop", "low_income_pct_pop", "lone_parent_pct_families"),
      .funs = list(`z` = ~{(.- mean(., na.rm = TRUE))/sd(., na.rm = TRUE)})) 
  
  datalist[[n]] <- CTs_canada_temp
  
}

CTs_canada = do.call(rbind, datalist)

rm(CTs_canada_temp, datalist)


########################################### 2 - UNITED STATES #####################################

# 2.1 LIST CENSUS VARIBALES
variables_us <- load_variables(2017, "acs5", cache = TRUE) 

# 2.2 IMPORT CENSUS VARIABLES FOR COUNTRY-WIDE AVERAGE
us <- rbind(get_acs(geography = "us", variables = c("B01003_001", 
                                              "B19013_001",
                                              "B15003_022",
                                              "B25070_007",
                                              "B25070_008",
                                              "B25070_009",
                                              "B25070_010",
                                              "B25091_008",
                                              "B25091_009",
                                              "B25091_010",
                                              "B25091_011",
                                              "B07001_017", 
                                              "B25012_002", 
                                              "B25011_026",
                                              "B06007_002", 
                                              "B06007_004",
                                              "B06007_007",
                                              "B05001_006", 
                                              "B01001H_001",
                                              "B11001_005", 
                                              "B11001_006",
                                              "B11001_002")), 
            get_acs(geography = "us", variables = c("C17002_001", "C17002_008")))

us <- us %>% 
  dplyr::select(-c("moe")) %>% 
  spread(variable, estimate)

# tidy the variables
us <- us %>% 
  mutate(language = B06007_002 + B06007_004 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011,
         households = B25012_002 + B25011_026,
         low_income_pct_pop = 1 - C17002_008/C17002_001, 
         lone_parent = B11001_005 + B11001_006) %>% 
  dplyr::select(-c("B06007_002", "B06007_004", "B06007_007", "B25070_007", "B25070_008", 
            "B25070_009", "B25070_010", "B25091_008", "B25091_009", "B25091_010",
            "B25091_011", "C17002_008", "C17002_001", "B11001_005", "B11001_006"))

names(us) <- 
  c("Geo_UID", "Country_name", "white", "population", "non_citizen", "non_mover",
    "families", "university_education", "med_income", "rental", "owner_occupied",
    "language",  "housing_need", "households", "low_income_pct_pop", "lone_parent")

us <- us %>% 
  mutate(citizen = population - non_citizen) %>% 
  dplyr::select(c("Geo_UID", "Country_name", "population", "households", "med_income",
           "university_education", "housing_need", "non_mover", "owner_occupied", 
           "rental", "language", "citizen", "white", "low_income_pct_pop", 
           "lone_parent", "families"))

us <- us %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households})) %>% 
  mutate(lone_parent_pct_families = lone_parent/families,
         low_income = low_income_pct_pop * population)

# 2.3 IMPORT CENSUS VARIABLES FOR ALL CMAs
MSAs_us <- rbind(get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                                            variables = c("B01003_001", 
                                                         "B19013_001",
                                                         "B15003_022",
                                                         "B25070_007",
                                                         "B25070_008",
                                                         "B25070_009",
                                                         "B25070_010",
                                                         "B25091_008",
                                                         "B25091_009",
                                                         "B25091_010",
                                                         "B25091_011",
                                                         "B07001_017", 
                                                         "B25012_002", 
                                                         "B25011_026",
                                                         "B06007_002", 
                                                         "B06007_004",
                                                         "B06007_007",
                                                         "B05001_006", 
                                                         "B01001H_001",
                                                         "B11001_005", 
                                                         "B11001_006",
                                                         "B11001_002")), 
                 get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                         variables = c("C17002_001", "C17002_008")))


MSAs_us <- MSAs_us %>% 
  dplyr::select(-c("moe")) %>% 
  spread(variable, estimate) %>% 
  separate(NAME, into = c("City_name", "Rest"), sep = ",") %>% 
  separate(Rest, into = c(NA, "State", "Type", NA), sep = " ") %>% 
  filter(str_detect(State, paste(states, collapse="|")))


MSAs_us <- MSAs_us %>% 
  mutate(language = B06007_002 + B06007_004 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011,
         households = B25012_002 + B25011_026, 
         low_income_pct_pop = 1 - C17002_008/C17002_001, 
         lone_parent = B11001_005 + B11001_006) %>% 
  dplyr::select(-c("B06007_002", "B06007_004", "B06007_007", "B25070_007", "B25070_008", 
                   "B25070_009", "B25070_010", "B25091_008", "B25091_009", "B25091_010",
                   "B25091_011", "C17002_008", "C17002_001", "B11001_005", "B11001_006"))

names(MSAs_us) <- 
  c("CMA_UID", "CMA_name", "State", "Type", "white", "population", "non_citizen", "non_mover",
    "families", "university_education", "med_income", "rental", "owner_occupied",
    "language",  "housing_need", "households", "low_income_pct_pop", "lone_parent")

MSAs_us <- MSAs_us %>% 
  mutate(citizen = population - non_citizen) %>% 
  dplyr::select(c("CMA_UID", "CMA_name", "State", "population", "households", "med_income",
           "university_education", "housing_need", "non_mover", "owner_occupied", 
           "rental", "language", "citizen", "white", "low_income_pct_pop", "lone_parent",
           "families"))

MSAs_us <- MSAs_us%>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households})) %>% 
  mutate(lone_parent_pct_families = lone_parent/families,
         low_income = low_income_pct_pop *population) 

MSAs_us <- core_based_statistical_areas(cb = TRUE, class = "sf", refresh = TRUE) %>% 
  st_transform(102009) %>% 
  dplyr::select(c("GEOID", "geometry")) %>% 
  left_join(MSAs_us, ., by = c("CMA_UID" = "GEOID")) %>% 
  st_as_sf(sf_column_name = "geometry")

# 2.4 IMPORT CENSUS VARIABLES FOR ALL CENSUS TRACTS (include geometries)
CTs_us <- rbind(
  reduce(
  map(states, function(x) {
    get_acs(geography = "tract", variables = c("B01003_001", 
                                               "B19013_001",
                                               "B15003_022",
                                               "B25070_007",
                                               "B25070_008",
                                               "B25070_009",
                                               "B25070_010",
                                               "B25091_008",
                                               "B25091_009",
                                               "B25091_010",
                                               "B25091_011",
                                               "B07001_017", 
                                               "B25012_002", 
                                               "B25011_026",
                                               "B06007_002", 
                                               "B06007_004",
                                               "B06007_007",
                                               "B05001_006", 
                                               "B01001H_001",
                                               "B11001_005", 
                                               "B11001_006",
                                               "B11001_002"),
            state = x, geometry = TRUE)
  }),
  rbind
), 
 reduce(
  map(states, function(x) {
    get_acs(geography = "tract", variables = c("C17002_001", "C17002_008"),
            state = x, geometry = TRUE)
  }),
  rbind
))


# Tidy the naming and coding of census tracts
CTs_us <- CTs_us %>% 
  st_transform(102009) %>% 
  dplyr::select(-c("moe")) %>% 
  spread(variable, estimate) %>% 
  separate(NAME, into = c("CT", "County_name", "State_name"), sep = ",") 

CTs_us <- CTs_us %>% 
  mutate(ST_UID = substr(CTs_us$GEOID, 1, 2),
         CT_UID = as.numeric(gsub("Census Tract ", "", CT))) %>% 
  dplyr::select(-c("CT", "State_name"))

# Tidy the variables
CTs_us <- CTs_us %>% 
  mutate(language = B06007_002 + B06007_004 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011,
         households = B25012_002 + B25011_026,
         low_income_pct_pop = 1 - C17002_008/C17002_001, 
         lone_parent = B11001_005 + B11001_006) %>% 
  dplyr::select(-c("B06007_002", "B06007_004", "B06007_007", "B25070_007", "B25070_008", 
                   "B25070_009", "B25070_010", "B25091_008", "B25091_009", "B25091_010",
                   "B25091_011", "C17002_008", "C17002_001", "B11001_005", "B11001_006"))

names(CTs_us) <- 
  c("Geo_UID", "County_name", "white", "population", "non_citizen", "non_mover",
    "families", "university_education", "med_income", "rental", "owner_occupied", 
    "ST_UID", "CT_UID", "language", "housing_need", "households", "low_income_pct_pop",
    "lone_parent", "geometry")


CTs_us <- CTs_us %>% 
  mutate(citizen = population - non_citizen) %>% 
  dplyr::select(c("Geo_UID", "ST_UID", "CT_UID", "County_name", "population", "households", "med_income",
           "university_education", "housing_need", "non_mover", "owner_occupied", 
           "rental", "language", "citizen", "white", "low_income_pct_pop",
            "lone_parent", "families", "geometry"))

CTs_us <- CTs_us%>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households})) %>% 
  mutate(
    lone_parent_pct_families = lone_parent/families, 
    low_income = low_income_pct_pop * population)

CTs_us <- CTs_us %>% 
  st_join(MSAs_us, left = FALSE, suffix = c("",".y")) %>% 
  dplyr::select(c(1, 29, 30, 2, 5:28, 56))
           
# 2.5 Z SCORES
datalist = list()

for (n in c(1:nrow(MSAs_us))) {
  
  CTs_us_temp <- CTs_us %>% 
    filter(CMA_UID == as.numeric(st_drop_geometry(MSAs_us[n, 1]))) %>% 
    mutate_at(
      .vars = c("population", "households", "med_income",
                "university_education", "housing_need", "non_mover", "owner_occupied", 
                "rental", "language", "citizen", "white", "low_income", "lone_parent", 
                "university_education_pct_pop", "housing_need_pct_household", "non_mover_pct_pop", 
                "owner_occupied_pct_household", "rental_pct_household", "language_pct_pop", 
                "citizen_pct_pop", "white_pct_pop", "low_income_pct_pop", "lone_parent_pct_families"),
      .funs = list(`z` = ~{(.- mean(., na.rm = TRUE))/sd(., na.rm = TRUE)})) 
  
  datalist[[n]] <- CTs_us_temp
}

CTs_us = do.call(rbind, datalist)

rm(CTs_us_temp, datalist)

CTs_us$geometry <- st_cast(CTs_us$geometry, "MULTIPOLYGON")

########################################### 3 - COMBINE CENSUS TRACTS #####################################

CTs <- 
rbind(CTs_canada %>% 
        mutate(ST_UID = PR_UID,
               country = "Canada") %>%
        dplyr::select(-"PR_UID") %>% 
        st_transform(102009), 
      CTs_us %>% 
        mutate(country = "US") %>% 
        st_transform(102009))
