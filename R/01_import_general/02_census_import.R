######################################### CENSUS IMPORT  ###############################

source("R/01_import_general/01_helper_functions.R")

# census import for all census tracts in Canada and the census tracts in the US in the following states
states <- unique(fips_codes$state)[1:51] 
states <- c("NY", "CA", "LA", "FL", "DC")

########################################### 1 - CANADA #######################################

# 1.1 LIST CENSUS VARIABLES
variables_canada <- list_census_vectors("CA16")

# 1.2 IMPORT CENSUS VARIABLES FOR COUNTRY-WIDE AVERAGE
canada <-
  get_census(
    dataset = "CA16", regions = list(C = "Canada"),
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_512", 
                "v_CA16_3393", "v_CA16_3996"),
    geo_format = "sf") %>% 
  st_transform(3347) %>% 
  select(GeoUID, Population, Households, contains("v_CA"))

names(canada) <- 
  c("Geo_UID", "population", "households", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupied", 
    "rental", "official_language", "citizen", "white", "geometry")

canada <- canada%>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households}))

# 1.3 IMPORT CENSUS VARIABLES FOR ALL CMAs
CMAs_canada <-
  get_census(
    dataset = "CA16", regions = list(C = "Canada"), level = "CMA",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6695",
                "v_CA16_4837", "v_CA16_4838", "v_CA16_512", 
                "v_CA16_3393", "v_CA16_3996"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  filter(Type == "CMA") %>% 
  select(GeoUID, CMA_name = name, Population, Households, contains("v_CA")) 

CMAs_canada$CMA_name <- CMAs_canada$CMA_name %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("'", "") %>% 
  str_replace("`", "")

names(CMAs_canada) <- 
  c("CMA_UID", "CMA_name", "population", "households", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupied", 
    "rental", "official_language", "citizen", "white", "geometry")

CMAs_canada <- CMAs_canada%>% 
  separate(CMA_name, into = c("CMA_name", NA), sep = "[(]") %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
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
                "v_CA16_4837", "v_CA16_4838", "v_CA16_512", 
                "v_CA16_3393", "v_CA16_3996"),
    geo_format = "sf") %>% 
  st_transform(32618) %>% 
  filter(Type == "CT") %>% 
  select(GeoUID, PR_UID, CMA_UID, Population, Households, contains("v_CA"))

CTs_canada <- CTs_canada %>% 
  inner_join(st_drop_geometry(select(CMAs_canada, c(1,2))), by = "CMA_UID") %>% 
  select(GeoUID, CMA_UID, CMA_name, everything()) 

names(CTs_canada) <- 
  c("Geo_UID", "CMA_UID", "CMA_name", "PR_UID", "population", "households", "med_income",
    "university_education", "housing_need", "non_mover", "owner_occupied", 
    "rental", "official_language", "citizen", "white", "geometry")

CTs_canada <- CTs_canada%>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households}))


# 1.5 Z SCORES

n = 1

datalist = list()

repeat{

  CTs_canada_temp <- CTs_canada %>% 
    filter(CMA_UID == as.numeric(st_drop_geometry(CMAs_canada[n, 1]))) %>% 
    mutate_at(
      .vars = c("population", "households", "med_income",
                "university_education", "housing_need", "non_mover", "owner_occupied", 
                "rental", "official_language", "citizen", "white"),
      .funs = list(`z` = ~{(.- mean(.))/sd(.)})) 
  
  datalist[[n]] <- CTs_canada_temp

  n = n + 1
  
  if (n > nrow(CMAs_canada)) {
    break
    
  }
}

CTs_canada = do.call(rbind, datalist)

rm(CTs_canada_temp, datalist)


########################################### 2 - UNITED STATES #####################################

# 2.1 LIST CENSUS VARIBALES
variables_us <- load_variables(2017, "acs5", cache = TRUE) 

# 2.2 IMPORT CENSUS VARIABLES FOR COUNTRY-WIDE AVERAGE
us <- get_acs(geography = "us", variables = c("B01003_001", 
                                              "B25001_001", 
                                              "B06011_001",
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
                                              "B06007_003",
                                              "B06007_007",
                                              "B05001_006", 
                                              "B01001H_001"))

us <- us %>% 
  select(-c("moe")) %>% 
  spread(variable, estimate)

# tidy the variables
us <- us %>% 
  mutate(official_language = B06007_002 + B06007_003 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011) %>% 
  select(-c("B06007_002", "B06007_003", "B06007_007", "B25070_007", "B25070_008", 
            "B25070_009", "B25070_010", "B25091_008", "B25091_009", "B25091_010",
            "B25091_011"))

names(us) <- 
  c("Geo_UID", "Country_name", "white", "population", "non_citizen", "med_income",
    "non_mover", "university_education", "households", 
    "rental", "owner_occupied", "official_language", 
    "housing_need")

us <- us %>% 
  mutate(citizen = population - non_citizen) %>% 
  select(c("Geo_UID", "Country_name", "population", "households", "med_income",
           "university_education", "housing_need", "non_mover", "owner_occupied", 
           "rental", "official_language", "citizen", "white"))

us <- us %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households}))

# 2.3 IMPORT CENSUS VARIABLES FOR ALL CMAs
MSAs_us <- get_acs(geography = "metropolitan statistical area/micropolitan statistical area", 
                   variables = c("B01003_001", 
                                 "B25001_001", 
                                 "B06011_001",
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
                                 "B06007_003",
                                 "B06007_007",
                                 "B05001_006", 
                                 "B01001H_001"))
MSAs_us <- MSAs_us %>% 
  select(-c("moe")) %>% 
  spread(variable, estimate) %>% 
  separate(NAME, into = c("City_name", "Rest"), sep = ",") %>% 
  separate(Rest, into = c(NA, "State", "Type", NA), sep = " ") %>% 
  filter(str_detect(State, paste(states, collapse="|")))

MSAs_us <- MSAs_us %>% 
  mutate(official_language = B06007_002 + B06007_003 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011) %>% 
  select(-c("B06007_002", "B06007_003", "B06007_007", "B25070_007", "B25070_008", 
            "B25070_009", "B25070_010", "B25091_008", "B25091_009", "B25091_010",
            "B25091_011"))

names(MSAs_us) <- 
  c("CMA_UID", "CMA_name", "State", "Type", "white", "population", "non_citizen", "med_income",
    "non_mover", "university_education", "households", 
    "rental", "owner_occupied", "official_language", 
    "housing_need")

MSAs_us <- MSAs_us %>% 
  mutate(citizen = population - non_citizen) %>% 
  select(c("CMA_UID", "CMA_name", "State", "population", "households", "med_income",
           "university_education", "housing_need", "non_mover", "owner_occupied", 
           "rental", "official_language", "citizen", "white"))

MSAs_us <- MSAs_us%>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households}))

MSAs_us <- core_based_statistical_areas(cb = TRUE, class = "sf", refresh = TRUE) %>% 
  st_transform(26918) %>% 
  select(c("GEOID", "geometry")) %>% 
  left_join(MSAs_us, ., by = c("CMA_UID" = "GEOID")) %>% 
  st_as_sf(sf_column_name = "geometry")

# 2.4 IMPORT CENSUS VARIABLES FOR ALL CENSUS TRACTS (include geometries)

CTs_us <- reduce(
  map(states, function(x) {
  get_acs(geography = "tract", variables = c("B01003_001", 
                                             "B25001_001", 
                                             "B06011_001",
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
                                             "B06007_003",
                                             "B06007_007",
                                             "B05001_006", 
                                             "B01001H_001"),
          state = x, geometry = TRUE)
}),
  rbind
)

# tidy the naming and coding of census tracts
CTs_us <- CTs_us %>% 
  st_transform(26918) %>% 
  select(-c("moe")) %>% 
  spread(variable, estimate) %>% 
  separate(NAME, into = c("CT", "County_name", "State_name"), sep = ",") 

CTs_us$geometry <- st_cast(CTs_us$geometry, "MULTIPOLYGON")

CTs_us <- CTs_us %>% 
  mutate(ST_UID = substr(CTs_us$GEOID, 1, 2),
         CT_UID = as.numeric(gsub("Census Tract ", "", CT))) %>% 
  select(-c("CT", "State_name"))

# tidy the variables
CTs_us <- CTs_us %>% 
  mutate(official_language = B06007_002 + B06007_003 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011) %>% 
  select(-c("B06007_002", "B06007_003", "B06007_007", "B25070_007", "B25070_008", 
            "B25070_009", "B25070_010", "B25091_008", "B25091_009", "B25091_010",
            "B25091_011"))

names(CTs_us) <- 
  c("Geo_UID", "County_name", "white", "population", "non_citizen", "med_income",
    "non_mover", "university_education", "households", 
    "rental", "owner_occupied", "ST_UID", "CT_UID", "official_language", 
    "housing_need", "geometry")

CTs_us <- CTs_us %>% 
  mutate(citizen = population - non_citizen) %>% 
  select(c("Geo_UID", "ST_UID", "CT_UID", "County_name", "population", "households", "med_income",
           "university_education", "housing_need", "non_mover", "owner_occupied", 
           "rental", "official_language", "citizen", "white", "geometry"))
  
CTs_us <- CTs_us%>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "official_language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households}))

CTs_us <- CTs_us %>% 
  st_join(MSAs_us, left = FALSE, suffix = c("",".y")) %>% 
  select(c(1, 24, 25, 2, 5:23, 46)) 

# 2.5 Z SCORES

n = 1

datalist = list()

repeat{
  
  CTs_us_temp <- CTs_us %>% 
    filter(CMA_UID == as.numeric(st_drop_geometry(MSAs_us[n, 1]))) %>% 
    mutate_at(
      .vars = c("population", "households", "med_income",
                "university_education", "housing_need", "non_mover", "owner_occupied", 
                "rental", "official_language", "citizen", "white"),
      .funs = list(`z` = ~{(.- mean(.))/sd(.)})) 
  
  datalist[[n]] <- CTs_us_temp
  
  n = n + 1
  
  if (n > nrow(MSAs_us)) {
    break
    
  }
}

CTs_us = do.call(rbind, datalist)

rm(CTs_us_temp, datalist)


