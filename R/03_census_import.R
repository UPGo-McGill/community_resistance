#### 03. CENSUS IMPORT  ########################################################

source("R/01_helper_functions.R")

# Census import for all census tracts in the following states
states <- unique(fips_codes$state)[1:51]

# List census variables
variables_us <- load_variables(2017, "acs5", cache = TRUE) 


## Import census variables for country

us <- 
  rbind(get_acs(geography = "us", variables = 
                  c("B01003_001", "B19013_001", "B15003_022", "B25070_007",
                    "B25070_008", "B25070_009", "B25070_010", "B25091_008",
                    "B25091_009", "B25091_010", "B25091_011", "B07001_017", 
                    "B25012_002", "B25011_026", "B06007_002", "B06007_004",
                    "B06007_007", "B05001_006", "B01001H_001", "B11001_005", 
                    "B11001_006", "B11001_002")), 
        get_acs(geography = "us", variables = c("C17002_001", "C17002_008"))
        ) %>% 
  dplyr::select(-c("moe")) %>% 
  spread(variable, estimate)

# Tidy the variables
us <- 
  us %>% 
  mutate(language = B06007_002 + B06007_004 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011,
         households = B25012_002 + B25011_026,
         low_income_pct_pop = 1 - C17002_008/C17002_001, 
         lone_parent = B11001_005 + B11001_006) %>% 
  dplyr::select(
    -c("B06007_002", "B06007_004", "B06007_007", "B25070_007", "B25070_008", 
       "B25070_009", "B25070_010", "B25091_008", "B25091_009", "B25091_010",
       "B25091_011", "C17002_008", "C17002_001", "B11001_005", "B11001_006")
    ) %>% 
  set_names(c("Geo_UID", "Country_name", "white", "population", "non_citizen", 
              "non_mover", "families", "university_education", "med_income", 
              "rental", "owner_occupied", "language",  "housing_need", 
              "households", "low_income_pct_pop", "lone_parent")) %>% 
  mutate(citizen = population - non_citizen) %>% 
  dplyr::select(c("Geo_UID", "Country_name", "population", "households", 
                  "med_income", "university_education", "housing_need", 
                  "non_mover", "owner_occupied", "rental", "language", 
                  "citizen", "white", "low_income_pct_pop", "lone_parent", 
                  "families")) %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", 
              "language", "citizen", "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households})) %>% 
  mutate(lone_parent_pct_families = lone_parent/families,
         low_income = low_income_pct_pop * population)


## Import census variables for MSAs

MSAs_us <- 
  rbind(get_acs(
    geography = "metropolitan statistical area/micropolitan statistical area", 
    variables = c("B01003_001", "B19013_001", "B15003_022", "B25070_007",
                  "B25070_008", "B25070_009", "B25070_010", "B25091_008",
                  "B25091_009", "B25091_010", "B25091_011", "B07001_017", 
                  "B25012_002", "B25011_026", "B06007_002", "B06007_004", 
                  "B06007_007", "B05001_006", "B01001H_001", "B11001_005", 
                  "B11001_006", "B11001_002")), 
    get_acs(geography = 
              "metropolitan statistical area/micropolitan statistical area", 
            variables = c("C17002_001", "C17002_008"))) %>% 
  dplyr::select(-c("moe")) %>% 
  spread(variable, estimate) %>% 
  separate(NAME, into = c("City_name", "Rest"), sep = ",") %>% 
  separate(Rest, into = c(NA, "State", "Type", NA), sep = " ") %>% 
  filter(str_detect(State, paste(states, collapse="|"))) %>% 
  mutate(language = B06007_002 + B06007_004 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011,
         households = B25012_002 + B25011_026, 
         low_income_pct_pop = 1 - C17002_008/C17002_001, 
         lone_parent = B11001_005 + B11001_006) %>% 
  dplyr::select(
    -c("B06007_002", "B06007_004", "B06007_007", "B25070_007", "B25070_008", 
       "B25070_009", "B25070_010", "B25091_008", "B25091_009", "B25091_010",
       "B25091_011", "C17002_008", "C17002_001", "B11001_005", "B11001_006")
    ) %>% 
  set_names(c("CMA_UID", "CMA_name", "State", "Type", "white", "population", 
              "non_citizen", "non_mover", "families", "university_education", 
              "med_income", "rental", "owner_occupied", "language", 
              "housing_need", "households", "low_income_pct_pop", "lone_parent")
            )  %>% 
  mutate(citizen = population - non_citizen) %>% 
  dplyr::select(
    c("CMA_UID", "CMA_name", "State", "population", "households", "med_income",
      "university_education", "housing_need", "non_mover", "owner_occupied", 
      "rental", "language", "citizen", "white", "low_income_pct_pop", 
      "lone_parent", "families")) %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", "language", "citizen", 
              "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households})) %>% 
  mutate(lone_parent_pct_families = lone_parent/families,
         low_income = low_income_pct_pop *population)

MSAs_us <- 
  core_based_statistical_areas(cb = TRUE, class = "sf", refresh = TRUE) %>% 
  st_transform(102009) %>% 
  dplyr::select(GEOID, geometry) %>% 
  left_join(MSAs_us, ., by = c("CMA_UID" = "GEOID")) %>% 
  st_as_sf(sf_column_name = "geometry")


## Import census variables for CTs

CTs <-
  map(states, ~get_acs(
    geography = "tract",
    variables = c(
      "B01003_001", "B19013_001", "B15003_022", "B25070_007", 
      "B25070_008", "B25070_009", "B25070_010", "B25091_008",
      "B25091_009", "B25091_010", "B25091_011", "B07001_017", 
      "B25012_002", "B25011_026", "B06007_002", "B06007_004",
      "B06007_007", "B05001_006", "B01001H_001", "B11001_005", 
      "B11001_006", "B11001_002", "C17002_001", "C17002_008"), 
  state = .x,
  geometry = TRUE)) %>% 
  do.call(rbind, .)

# Tidy the naming and coding of census tracts
CTs <- 
  CTs %>% 
  st_transform(102009) %>% 
  dplyr::select(-moe) %>% 
  spread(variable, estimate) %>% 
  separate(NAME, into = c("CT", "County_name", "State_name"), sep = ",") %>% 
  mutate(ST_UID = substr(GEOID, 1, 2),
         CT_UID = as.numeric(gsub("Census Tract ", "", CT))) %>% 
  dplyr::select(-CT, -State_name) %>% 
  as_tibble() %>% 
  st_as_sf()

# Tidy the variables
CTs <-
  CTs %>% 
  mutate(language = B06007_002 + B06007_004 + B06007_007,
         housing_need = B25070_007 + B25070_008 + B25070_009 + B25070_010 +
           B25091_008 + B25091_009 + B25091_010 + B25091_011,
         households = B25012_002 + B25011_026,
         low_income_pct_pop = 1 - C17002_008/C17002_001, 
         lone_parent = B11001_005 + B11001_006) %>% 
  dplyr::select(
    -c(B06007_002, B06007_004, B06007_007, B25070_007, B25070_008, B25070_009, 
       B25070_010, B25091_008, B25091_009, B25091_010, B25091_011, C17002_008,
       C17002_001, B11001_005, B11001_006)
    ) %>% 
  dplyr::select(-geometry, everything(), geometry) %>% 
  set_names(
    c("Geo_UID", "County_name", "white", "population", "non_citizen", 
      "non_mover", "families", "university_education", "med_income", "rental", 
      "owner_occupied", "ST_UID", "CT_UID", "language", "housing_need", 
      "households", "low_income_pct_pop", "lone_parent", "geometry")) %>% 
  mutate(citizen = population - non_citizen) %>% 
  dplyr::select(
    c("Geo_UID", "ST_UID", "CT_UID", "County_name", "population", "households", 
      "med_income", "university_education", "housing_need", "non_mover", 
      "owner_occupied", "rental", "language", "citizen", "white", 
      "low_income_pct_pop", "lone_parent", "families", "geometry")) %>% 
  mutate_at(
    .vars = c("university_education", "non_mover", "language", "citizen", 
              "white"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("housing_need", "owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households})) %>% 
  mutate(
    lone_parent_pct_families = lone_parent/families, 
    low_income = low_income_pct_pop * population) %>% 
  st_join(MSAs_us, left = FALSE, suffix = c("", ".y")) %>% 
  dplyr::select(c(1, 29, 30, 2, 5:28, 56))
  
         
## Z scores

CTs <- 
  CTs %>% 
  group_by(CMA_UID) %>% 
  mutate_at(
    .vars = 
      c("population", "households", "med_income", "university_education", 
        "housing_need", "non_mover", "owner_occupied", "rental", "language", 
        "citizen", "white", "low_income", "lone_parent", 
        "university_education_pct_pop", "housing_need_pct_household", 
        "non_mover_pct_pop", "owner_occupied_pct_household", 
        "rental_pct_household", "language_pct_pop", "citizen_pct_pop", 
        "white_pct_pop", "low_income_pct_pop", "lone_parent_pct_families"),
    .funs = list(`z` = ~{(.- mean(., na.rm = TRUE))/sd(., na.rm = TRUE)})) %>% 
  ungroup() %>% 
  mutate(geometry = st_cast(geometry, "MULTIPOLYGON")) %>% 
  dplyr::select(-geometry, everything(), geometry)


## Clean up
save(MSAs_us, file = "data/MSAs.Rdata")
rm(MSAs_us, states, us, variables_us)

