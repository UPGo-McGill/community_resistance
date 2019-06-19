######################################### SOCIAL CAPITAL INDEX ###############################

source("R/01_helper_functions.R")

########################################### 1 - CANADA #######################################

# list census variables
variables_canada <- list_census_vectors("CA16")

# import Canadian CMAs
CMAs_canada <-
  get_census(
    dataset = 'CA16', regions = list(C = "Canada"), level = 'CMA',
    geo_format = "sf") %>% 
  st_transform(3347) %>% 
  filter(Type == "CMA")%>%
  select(GeoUID, CMA_name = name)

# remove accents from CMA names
CMAs_canada$CMA_name <- CMAs_canada$CMA_name %>% 
  iconv(to = "ASCII//TRANSLIT") %>% 
  str_replace("'", "") %>% 
  str_replace("`", "")

# import census variables for all census tracts in Canada
CTs_canada <-
  get_census(
    dataset = "CA16", regions = list(C = "Canada"), level = "CT",
    vectors = c("v_CA16_2398", "v_CA16_5078", "v_CA16_4888", "v_CA16_6698",
                "v_CA16_6725", "v_CA16_4837", "v_CA16_4838", "v_CA16_524", 
                "v_CA16_3390", "v_CA16_3957"),
    geo_format = "sf") %>% 
  st_transform(3347) %>% 
  filter(Type == "CT") %>% 
  select(GeoUID, PR_UID, CMA_UID, Population, Households, contains("v_CA"))

CTs_canada <- CTs_canada %>% 
  inner_join(st_drop_geometry(CMAs_canada), by = c("CMA_UID" = "GeoUID")) %>% 
  select(GeoUID, PR_UID, CMA_UID, CMA_name, everything()) 

names(CTs_canada) <- 
  c("Geo_UID", "PR_UID", "CMA_UID", "CMA_name", "population", "households", "med_income",
    "university_education", "housing_need", "moved_1yr", "moved_5yr", "owner_occupied", 
    "rental", "no_official_language", "citizen", "visible_minority", "geometry")

CTs_canada <- CTs_canada%>% 
  mutate_at(
    .vars = c("university_education", "housing_need", "moved_1yr", "moved_5yr", 
              "no_official_language", "citizen", "visible_minority"),
    .funs = list(`pct_pop` = ~{. / population})) %>% 
  mutate_at(
    .vars = c("owner_occupied", "rental"),
    .funs = list(`pct_household` = ~{. / households}))

########################################### 2 - UNITED STATES #####################################

# list census variables
variables_us <- load_variables(2017, "acs5", cache = TRUE) 

# get state codes
states <- unique(fips_codes$state)[1:51]

# import variables for every census tract with geometries
CTs_us <- reduce(
  map(states, function(x) {
  get_acs(geography = "tract", variables = c("B01003_001", "B25001_001", 
                                             "B19001_001","B15003_022",
                                             "B07001_017", "B25012_002", "B25011_026",
                                             "B06007_008", "B05001_006", "B01001H_001"),
          state = x, geometry = TRUE)
}),
  rbind
)

# tidy the data such that it is in the same format as the canadian census data
CTs_us <- CTs_us %>% 
  select(-c("moe")) %>% 
  spread(variable, estimate) %>% 
  separate(NAME, into = c("CT", "County_name", "State_name"), sep = ",") 

CTs_us <- CTs_us %>% 
  mutate(ST_UID = substr(CTs_us$GEOID, 1, 2),
         CT_UID = as.numeric(gsub("Census Tract ", "", CT))) %>% 
  select(-c("CT", "State_name"))






