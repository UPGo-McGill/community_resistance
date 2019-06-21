######################################### SOCIAL CAPITAL INDEX ###############################

source("R/01_helper_functions.R")

# Since focusing on Canada and the US, the social capital index is the relative 
  # social capital of a census tract in relation to the country-wide average

CTs_canada <- CTs_canada %>% 
  mutate(SCI = (med_income/canada$med_income + 
           university_education_pct_pop/canada$university_education_pct_pop +
           non_mover_pct_pop/canada$non_mover_pct_pop + 
           official_language_pct_pop/canada$official_language_pct_pop + 
           citizen_pct_pop/canada$citizen_pct_pop + 
           white_pct_pop/canada$white_pct_pop +
           (1 - housing_need_pct_household)/(1 - canada$housing_need_pct_household) +
           owner_occupied_pct_household/canada$owner_occupied_pct_household)/8)
           
CTs_us <- CTs_us %>% 
  mutate(SCI = (med_income/us$med_income + 
                  university_education_pct_pop/us$university_education_pct_pop +
                  non_mover_pct_pop/us$non_mover_pct_pop + 
                  official_language_pct_pop/us$official_language_pct_pop + 
                  citizen_pct_pop/us$citizen_pct_pop + 
                  white_pct_pop/us$white_pct_pop +
                  (1 - housing_need_pct_household)/(1 - us$housing_need_pct_household) +
                  owner_occupied_pct_household/us$owner_occupied_pct_household)/8)           

# Since focusing on Canada and the US, the social capital index is the relative 
# social capital of a census tract in relation to the city/county-wide average



st_join(CTs_us, CMAs_us)

class(CMAs_us)
