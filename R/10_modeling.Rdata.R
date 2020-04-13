################################################# MODELLING ##############################################################################

source("R/01_helper_functions.R")

load("data/modeling_data.Rdata")

# Test out different STR measures
model <- 
  neighbourhoods_table %>% 
 # filter(CRI > 0) %>% 
  filter(active_listings > 0) %>% 
  glm((CRI) ~ 
        active_listings +
        active_listings_yoy +
       # PR_10_ct, 
       # PR_25_ct,
       # PR_50_ct,
       # housing_loss_pct_households,
       # housing_loss_pct_listings,
       # revenue_10pct,
       # FREH_pct,
       # ML_pct,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

cities_table <- 
  cities_table %>% 
  arrange(population)

bottom_50 <- 
  cities_table[1:50,1] %>% 
  st_drop_geometry() %>% 
  do.call(paste, .)

model <- 
  neighbourhoods_table %>% 
  # filter(!(city %in% bottom_50)) %>% 
  # filter(city %in% cityname[51:115]) %>% 
  glm((CRI) ~ 
        active_listings +
        active_listings_yoy +
        # PR_10_ct, 
        # PR_25_ct,
         PR_50_ct,
        # housing_loss_pct_households,
        # housing_loss_pct_listings,
        # revenue_10pct,
        # FREH_pct,
        # ML_pct,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

# Notes:
# active_listings > 0 and without the filter - same results
  # PR_10_ct/25 somewhat significant - PR_50_ct most out of the principal residence
  # housing_loss_pct_households very significant
  # housing_loss_pct_lisings, revenue_10pct, FREH, ML not significant
# CRI > 0
  # doesnt work with PR_x_ct, housing_loss - gives opposite to what we want
  # DO NOT USE THIS FILTER! 
# Take out smallest 50 cities in terms of population and media attention
  # housing_loss_pct_households isnt as significant when filtering in terms of population 
    # just as significant when filtering in terms of media attention
  # PR_50_ct is not as significant when filtering in terms of media attention
    # just as significant when filtering in terms of population
  # same results, though arent as significant... we can decide what we want to include

# CONCLUSION: USE EITHER PR_50_ct OR housing_loss_pct_households FOR AIRBNB MEASURES
  # FILTERING OUT BOTTOM 50 CITIES IS POSSIBLE AND STILL WORKS
  # DO NOT FILTER OUT NEGATIVE CRI

# Test out different output variables (CRI, CRI_2yr, CRI_1yr)
model <- 
  neighbourhoods_table %>% 
  # filter(!(city %in% bottom_50)) %>% 
  # filter(city %in% cityname[51:115]) %>% 
  glm((CRI) ~ 
        active_listings +
        active_listings_yoy +
        # PR_10_ct, 
        # PR_25_ct,
        # PR_50_ct,
         housing_loss_pct_households,
        # housing_loss_pct_listings,
        # revenue_10pct,
        # FREH_pct,
        # ML_pct,
        family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

# The 5 year CRI is best when including all cities and when filtering out the bottom 50.

# Find the complete model
# Test out filters before the model
  # CRI > 0
  # Take out x number of cities with the least media attention 

model <- 
  neighbourhoods_table %>% 
 # filter(CRI > 0) %>% 
 # filter(city %in% cityname[51:115]) %>% 
 # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
        active_listings +
        active_listings_yoy +
        # PR_50_ct +
         housing_loss_pct_households +
        population +
        med_income +
        housing_need_pct_household +
        non_mover_pct_pop +
        owner_occupied_pct_household +
        low_income_pct_pop,
        language_pct_pop +
        white_pct_pop + 
        citizen_pct_pop +
        university_education_pct_pop +
        lone_parent_pct_families,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

# Active listings, housing_loss_pct_households, and non_mover_pct_pop are the most significant
# Filtering CRI > 0 gives results we do not want (low housing need, high income, high rental, 
      # low income pct pop)
# Filtering out bottom 50 cities in terms of media attention and population is possible
  # housing_loss_pct_households and non_mover are still significant, though less so
# Still can choose between PR_50_oct and housing_loss_pct_households

# Test for the best model
best <- 
  glmulti(model, level = 1, crit = "aic")

summary(best)

# Only has the intercept, active_listings, housing_loss_pct_households, and non_mover_pct pop

best <- 
  neighbourhoods_table %>% 
  # filter(CRI > 0) %>% 
  # filter(city %in% cityname[51:115]) %>% 
  # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
        active_listings +
        # active_listings_yoy +
        # PR_50_ct +
        housing_loss_pct_households +
        # population +
        # med_income +
        # housing_need_pct_household +
        non_mover_pct_pop,
        # owner_occupied_pct_household +
        # low_income_pct_pop,
        # language_pct_pop +
        # white_pct_pop + 
        # citizen_pct_pop +
        # university_education_pct_pop +
        # lone_parent_pct_families,
      family = gaussian, 
      data = .)

summary(best)

r.squaredGLMM(best)

# Effects of the filters?
  # CRI > 0 - messes it up
  # filtering out the bottom cities keeps everything significant but increases the R squared

# Add in the city as a variable

neighbourhoods_table$city <- as.factor(neighbourhoods_table$city)

model <- 
  neighbourhoods_table %>% 
  # filter(CRI > 0) %>% 
   filter(city %in% cityname[51:115]) %>% 
  # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
         active_listings +
        # active_listings_yoy +
        # PR_50_ct +
         housing_loss_pct_households+
        #  population +
        #  med_income +
        #  housing_need_pct_household +
          non_mover_pct_pop +
        #  owner_occupied_pct_household +
        #  low_income_pct_pop +
        #  language_pct_pop +
        #  white_pct_pop + 
        #  citizen_pct_pop +
        #  university_education_pct_pop +
        # lone_parent_pct_families +
         city,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

# Including the city as a variable if you filter out the bottom 50 cities (in terms of media)
  # provides a higher AIC and lower R squared. This is not the case if you do not filter out  
  # the cities

# Adding in region
model <- 
  neighbourhoods_table %>% 
  # filter(CRI > 0) %>% 
  filter(city %in% cityname[51:115]) %>% 
  # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
        active_listings +
        # active_listings_yoy +
        # PR_50_ct +
        housing_loss_pct_households+
        #  population +
        #  med_income +
        #  housing_need_pct_household +
        non_mover_pct_pop +
        #  owner_occupied_pct_household +
        #  low_income_pct_pop +
        #  language_pct_pop +
        #  white_pct_pop + 
        #  citizen_pct_pop +
        #  university_education_pct_pop +
        # lone_parent_pct_families +
        # city,
        region,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

# Region is not significant if city is also taken. However if it is not, 
  # the west is overly influential (more CRI in the west)

# To try to explain this city variance, include city variables in place of 
  # (or alongside?) neighbourhood variables

model <- 
  neighbourhoods_table %>% 
  # filter(CRI > 0) %>% 
  # filter(city %in% cityname[51:115]) %>% 
  # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
        active_listings_city + 
        active_listings_yoy_city +
        PR_50_pct +
        housing_loss_pct_households_city +
        population_city +
        med_income_city +
        housing_need_pct_household_city +
        non_mover_pct_pop_city +
        owner_occupied_pct_household_city +
        low_income_pct_pop_city +
        language_pct_pop_city +
        white_pct_pop_city + 
        citizen_pct_pop_city +
        university_education_pct_pop_city +
        lone_parent_pct_families_city +
        region,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

best <- 
  glmulti(model, level = 1, crit = "aic")

summary(best)

# The variables at the city-level that explain variance: 
  # active_listings, non_mover, owner_occupied, language, citizen, lone_parent

best <- 
  neighbourhoods_table %>% 
  # filter(CRI > 0) %>% 
  # filter(city %in% cityname[51:115]) %>% 
  # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
        active_listings_city + 
        non_mover_pct_pop_city +
        owner_occupied_pct_household_city +
        language_pct_pop_city +
        citizen_pct_pop_city +
        lone_parent_pct_families_city,
      family = gaussian, 
      data = .)

r.squaredGLMM(best)


# UPDATED MODEL

  # R SQUARED 0.19, AIC 1976
  # TAKE OUT BOTTOM 50 CITIES BY MEDIA - RSQUARED 0.2, AIC 1624

model <- 
  neighbourhoods_table %>% 
   #filter(CRI > 0) %>% 
  filter(city %in% cityname[51:115]) %>% 
  # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
        active_listings +
        active_listings_yoy +
        housing_loss_pct_households +
        population+
        housing_need_pct_household+
        non_mover_pct_pop +
        active_listings_city + 
        non_mover_pct_pop_city +
        owner_occupied_pct_household_city +
        language_pct_pop_city +
        citizen_pct_pop_city +
        lone_parent_pct_families_city +
        region,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

plot(model)


# Model by city
model <- 
  cities_table %>% 
  # filter(CRI > 0) %>% 
  # filter(city %in% cityname[51:115]) %>% 
  # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
        active_listings +
        active_listings_yoy +
        # PR_50_ct +
        housing_loss_pct_households +
        population +
        med_income +
        housing_need_pct_household +
        non_mover_pct_pop +
        owner_occupied_pct_household +
        low_income_pct_pop,
        language_pct_pop +
        white_pct_pop + 
        citizen_pct_pop +
        university_education_pct_pop +
        lone_parent_pct_families,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

# Not much can be said when just considering city
  # non_mover has a positive correlate and population a negative
  # Explains about 25% of the variance

best <- 
  glmulti(model, level = 1, crit = "aic")

summary(best)

model <- 
  cities_table %>% 
  # filter(CRI > 0) %>% 
  # filter(city %in% cityname[51:115]) %>% 
  # filter(!(city %in% bottom_50)) %>% 
  glm((CRI) ~ 
        population +
         non_mover_pct_pop,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)









weightable(best)

# Top 4 have similair aic

model1 <-   
  neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glm(CRI ~ active_listings +
        PR_50_ct,
      family = gaussian(), 
      data = .)

model2 <- 
  neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glm(CRI ~ active_listings +
        active_listings_yoy +
        PR_50_ct,
      family = gaussian(), 
      data = .)

model3 <- 
  neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glm(CRI ~ active_listings,
      family = gaussian(), 
      data = .)

model4 <- 
  neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glm(CRI ~ active_listings +
        active_listings_yoy,
      family = gaussian(), 
      data = .)

model_average <- 
  model.avg(model1, 
            model2, 
            model3, 
            model4)

summary(model_average)

