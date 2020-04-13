################################################# MODELLING ##############################################################################

source("R/01_helper_functions.R")

load("data/modeling_data.Rdata")





neighbourhoods_table <- 
  neighbourhoods_table %>% 
  mutate(CRI_adjusted = CRI/max(CRI, na.rm = TRUE))

# Cannot be used with grouping
model <- 
  neighbourhoods_table %>% 
  #filter(CRI > 0) %>% 
  #filter(city %in% cityname_large) %>% 
  filter(active_listings > 0) %>% 
  glm((CRI) ~ 
              active_listings +
              active_listings_yoy +
             # PR_50_ct +
              housing_loss_pct_households+
              #population +
              med_income +
              housing_need_pct_household +
              #non_mover_pct_pop +
              owner_occupied_pct_household +
              low_income_pct_pop,
  
              #language_pct_pop +
              #white_pct_pop + 
              #citizen_pct_pop +
              #university_education_pct_pop +
              #lone_parent_pct_families,
             # city,
      family = gaussian, 
      data = .)

summary(model)

r.squaredGLMM(model)

plot(model)

best <- 
  glmulti(model, level = 1, crit = "aic")

summary(best)

best <- 
  neighbourhoods_table %>% 
  #filter(active_listings > 0) %>% 
  glm(CRI ~ 
        active_listings +
        #active_listings_yoy +
         PR_50_ct +
        # population + 
         med_income +
        housing_need_pct_household +
        #non_mover_pct_pop +
        owner_occupied_pct_household +
        low_income_pct_pop,
        #lone_parent_pct_families,
      #city, 
      family = gaussian, 
      data = .)

r.squaredGLMM(best)

# Mixed modeling
model_2 <- 
  neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer((CRI) ~
         #scale(active_listings) +
         #scale(EH_pct) +
         #scale(revenue_LTM_per_listing) + 
         #scale(GH_pct) +
         #scale(FREH_pct) +
         #scale(ML_pct) +
         #scale(PR_50_ct) +
         #scale(revenue_10pct) +
         #scale(active_listings_yoy) +
         #scale(housing_loss_pct_listings) + 
         #   scale(population)+ 
       #  scale(med_income_z)+ 
       # scale(low_income_z)+
       #scale(university_education_z)+
       (housing_need_pct_household) + 
         # scale(non_mover_z) + 
         #scale(owner_occupied_z) +
         #scale(white_z) +
         #scale(citizen_z) +
         #scale(lone_parent_z) +
         #scale(language_z)+
         (1 | city), 
       data = ., 
       nAGQ = 100)

ghq %>% 
  summary()





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

