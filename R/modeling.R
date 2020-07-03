library(tidyverse); library(rJava); library(glmulti); library(MuMIn); library(leaps)


load("data/modeling_David.Rdata")

model <- 
  neighbourhoods_table %>% 
  filter(city %in% cityname[51:115]) %>%
  # filter(!(city %in% bottom_50)) %>%
  glm((CRI) ~ 
        active_listings +
        active_listings_yoy +
        housing_loss_pct_households +
        population +
        med_income +
        housing_need_pct_household +
        non_mover_pct_pop +
        owner_occupied_pct_household +
        low_income_pct_pop+
        language_pct_pop +
        white_pct_pop + 
        citizen_pct_pop +
        university_education_pct_pop +
        lone_parent_pct_families +
        active_listings_city + 
        active_listings_yoy_city +
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
# summary(model)
# r.squaredGLMM(model)

best_1 <- glmulti(model, level = 1, crit = "aic", method = "l", chunk = 1, chunks = 100)
# best_2 <- glmulti(model, level = 1, crit = "aic", chunk = 2, chunks = 40)
# best_3 <- glmulti(model, level = 1, crit = "aic", chunk = 3, chunks = 40)
# best_4 <- glmulti(model, level = 1, crit = "aic", chunk = 4, chunks = 40)
# best_5 <- glmulti(model, level = 1, crit = "aic", chunk = 5, chunks = 40)
# best_6 <- glmulti(model, level = 1, crit = "aic", chunk = 6, chunks = 40)
# best_7 <- glmulti(model, level = 1, crit = "aic", chunk = 7, chunks = 40)
# best_8 <- glmulti(model, level = 1, crit = "aic", chunk = 8, chunks = 40)
# best_9 <- glmulti(model, level = 1, crit = "aic", chunk = 9, chunks = 40)
# best_10 <- glmulti(model, level = 1, crit = "aic", chunk = 10, chunks = 40)
# best_11 <- glmulti(model, level = 1, crit = "aic", chunk = 11, chunks = 40)
# best_12 <- glmulti(model, level = 1, crit = "aic", chunk = 12, chunks = 40)
# best_13 <- glmulti(model, level = 1, crit = "aic", chunk = 13, chunks = 40)
# best_14 <- glmulti(model, level = 1, crit = "aic", chunk = 14, chunks = 40)
# best_15 <- glmulti(model, level = 1, crit = "aic", chunk = 15, chunks = 40)
# best_16 <- glmulti(model, level = 1, crit = "aic", chunk = 16, chunks = 40)
# best_17 <- glmulti(model, level = 1, crit = "aic", chunk = 17, chunks = 40)
# best_18 <- glmulti(model, level = 1, crit = "aic", chunk = 18, chunks = 40)
# best_19 <- glmulti(model, level = 1, crit = "aic", chunk = 19, chunks = 40)
# best_20 <- glmulti(model, level = 1, crit = "aic", chunk = 20, chunks = 40)
# best_21 <- glmulti(model, level = 1, crit = "aic", chunk = 21, chunks = 40)
# best_22 <- glmulti(model, level = 1, crit = "aic", chunk = 22, chunks = 40)
# best_23 <- glmulti(model, level = 1, crit = "aic", chunk = 23, chunks = 40)
# best_24 <- glmulti(model, level = 1, crit = "aic", chunk = 24, chunks = 40)
# best_25 <- glmulti(model, level = 1, crit = "aic", chunk = 25, chunks = 40)
# best_26 <- glmulti(model, level = 1, crit = "aic", chunk = 26, chunks = 40)
# best_27 <- glmulti(model, level = 1, crit = "aic", chunk = 27, chunks = 40)
# best_28 <- glmulti(model, level = 1, crit = "aic", chunk = 28, chunks = 40)
# best_29 <- glmulti(model, level = 1, crit = "aic", chunk = 29, chunks = 40)
# best_30 <- glmulti(model, level = 1, crit = "aic", chunk = 30, chunks = 40)
# best_31 <- glmulti(model, level = 1, crit = "aic", chunk = 31, chunks = 40)
# best_32 <- glmulti(model, level = 1, crit = "aic", chunk = 32, chunks = 40)
# best_33 <- glmulti(model, level = 1, crit = "aic", chunk = 33, chunks = 40)
# best_34 <- glmulti(model, level = 1, crit = "aic", chunk = 34, chunks = 40)
# best_35 <- glmulti(model, level = 1, crit = "aic", chunk = 35, chunks = 40)
# best_36 <- glmulti(model, level = 1, crit = "aic", chunk = 36, chunks = 40)
# best_37 <- glmulti(model, level = 1, crit = "aic", chunk = 37, chunks = 40)
# best_38 <- glmulti(model, level = 1, crit = "aic", chunk = 38, chunks = 40)
# best_39 <- glmulti(model, level = 1, crit = "aic", chunk = 39, chunks = 40)
best_40 <- glmulti(model, level = 1, crit = "aic", chunk = 40, chunks = 40)




answer_1 <- 
  consensus(
    list(best_1, best_2, best_3, best_4, best_5, best_6, best_7, best_8,
         best_9, best_10, best_11, best_12, best_13, best_14, best_15, best_16,
         best_17, best_18, best_19, best_20, best_21, best_22, best_23, best_24,
         best_25, best_26, best_27, best_28, best_29, best_30, best_31, best_32))

answer_2 <- 
  consensus(
    list(best_1, best_2, best_3, best_4, best_5, best_6, best_7, best_8,
         best_9, best_10, best_11, best_12, best_13, best_14, best_15, best_16,
         best_17, best_18, best_19, best_20, best_21, best_22, best_23, best_24,
         best_25, best_26, best_27, best_28, best_29, best_30, best_31, best_32))




glmulti(model, level = 1, crit = "aic", method = "d")



