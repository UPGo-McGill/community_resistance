######################################### REGRESSION MODELLING ###############################

source("R/01_helper_functions.R")

## Census variable choices
# absolute values - need to account for population and number of households - i do not think this is a 
# sensible choice.
# percentages - a step up from absolute values as it takes into account population size and number of 
# households. it also allows for comparison between neighbourhoods in different cities, though i still 
# don't think this is the best measure. in NYC for example, it is likely more common that there are less 
# white people and less owner-occupied households on average than in Vancouver due to general city make=up. 
# Shouldn't this be taken into account? I suppose, however, this would be accounted for in the spatial effects
# so this could definitely be ana verage.
# z scored with city-wide average - i think this is best as it allows for consistent relativity. 
# I think it makes logical sense that z scores be used to capture intra-city variations, especially as we will be 
# accounting for inter-city differences when using city as either dummy or grouping variable.

# Scaling
# After doing some research online, it seems like general practice to scale the variables. Scaling  
# is done by dividing the centered values of x by their standard deviations. Normalizes the distribution.
# The log of the population takes the effect of a % increase in population, rather than absolute. 
# Taking the log of the population, rather than scaling, reduces inter-city bariations betwen cities
# and the significance of the population variable itself. As we want to control for population, 
# this makes most sense?

# Examine the probability density function
ggplot(data = neighbourhoods_table, aes((CRI))) +
  stat_function(fun = dnorm, n = 580, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)

# Explore the data
neighbourhoods_table %>% 
  ggplot() +
  geom_point(aes(x = CRI, y = housing_loss_pct_households)) 

# Determine which probability density function best fits the data
# Manipulate CRI such that it is scaled and there are no 0s.
neighbourhoods_table[is.na(neighbourhoods_table)] <- 0
neighbourhoods_table$CRI <- ifelse(neighbourhoods_table$CRI < 0, 0.0000001, neighbourhoods_table$CRI)
neighbourhoods_table$CRI <- (neighbourhoods_table$CRI/max(neighbourhoods_table$CRI)) + 0.0000001

# Normal distribution
qqp(neighbourhoods_table$CRI, "norm")
  # not a good fit

# Lognormal distribution
qqp(neighbourhoods_table$CRI, "lnorm")
  # not a good fit

# Negative binomial distribution
  # only works for discrete values - does not apply

# Poisson distribution
  # only works for discrete values - does not apply

# Gamma distribution
gamma <- fitdistr(neighbourhoods_table$CRI, "gamma", lower=0.00001)
qqp(neighbourhoods_table$CRI, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])
  # clearly the best fit but has trouble capturing the larger CRIs

# NOTE: As the residuals are not normally distributed, we should not use linear modelling.

#################################### GUASS-HERMITE QUADRATURE ####################################
# GHQ is more accurate than laplace due to repeated iterations, but only works when there are
  # maximum 2-3 random effect (true in this case)

# Start with all variables
ghq <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  #filter(country == "United States") %>% 
  glmer((CRI) ~
            scale(active_listings) +
            scale(EH_pct) +
            scale(revenue_LTM_per_listing) + 
            scale(GH_pct) +
            scale(FREH_pct) +
           scale(ML_pct) +
           scale(PR_50_ct) +
           scale(revenue_10pct) +
           scale(active_listings_yoy) +
           scale(housing_loss_pct_listings) + 
           scale(population)+ 
           scale(med_income_z)+ 
           scale(low_income_z)+
           scale(university_education_z)+
           scale(housing_need_z) + 
           scale(non_mover_z) + 
           scale(owner_occupied_z) +
           scale(white_z) +
           scale(citizen_z) +
           scale(lone_parent_z) +
           scale(language_z)+
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

ghq %>% 
  summary()

ghq %>% 
  overdisp_fun()

# the data is overdispersed
  # more variability in the data than would be expected based on the model
# in the case of overdispersion, model it as a random effect with one random effect for each 
  # observation (ie. neighbourhood_name).
# OVERDISPERSION IS IRRELEVANT FOR MODELS THAT ESTIMATE A SCALE PARAMETER.

# Ultimately, GHQ is the model best suited for the data given its gamma distribution, 
  # continuous nature of CRI, minimum random effects, and increased iterations. 

# Examine the significance
ghq_null <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
         (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

anova(ghq, ghq_null)
# fail to accept the null hypothesis

# Test individual variables to see the affect on the r squared value
ghq_null <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
          scale(active_listings) +
          scale(EH_pct) +
          scale(revenue_LTM_per_listing) + 
          scale(GH_pct) +
          scale(FREH_pct) +
          scale(ML_pct) +
          scale(PR_50_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_listings) + 
          scale(population)+ 
          scale(med_income_z)+ 
          scale(low_income_z)+
          scale(university_education_z)+
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          scale(white_z) +
          scale(citizen_z) +
          scale(lone_parent_z) +
          #scale(language_z)+
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

# What variables lower the r squared when they are taken out?
  # should remain in the model
r.squaredGLMM(ghq_null)

# The variables that worsened the model have been taken out. 
  # However, active_listings, active_listings_yoy, and population
  # were supposed to be taken out but I think should remain in?

ghq <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
          # scale(active_listings) +
          scale(EH_pct) +
          scale(ML_pct) +
          scale(PR_50_ct) +
          scale(revenue_10pct) +
          # scale(active_listings_yoy) +
          # scale(population) + 
          scale(med_income_z)+ 
          scale(university_education_z)+
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(white_z) +
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

r.squaredGLMM(ghq)

# Now use ANOVA to compare the variables 
ghq <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
          scale(active_listings) +
          scale(EH_pct) +
          scale(revenue_LTM_per_listing) + 
          scale(GH_pct) +
          scale(FREH_pct) +
          scale(ML_pct) +
          scale(PR_50_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_listings) + 
          scale(population)+ 
          scale(med_income_z)+ 
          scale(low_income_z)+
          scale(university_education_z)+
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          scale(white_z) +
          scale(citizen_z) +
          scale(lone_parent_z) +
          scale(language_z)+
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

ghq_null <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
          scale(active_listings) +
          scale(EH_pct) +
          scale(revenue_LTM_per_listing) + 
          scale(GH_pct) +
          scale(FREH_pct) +
          scale(ML_pct) +
          scale(PR_50_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_listings) + 
          scale(population)+ 
          scale(med_income_z)+ 
          scale(low_income_z)+
          scale(university_education_z)+
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          scale(white_z) +
          scale(citizen_z) +
          scale(lone_parent_z) +
          #scale(language_z)+
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

anova(ghq, ghq_null)

# Updated model that takes out the variables that worsened the model
ghq <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
          scale(active_listings) +
          scale(EH_pct) +
          scale(GH_pct) +
          scale(PR_50_ct) +
          scale(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(white_z) +
          scale(citizen_z) +
          scale(lone_parent_z) +
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

ghq %>% 
  summary()

# note: PR_50 likely overlaps with ML_pct, revenue_10pct, and housing_loss variables
# either include the PR comprehensive variable, or FREH/GH/ML/housing_loss
ghq <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
          scale(active_listings) +
          scale(EH_pct) +
          scale(PR_50_ct) +
          scale(active_listings_yoy)+
          scale(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(white_z) +
          scale(citizen_z) +
          scale(lone_parent_z) +
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)
ghq %>% 
  summary()

r.squaredGLMM(ghq)

ghq <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  #filter(country == "United States") %>% 
  glmer((CRI) ~
         #scale(population) +
          scale(active_listings) +
          scale(housing_loss_pct_listings) +
          #scale(EH_pct) +
          scale(active_listings_yoy) +
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(white_z) +
          scale(citizen_z) +
          scale(lone_parent_z) +
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

ghq %>% 
  summary()

r.squaredGLMM(ghq)

# Interaction between variables -- UNSURE ABOUT THIS
ghq_interaction <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_50_ct) +
          scale(housing_loss_pct_households) +  
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(university_education_pct_pop_z)+
          scale(housing_need_pct_household_z) + 
          (1 | city), 
        family = Gamma(link = "log"),
        data = ., 
        nAGQ = 100)

ghq_interaction %>% 
  summary()

anova(ghq_interaction, ghq)

# ML_pct and PR_50_ct
  # significant interaction
  # increased AIC -- worsens the model

# Playing around with more interactions seems to either
  # worsen the model or cause it not to converge.

# CALCULATING R SQUARED
  # R2m referes to the marginal variance explained by fixed effects
  # R2c refers to the conditional variance explained by the entire model
  # delta works for all models - the only one that allows for comparison across model types
      # first order Taylor series expansion 
      # most flexible as it works for all kinds of distributions and link functions 
  # lognormal is limited to distributions with logarithmic link (such as this case)
  # trigamma is limited to distributions with logarithmic link (such as this case)
      # most accurate estimate of the observation level variance

r.squaredGLMM(ghq)

# Reported values using delta estimation - the only one that allows for direct comparison
  # ghq has the highest r squared 

# Evidently ghq_interaction is the best model, both theoretically and statistically.
  # Though a high R squred of 0.734, must report the lognormal and trigamma results as well

# Explore the model
plot(fitted(ghq), residuals(ghq), xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
lines(smooth.spline(fitted(ghq), residuals(ghq)))

# Residual plot using the ghq model
neighbourhoods_table_error <- neighbourhoods_table %>% 
  filter(active_listings>0) %>% 
  mutate(error = resid(ghq_interaction), 
         variance = error^2, 
         predicted = CRI - error)

ggplot(neighbourhoods_table_error , aes(x = CRI, y = error)) +
  geom_point() 

# Plot error 
neighbourhoods_table_error %>% 
  filter(city == "Toronto") %>% 
  dplyr::select(c("geometry", "error")) %>% 
  st_as_sf() %>% 
  mapview()





######################################### ARCHIVE/ROUGH WORK ################################################

########################################### 1 - LINEAR MODELLING #########################################
linear_model <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lm(scale(CRI) ~ 
       scale(active_listings) + 
       scale(revenue_LTM) + 
       scale(housing_loss_pct) + 
       scale(med_income_z)+ 
       log(population)+ 
       scale(housing_need_z) + 
       scale(white_z) +  
       scale(official_language_z) + 
       scale(university_education_z) + 
       scale(non_mover_z) + 
       scale(owner_occupied_z) +
       toronto + 
       vancouver + 
       nyc + 
       washington + 
       new_orleans + 
       miami + 
       los_angeles +
       san_francisco +
       usa, 
     data = .) 

class(linear_model)
names(linear_model)
confint(linear_model)

# Plot a histogram of the residuals to see if the deviation is normally distributed
hist(residuals(linear_model))
# As the distribution is normal, the normality assumption is likely to be true.
# Underlying assumptions are valid.

# Residual error
neighbourhoods_table_error <- neighbourhoods_table %>% 
  filter(active_listings> 0) %>% 
  mutate(error = residuals(linear_model))

ggplot(neighbourhoods_table_error , aes(x = (CRI), y = (CRI-error))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Plot residuals versus indepedent variables to see if a higher order term must be introduced.
# If linear, the model is fine.
ggplot(neighbourhoods_table_error , aes(x = (scale(university_education_z)), y = (error))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
# Higher order terms are not necessary for any indepedent variable.

# Remove insignificant terms
linear_model_reduced <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lm(scale(CRI) ~ 
       scale(active_listings) + 
       scale(revenue_LTM) + 
       scale(housing_loss_pct) + 
       scale(housing_need_z) + 
       scale(non_mover_z) + 
       toronto + 
       vancouver + 
       nyc + 
       washington + 
       new_orleans + 
       miami + 
       los_angeles +
       san_francisco +
       usa, 
     data = .)

anova(linear_model, linear_model_reduced)

# There is weak evidence of a statistically significant effect when reducing the model. 
neighbourhoods_table_error <- neighbourhoods_table %>% 
  filter(active_listings> 0) %>% 
  mutate(error = residuals(linear_model_reduced))

ggplot(neighbourhoods_table_error , aes(x = (CRI), y = (CRI-error))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Test the significance of the dummy variables
linear_model_reduced_2 <-  neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lm(scale(CRI) ~ 
       scale(active_listings) + 
       scale(revenue_LTM) + 
       scale(housing_loss_pct) + 
       scale(housing_need_z) + 
       scale(non_mover_z),
     data = .)

anova(linear_model_reduced_2, linear_model_reduced)
# There is strong evidence that removing the dummy variables is statistically significant.
# The dummy variables must be included. This implies that a mixed model may be beneficial, 
# with city as the grouping variable.

# Modeling Interactions
neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lm(scale(CRI) ~ 
       scale(active_listings) *
       scale(revenue_LTM) +
       scale(housing_loss_pct) +
       scale(housing_need_z) +
       scale(non_mover_z) +
       scale(revenue_LTM) * scale(housing_loss_pct) + 
       toronto + 
       vancouver + 
       nyc + 
       washington + 
       new_orleans + 
       miami + 
       los_angeles +
       san_francisco +
       usa,
     data = .) %>% 
  summary()
# revenue_LTM and active_listings,
# active_listings and housing_loss, (inverse relationship)
# revenue_LTM and housing_loss,
# revenue_LTM and non-mover, (inverse relationship)
# housing_loss and non-mover,
# housing_need and non_mover,
# are interactive variables when individually added to the model. 
# when all are added, only the interaction between active_listings and revenue_LTM, and
# revenue_LTM and housing_loss_pct are significant.
# this increases the r squared value, but also reduces the significance of
# housing_loss as a stand-alone variable

######################################## MIXED EFFECTS INTRO ######################################
null <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer(scale(CRI) ~ 1 + 
         (1 |city),
       data = ., 
       REML = FALSE) 
# 32% of variance is at the city level
# calculated by city variance / (city variance + residual variance)
# now, add fixed-effects predictors

######################################### RANDOM INTERCEPT MODEL #############################################
## QUESTION - Should I be using z values of the percent or absolute values?
# Z VALUES OF PCT 
random_intercept <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_pct_pop_z)+
          scale(university_education_pct_pop_z)+
          scale(population)+ 
          scale(housing_need_pct_household_z) + 
          scale(non_mover_pct_pop_z) + 
          scale(owner_occupied_pct_household_z) +
          scale(white_pct_pop_z) +
          scale(lone_parent_pct_families_z) +
          (1 | city),
        data = .,
        REML = FALSE)

# The null model
random_intercept_null <-  neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_pct_pop_z)+
          scale(university_education_pct_pop_z)+
          scale(population)+ 
          scale(housing_need_pct_household_z) + 
          scale(non_mover_pct_pop_z) + 
          scale(owner_occupied_pct_household_z) +
          scale(white_pct_pop_z) +
          # scale(lone_parent_pct_families_z) +
          (1 | city),
        data = .,
        REML = FALSE)

anova(random_intercept, random_intercept_null)

# Lets take an alpha value of 0.05 

# active_listings
# we cannot accept the null hypothesis

# revenue_LTM_per_listing
# we cannot accept the null hypothesis

# ML_pct
# we can accept the null hypothesis

# PR25_ct
# we can accept the null hypothesis

# revenue_10pct
# we can accept the null hypothesis

# active_listings_yoy
# we can accept the null hypothesis

# housing_loss_pct_households
# we cannot accept the null hypothesis

# med_income_z
# we can accept the null hypothesis 

# low_income_pct_pop_z
# we can accept the null hypothesis

# university_education_pct_pop_z
# we can accept the null hypothesis

# population  
# we can accept the null hypothesis

# housing_need_pct_households_z
# we can accept the null hypothesis

# non_mover_pct_pop_z 
# we can accept the null hypothesis

# owner_occupier_pct_household_z
# we can accept the null hypothesis

# white_pct_pop_z
# we can accept the null hypothesis

# lone_parent_pct_families_z
# we can accept the null hypothesis

# Final model 
random_intercept <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          # scale(ML_pct) +
          # scale(PR_25_ct) +
          # scale(revenue_10pct) +
          # scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          # scale(med_income_z)+ 
          # scale(low_income_pct_pop_z)+
          # scale(university_education_pct_pop_z)+
          # scale(population)+ 
          # scale(housing_need_pct_household_z) + 
          # scale(non_mover_pct_pop_z) + 
          # scale(owner_occupied_pct_household_z) +
          # scale(white_pct_pop_z) +
          # # scale(lone_parent_pct_families_z) +
          (1 | city),
        data = .,
        REML = FALSE)
# city explains 22.4% of variance.
# adding the other variables further explains the variance between cities.

# Residual plot using the random intercept model
neighbourhoods_table_error <- neighbourhoods_table %>% 
  filter(active_listings>0) %>% 
  mutate(error = resid(random_intercept))

ggplot(neighbourhoods_table_error , aes(x = CRI, y = CRI - error), colour = city) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Note high error. Neighbourhoods with the highest CRI are also prone to the highest error. 

# Z VALUES OF ABSOLUTE 
random_intercept <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_z)+
          scale(university_education_z)+
          scale(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          scale(white_z) +
          scale(lone_parent_z) +
          (1 | city),
        data = .,
        REML = FALSE)

# The null model
random_intercept_null <-  neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_z)+
          scale(university_education_z)+
          scale(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          scale(white_z) +
          # scale(lone_parent_z) +
          (1 | city),
        data = .,
        REML = FALSE)

anova(random_intercept, random_intercept_null)

# Lets take an alpha value of 0.05 

# active_listings
# we cannot accept the null hypothesis

# revenue_LTM_per_listing
# we cannot accept the null hypothesis

# ML_pct
# we can accept the null hypothesis

# PR25_ct
# we cannot accept the null hypothesis

# revenue_10pct
# we can accept the null hypothesis

# active_listings_yoy
# we can accept the null hypothesis

# housing_loss_pct_households
# we cannot accept the null hypothesis

# med_income_z
# we can accept the null hypothesis 

# low_income_pct_pop_z
# we can accept the null hypothesis

# university_education_pct_pop_z
# we can accept the null hypothesis

# population  
# we can accept the null hypothesis

# housing_need_pct_households_z
# we can accept the null hypothesis

# non_mover_pct_pop_z 
# we can accept the null hypothesis

# owner_occupier_pct_household_z
# we can accept the null hypothesis

# white_pct_pop_z
# we can accept the null hypothesis

# lone_parent_pct_families_z
# we can accept the null hypothesis

# Final model 
random_intercept <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) +
          # scale(ML_pct) +
          scale(PR_50_ct) +
          # scale(revenue_10pct) +
          # scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          # scale(med_income_z)+ 
          # scale(low_income_z)+
          # scale(university_education_z)+
          # scale(population)+ 
          # scale(housing_need_z) + 
          # scale(non_mover_z) + 
          # scale(owner_occupied_z) +
          # scale(white_z) +
          # scale(lone_parent_z) +
          (1 | city),
        data = .,
        REML = FALSE)
# city explains 25.1% of variance.
# adding the other variables further explains the variance between cities.

# Residual plot using the random intercept model
neighbourhoods_table_error <- neighbourhoods_table %>% 
  filter(active_listings>0) %>% 
  mutate(error = resid(random_intercept))

ggplot(neighbourhoods_table_error , aes(x = CRI, y = CRI - error), colour = city) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Note high error. Neighbourhoods with the highest CRI are also prone to the highest error. 

######################################### RANDOM SLOPE AND INTERCEPT MODEL ########################################

random_slope <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_pct_pop_z)+
          scale(university_education_pct_pop_z)+
          scale(population)+ 
          scale(housing_need_pct_household_z) + 
          scale(non_mover_pct_pop_z) + 
          scale(owner_occupied_pct_household_z) +
          scale(white_pct_pop_z) +
          scale(lone_parent_pct_families_z)  +
          (1 + scale(housing_need_pct_household_z)| city),
        data = .,
        REML = FALSE)

isSingular(random_slope)

# All random slopes are singular except for ML_pct, med_income_z, low_income_pct_pop_z,
# and housing_need_pct_household_z.
# Though the singular models are well defined, they may correspond to overfitting.

# Test the significance of the random slopes
random_slope_null <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_pct_pop_z)+
          scale(university_education_pct_pop_z)+
          scale(population)+ 
          scale(housing_need_pct_household_z) + 
          scale(non_mover_pct_pop_z) + 
          scale(owner_occupied_pct_household_z) +
          scale(white_pct_pop_z) +
          scale(lone_parent_pct_families_z)  +
          (1 | city),
        data = .,
        REML = FALSE)

anova(random_slope_null, random_slope)
# ML_pct - cannot accept the null hypothesis
# med_income_z - cannot accept the null hypothesis
# low_income_pct_pop_z - cannot accept the null hypothesis
# housing_need_pct_household_z - cannot accept the null hypothesis

# Can take ML_pct, med_income_z, low_income_pct_pop_z, and housing_need_pct_household_z as random slopes
random_slope <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_pct_pop_z)+
          scale(university_education_pct_pop_z)+
          scale(population)+ 
          scale(housing_need_pct_household_z) + 
          scale(non_mover_pct_pop_z) + 
          scale(owner_occupied_pct_household_z) +
          scale(white_pct_pop_z) +
          scale(lone_parent_pct_families_z)  +
          (1 + scale(housing_need_pct_household_z)| city),
        data = .,
        REML = FALSE)

coef(random_slope)

# ML_pct has a positive affect across all cities, though significantly more influential in Washington.
# med_income_z has a positive affect across all cities, though slightly more influential in Washington.
# low_income_pct_pop_z has both positive and negative effects...influences each city differently.
# housing_need_pct_household_z has both positive and negative effects

# NOTE: apparently you cannot treat continuous variables as random effects, therefore is this model invalid?

####################################### PENALIZED QUASILIKELIHOOD #####################################
# biased estimates for small means, binary responses, or if the response variable fits a discrete count distribution
# not the case

# find appropriate starting values
neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  lmer ((CRI) ~ 
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_pct_pop_z)+
          scale(university_education_pct_pop_z)+
          scale(population)+ 
          scale(housing_need_pct_household_z) + 
          scale(non_mover_pct_pop_z) + 
          scale(owner_occupied_pct_household_z) +
          scale(white_pct_pop_z) +
          scale(lone_parent_pct_families_z)+
          (1 | city),
        data = .,
        REML = FALSE) %>% 
  coef()

pql <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmmPQL((CRI) ~
            scale(active_listings) +
            scale(revenue_LTM_per_listing) + 
            scale(ML_pct) +
            scale(PR_25_ct) +
            scale(revenue_10pct) +
            scale(active_listings_yoy) +
            scale(housing_loss_pct_households) + 
            scale(med_income_z)+ 
            scale(low_income_pct_pop_z)+
            scale(university_education_pct_pop_z)+
            scale(population)+ 
            scale(housing_need_pct_household_z) + 
            scale(non_mover_pct_pop_z) + 
            scale(owner_occupied_pct_household_z) +
            scale(white_pct_pop_z) +
            scale(lone_parent_pct_families_z),
          ~ 1 | city, 
          family = Gamma(link = "log"),
          data = ., 
          verbose = FALSE,
          start = rep(0, 17))

pql %>% 
  summary()

####################################### LA PLACE APPROXIMATION #####################################
# less flexible than PQL, but is not biased in cases with small means 

laplace <- neighbourhoods_table %>% 
  filter(active_listings > 0) %>% 
  glmer((CRI) ~
          scale(active_listings) +
          scale(revenue_LTM_per_listing) + 
          scale(ML_pct) +
          scale(PR_25_ct) +
          scale(revenue_10pct) +
          scale(active_listings_yoy) +
          scale(housing_loss_pct_households) + 
          scale(med_income_z)+ 
          scale(low_income_pct_pop_z)+
          scale(university_education_pct_pop_z)+
          scale(population)+ 
          scale(housing_need_pct_household_z) + 
          scale(non_mover_pct_pop_z) + 
          scale(owner_occupied_pct_household_z) +
          scale(white_pct_pop_z) +
          scale(lone_parent_pct_families_z) +
          (1 | city), 
        family = Gamma(link = "log"),
        data = .)

laplace %>% 
  summary()

# convergence error. the current version of the lme4 package generates a lot of false positives.
# the authors of the package are set to increase the tolerance to 0.01 (from 0.001), in which case,
# the model would have converged.

laplace %>% 
  overdisp_fun()

# the data is overdispersed 
# more variability in the data than would be expected based on the model - as there is less than three
# random effects, we should use GHQ anyways.
# OVERDISPERSION IS IRRELEVANT FOR MODELS THAT ESTIMATE A SCALE PARAMETER.