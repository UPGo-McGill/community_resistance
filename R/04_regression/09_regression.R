######################################### REGRESSION MODELLING ###############################

source("R/01_import_general/01_helper_functions.R")

# Examine the probability density function
ggplot(data = airbnb_neighbourhoods, aes((CRI))) +
  stat_function(fun = dnorm, n = 580, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)

## Census variable choices
  # absolute values - need to account for population and number of households - i do not think this is a 
    # sensible choice.
  # percentages - a step up from absolute values as it takes into account population size and number of 
    # households. it also allows for comparison between neighbourhoods in different cities, though i still 
    # don't think this is the best measure. in NYC for example, it is likely more common that there are less 
    # white people and less owner-occupied households on average than in Vancouver due to general city make=up. 
    # Shouldn't this be taken into account?
  # z scored with city-wide average - i think this is best as it allows for consistent relativity. CRI is 
    # currently being measured in relation to the city-wide media coverage. Therefore i think it makes 
    # logical sense that z scores be used to capture intra-city variations, especially as we will be 
    # accounting for inter-city differences when using city as either dummy or grouping variable.

# Scaling
  # After doing some research online, it seems like general practice to scale the variables. Scaling  
  # is done by dividing the centered values of x by their standard deviations. Normalizes the distribution.
  # The log of the population takes the effect of a % increase in population, rather than absolute. 
  # Taking the log of the population, rather than scaling, reduces inter-city bariations betwen cities
  # and the significance of the population variable itself. As we want to control for population, 
  # this makes most sense?

############################################ LINEAR MODELLING #########################################
linear_model <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lm(scale(CRI) ~ 
      scale(active_listings) + 
      scale(revenue) + 
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
       data = .) %>% 
  summary()

# Residual error
airbnb_neighbourhoods_error <- airbnb_neighbourhoods %>% 
  filter(active_listings> 0) %>% 
  mutate(error = resid(linear_model))
 
ggplot(airbnb_neighbourhoods_error , aes(x = (CRI), y = (CRI - error))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

######################################### RANDOM SLOPE MODEL ########################################
random_slope <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue) + 
          scale(housing_loss_pct) + 
          scale(med_income_z)+ 
          log(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          (1 + scale(med_income_z) | city),
        data = .,
        REML = FALSE)

isSingular(random_slope)

# All random slopes are singular except for med_income_z
# Though the singular models are well defined, it may correspond to 
# overfitting. Remove terms that produce singular fitting.

# Despite individual variation in med_income_z, the values of the slope are quite
# simiar to eachother. There is consistency in how median income affects CRI.
# San Fran is the only city where lower incomes suggest higher CRI.
coef(random_slope)

# Create a null model to test for statistical significance.
random_slope_null <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue) + 
          scale(housing_loss_pct) + 
          # scale(med_income_z) +
          log(population) + 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          (1 + scale(med_income_z) | city),
        data = .,
        REML = FALSE)

anova(random_slope_null, random_slope)

# We fail to reject the null hypothesis.

######################################### RANDOM INTERCEPT MODEL #############################################
random_intercept <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
        scale(active_listings) +
        scale(revenue) + 
        scale(housing_loss_pct) + 
        scale(med_income_z)+ 
        log(population)+ 
        scale(housing_need_z) + 
        scale(non_mover_z) + 
        scale(owner_occupied_z) +
        (1   | city),
     data = .,
     REML = FALSE)

# The null model
random_intercept_null <-  airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
       # scale(active_listings) +
        scale(revenue) + 
        scale(housing_loss_pct) + 
        scale(med_income_z)+ 
        log(population)+ 
        scale(housing_need_z) + 
        scale(non_mover_z) + 
        scale(owner_occupied_z) +
          (1 | city),
        data = ., 
        REML = FALSE)

anova(random_intercept, random_intercept_null)

# Lets take an alpha value of 0.05 

  # active_listings
  # we cannot accept the null hypothesis

  # revenue
  # we cannot accept the null hypothesis

  # housing_loss_pct
  # we cannot accept the null hypothesis

  # med_income_z
  # we CAN accept the null hypothesis 

  # population  
  # we cannot accept the null hypothesis

  # housing_need_z
  # we cannot accept the null hypothesis

  # non_mover_z 
  # we cannot accept the null hypothesis

  # owner_occupier_z
  # we CAN accept the null hypothesis

# Final model - STATISTICALLY SIGNIFICANT
random_intercept <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue) + 
          scale(housing_loss_pct) + 
        #  scale(med_income_z) +
          log (population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
        #  scale(owner_occupied_z)+
          (1 | city),
        data = .,
        REML = FALSE)

# Residual plot using the random intercept model
airbnb_neighbourhoods_error <- airbnb_neighbourhoods %>% 
  filter(active_listings>0) %>% 
  mutate(error = resid(random_intercept))

ggplot(airbnb_neighbourhoods_error , aes(x = CRI, y = CRI - error), colour = city) +
  geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# Note high error. Neighbourhoods with the highest CRI
  # are also prone to the highest error. 





