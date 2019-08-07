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
       data = .) 

class(linear_model)
names(linear_model)
confint(linear_model)

# Plot a histogram of the residuals to see if the deviation is normally distributed
hist(residuals(linear_model))
  # As the distribution is normal, the normality assumption is likely to be true.
  # Underlying assumptions are valid.

# Residual error
airbnb_neighbourhoods_error <- airbnb_neighbourhoods %>% 
  filter(active_listings> 0) %>% 
  mutate(error = residuals(linear_model))
 
ggplot(airbnb_neighbourhoods_error , aes(x = (CRI), y = (CRI-error))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Plot residuals versus indepedent variables to see if a higher order term must be introduced.
  # If linear, the model is fine.
ggplot(airbnb_neighbourhoods_error , aes(x = (scale(university_education_z)), y = (error))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
  # Higher order terms are not necessary for any indepedent variable.

# Remove insignificant terms
linear_model_reduced <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lm(scale(CRI) ~ 
       scale(active_listings) + 
       scale(revenue) + 
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
airbnb_neighbourhoods_error <- airbnb_neighbourhoods %>% 
  filter(active_listings> 0) %>% 
  mutate(error = residuals(linear_model_reduced))

ggplot(airbnb_neighbourhoods_error , aes(x = (CRI), y = (CRI-error))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Test the significance of the dummy variables
linear_model_reduced_2 <-  airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lm(scale(CRI) ~ 
       scale(active_listings) + 
       scale(revenue) + 
       scale(housing_loss_pct) + 
       scale(housing_need_z) + 
       scale(non_mover_z),
     data = .)

anova(linear_model_reduced_2, linear_model_reduced)
# There is strong evidence that removing the dummy variables is statistically significant.
# The dummy variables must be included. This implies that a mixed model may be beneficial, 
# with city as the grouping variable.

# Modeling Interactions
airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lm(scale(CRI) ~ 
       scale(active_listings) *
       scale(revenue) +
       scale(housing_loss_pct) +
       scale(housing_need_z) +
       scale(non_mover_z) +
       scale(revenue) * scale(housing_loss_pct) + 
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
  # revenue and active_listings,
  # active_listings and housing_loss, (inverse relationship)
  # revenue and housing_loss,
  # revenue and non-mover, (inverse relationship)
  # housing_loss and non-mover,
  # housing_need and non_mover,
      # are interactive variables when individually added to the model. 
      # when all are added, only the interaction between active_listings and revenue, and
      # revenue and housing_loss_pct are significant.
  # this increases the r squared value, but also reduces the significance of
  # housing_loss as a stand-alone variable


######################################## MIXED EFFECTS INTRO ######################################
null <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer(scale(CRI) ~ 1 + 
         (1 |city),
       data = ., 
       REML = FALSE) 
# 8.1% of variance is at the city level
# now, add fixed-effects predictors

######################################### RANDOM INTERCEPT MODEL #############################################
random_intercept <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue) + 
          scale(housing_loss_pct) + 
          scale(med_income_z)+ 
          scale(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          (1 | city),
        data = .,
        REML = FALSE)

# The null model
random_intercept_null <-  airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue) + 
          scale(housing_loss_pct) + 
          scale(med_income_z)+ 
          scale(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          # scale(owner_occupied_z) +
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
          scale(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          #  scale(owner_occupied_z)+
          (1 | city),
        data = .,
        REML = FALSE)
# city explains 6.9% of variance.
# adding the other variables further explains the variance between cities.

# Residual plot using the random intercept model
airbnb_neighbourhoods_error <- airbnb_neighbourhoods %>% 
  filter(active_listings>0) %>% 
  mutate(error = resid(random_intercept))

ggplot(airbnb_neighbourhoods_error , aes(x = CRI, y = CRI - error), colour = city) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Note high error. Neighbourhoods with the highest CRI are also prone to the highest error. 

######################################### RANDOM SLOPE MODEL ########################################

random_slope <- airbnb_neighbourhoods %>% 
  filter(active_listings > 0) %>% 
  lmer (scale(CRI) ~ 
          scale(active_listings) +
          scale(revenue) + 
          scale(housing_loss_pct) + 
          scale(med_income_z)+ 
          scale(population)+ 
          scale(housing_need_z) + 
          scale(non_mover_z) + 
          scale(owner_occupied_z) +
          (scale(owner_occupied_z) | city),
        data = .,
        REML = FALSE)

isSingular(random_slope)

# All random slopes are singular except for revenue, housing_loss_pct, med_income_z, 
    # population, and non_mover_z.
# Though the singular models are well defined, they may correspond to overfitting.

# Test the significance of the random slopes

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






