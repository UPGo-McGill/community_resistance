############# HELPER FUNCTIONS ##################

# Install packages
library(tm.plugin.lexisnexis)
library(tm)
library(LexisNexisTools)
library(RCurl)
library(XML)
library(tidyverse)
library(stringr)
library(tm.plugin.factiva)
library(cancensus)
library(sf)
library(tidycensus)
library(purrr)
library(tigris)
options(tigris_use_cache = TRUE)
library(osmdata)
library(splitstackshape)
library(stats)
library(spacyr)
library(ggmap)
library(mapview)
library(tidytext)
library(parallel)
library(pbapply)
library(units)
library(RPostgres)
library(data.table)
library(lme4)
require(MASS)
require(car)
library(MuMIn)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tibble)
library(devtools)
devtools::install_github("UPGo-McGill/strr")
library(strr)
# Note: spaCy requires the user to download a version of miniconda and follow a set of instructions to set up

# Run Canadian census API key
# options(cancensus.api_key = "personal_key")
# options(cancensus.cache_path = "path")

# Run US census API key
# census_api_key("dab993e99dc7faf74295fc559dc2e1764b60e6b9", install = TRUE, overwrite = TRUE)

# Run Google Maps API Key
# register_google(key = "personal_key", write = TRUE)

# Function to replace NaN with 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

# Data loader that allows for in place renaming
loadRdata <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

## st_intersect_summarize helper function
st_intersect_summarize <- function(data, poly, group_vars, population, sum_vars,
                                   mean_vars) {
  
  pop <- enquo(population)
  
  data <- data %>% 
    mutate(CT_area = st_area(.))
  
  intersects <- suppressWarnings(st_intersection(data, poly)) %>%
    mutate(int_area_pct = st_area(.data$geometry) / .data$CT_area,
           population_int = !! pop * int_area_pct) %>%
    group_by(!!! group_vars)
  
  population <- intersects %>% 
    summarize(!! pop := sum(population_int, na.rm = TRUE))
  
  sums <- intersects %>%
    summarize_at(sum_vars, ~{sum(. * int_area_pct, na.rm = TRUE) /
        sum(population_int, na.rm = TRUE)})
  
  means <- intersects %>% 
    summarize_at(mean_vars, ~{
      sum(. * population_int, na.rm = TRUE) / sum(population_int, na.rm = TRUE)
    })
  
  suppressMessages(reduce(list(population,
                               st_drop_geometry(sums),
                               st_drop_geometry(means)),
                          full_join))
  
}

# Overdispersion function

overdisp_fun <- function(model) {
  
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  
  # extracts the Pearson residuals
  
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  
  # Generates a p-value. If less than 0.05, the data are overdispersed.
  
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
}

# Bivariate plotting

bivariate_mapping <- function(data, cityname, var1, var2, quantiles_var1 = NULL, quantiles_var2 = NULL,  
                              title, xlab, ylab) {
 
  
  # Set up mapping theme
  theme_map <- function(...) {
    theme_minimal() +
      theme(
        text = element_text(family = "sans",
                            color = "black"),
        # remove all axes
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        # add a subtle grid
        panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
        panel.grid.minor = element_blank(),
        # background colors
        plot.background = element_rect(fill = "white",
                                       color = NA),
        panel.background = element_rect(fill = "white",
                                        color = NA),
        legend.background = element_rect(fill = "white",
                                         color = NA),
        # borders and margins
        plot.margin = unit(c(.5, .5, .2, .5), "cm"),
        panel.border = element_blank(),
        panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
        # titles
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 9, hjust = 0,
                                   color = "black"),
        plot.title = element_text(size = 15, hjust = 0.5,
                                  color = "black"),
        plot.subtitle = element_text(size = 10, hjust = 0.5,
                                     color = "black",
                                     margin = margin(b = -0.1,
                                                     t = -0.1,
                                                     l = 2,
                                                     unit = "cm"),
                                     debug = F),
        # captions
        plot.caption = element_text(size = 7,
                                    hjust = .5,
                                    margin = margin(t = 0.2,
                                                    b = 0,
                                                    unit = "cm"),
                                    color = "#939184"),
        ...
      )
  }
  
  # Create a colour scale to encompass two variables
  bivariate_color_scale <- tibble(
    "3 - 3" = "#3F2949", # high var1, high var2
    "2 - 3" = "#435786",
    "1 - 3" = "#4885C1", # low var1, high var2
    "3 - 2" = "#77324C",
    "2 - 2" = "#806A8A", # medium var1, medium var2
    "1 - 2" = "#89A1C8",
    "3 - 1" = "#AE3A4E", # high var1, low var2
    "2 - 1" = "#BC7C8F",
    "1 - 1" = "#CABED0" # low var1, low var2
  ) %>%
    gather("group", "fill")
  
  # Calculate quantiles if not specified
 quantiles_var1 <- if(is.null(quantiles_var1)) {
   quantile(var1, probs = seq(0, 1, length.out = 4))} else {quantiles_var1}
 
 quantiles_var2 <- if(is.null(quantiles_var2)) {
   quantile(var2, probs = seq(0, 1, length.out = 4))} else {quantiles_var2}

    # Cut into groups defined above and join with fill
  data <- data %>% 
    mutate(
      var1_quantiles = cut(
        var1,
        breaks = quantiles_var1,
        include.lowest = TRUE
      ),
      var2_quantiles = cut(
        var2,
        breaks = quantiles_var2,
        include.lowest = TRUE
      ),
      group = paste(
        as.numeric(var1_quantiles), "-",
        as.numeric(var2_quantiles)
      )
    ) %>%
    left_join(bivariate_color_scale, by = "group")
  
  # Generate map
  map <- ggplot(data = data) +
    geom_sf(aes (
      fill = fill),
      colour = "white", 
      size = 0.1) + 
    scale_fill_identity() + 
    labs (x = NULL, 
          y = NULL, 
          title = title) +
    theme_map()
  
  # Separate the groups
  bivariate_color_scale <- bivariate_color_scale %>% 
    separate(group, into = c("var1", "var2"), sep = " - ") %>%
    mutate(var1 = as.integer(var1),
           var2 = as.integer(var2))
  
  # Generate legend
  legend <- ggplot() +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = var1,
        y = var2,
        fill = fill)
    ) +
    scale_fill_identity() +
    labs(x = xlab,
         y = ylab) +
    theme_map() +
    theme(
      axis.title = element_text(size = 6)
    ) +
    coord_fixed()
  
  # Plot
  ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.05, 0.075, 0.2, 0.2)
}

