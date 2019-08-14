######################################### MAPPING ##############################################

source("R/01_import_general/01_helper_functions.R")

# Ensure that the CRI is scaled accordingly
airbnb_neighbourhoods <- airbnb_neighbourhoods %>% 
  mutate(CRI = CRI / max(CRI)) %>% 
  mutate(CRI_scale = scale(CRI))

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


################################### BIVARIATE MAPPING #####################################
  # visualizing two variables at the same time using a bivariate colour scale
cityname <- "Montreal"
quantiles_CRI <- c(0, 0.01, 0.1, 1)

bivariate_mapping(cityname, quantiles_CRI) %>% 
  plot()
