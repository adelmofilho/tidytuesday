# Tidytuesday
# Week 2 - 2021

# Load operators and libraries
library(dplyr)
library(ggplot2)
library(ggExtra)
library(xkcd)

# Custom functions
to_numeric = function(x, na_strings=NA){
  
  x[x %in% na_strings] = NA_real_
  x = as.numeric(x)
  return(x) 
  
}

# Load Data and Metadata
tuesdata <- tidytuesdayR::tt_load('2021-01-05')

# Take a look at data
transit_cost <- tuesdata$transit_cost
transit_cost

# Visualization 

p1 <- transit_cost %>%
  mutate(end_year = to_numeric(end_year, na_strings = c(NA, "X"))) %>% 
  mutate(start_year = to_numeric(start_year, 
                                 na_strings = c(NA, "not start", "4 years","5 years"))) %>% 
  mutate(delta_time = end_year - start_year) %>% 
  ggplot(aes(x=delta_time, y=log(cost_km_millions, base=10))) + 
  geom_point() + 
  labs(x="Tunnel construction time (years)",
       y="Log of Cost/km in millions of USD",
       title="Relationship between transit cost and construction time") +
  theme(plot.title = element_text(hjust = 0.5))
  

ggMarginal(p1, type = "histogram")
