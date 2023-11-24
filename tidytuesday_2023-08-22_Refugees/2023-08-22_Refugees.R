# My very first R dataviz exercise 🥳
# Inspired by the 2023-08-22 tidytuesday challenge: https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-08-22/readme.md
# Thanks to [@abichat](https://github.com/abichat) for the code review and some tips to help me improve the code.

library(tidyverse)
library(countrycode)

# Get data from tidytuesday repo
population <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

# Have a look at the data
population
glimpse(population)

# Vector to for regions ordering
regions <- c("The Americas",
             "Europe",
             "Middle East and North Africa",
             "East and Horn of Africa",
             "West and Central Africa",
             "Southern Africa",
             "Asia and the Pacific",
             "Region of origin")

# Add region of origin and asylum, using the countrycode package
modif_population <- 
  population %>% 
  mutate(roo = countrycode(sourcevar = coo_iso,
                           origin = "iso3c",
                           destination = "unhcr.region"),
         roa = countrycode(sourcevar = coa_iso,
                           origin = "iso3c",
                           destination = "unhcr.region"),
         roa_custom = roa,
         same_region_refugee = roo == roa) # boolean: does the refugees come from the same region or not?

modif_population$roa_custom[modif_population$roa == modif_population$roo] <- "Region of origin"

modif_population$roa_custom <- 
  factor(modif_population$roa_custom, 
         levels = regions)

modif_population$roo <- 
  factor(modif_population$roo, 
         levels = regions)

# Have a look at the modified data
modif_population
glimpse(modif_population)

# Set base GGPlot2 theme
theme_set(theme_light(base_size = 4) +
            theme(plot.title.position = "plot"))

# Plot data
modif_population %>% 
  filter(year == 2022, refugees != 0, !is.na(same_region_refugee)) %>% # only 2022 data, discard unknown countries
  select(year, coo_name, coo_iso, roo, coa_name, coa_iso, roa, roa_custom, same_region_refugee, refugees) %>% # remove unnecessary columns
  group_by(roo, roa_custom) %>%  # group by region of origin and same-region boolean
  summarise(total_refugees = sum(refugees)/1000) %>% # compute total number of refugees within the roo/same-region group
  ggplot() +
  aes(x = roo, y = total_refugees) + 
  geom_col(aes(fill = roa_custom)) + 
  labs(x = NULL,
       y = "Thousands of refugees",
       title = "Destination of refugees according to their region of origin in 2022") + 
  labs(fill = 'Asylum region')

# Save output image
ggsave("tidytuesday_2023-08-22_Refugees/2023-08-22_Refugees.png", width = 1500, height = 700, units = "px")

