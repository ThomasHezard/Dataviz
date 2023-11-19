# My very first R dataviz exercise 🥳
# Inspired by the 2023-08-22 tidytuesday challenge: https://github.com/rfordatascience/tidytuesday/blob/master/data/2023/2023-08-22/readme.md


if (!require("tidyverse")) install.packages("tidyverse")
if (!require("countrycode")) install.packages("countrycode")

library(tidyverse)
library(countrycode)

# Get data from tinytuesday repo
population <- as_tibble(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv'))

# Have a look at the data
population
glimpse(population)

# Add region of origin and asylum, using the countrycode package
modif_population <- population %>% 
  mutate(roo = countrycode(sourcevar = coo_iso,
                              origin = "iso3c",
                              destination = "region")) %>% 
  mutate(roa = countrycode(sourcevar = coa_iso,
                           origin = "iso3c",
                           destination = "region")) %>% 
  mutate(same_region_refugee = roo == roa) # boolean: does the refugees come from the same region or not?

# Have a look at the modified data
modif_population
glimpse(modif_population)

# Set base GGPlot2 theme
theme_set(theme_light(base_size = 4) +
            theme(plot.title.position = "plot"))

# Plot data
summary <- modif_population %>% 
  filter(year == 2022, refugees != 0, !is.na(same_region_refugee)) %>% # only 2022 data, discard unknown countries
  select(year, coo_name, coo_iso, roo, coa_name, coa_iso, roa, same_region_refugee, refugees) %>% # remove unnecessary columns
  group_by(roo, same_region_refugee) %>%  # group by region of origin and same-region boolean
  summarise(total_refugees = sum(refugees)/1000) %>% # compute total number of refugees within the roo/same-region group
  group_by(roo) %>%  # group by roo
  ggplot() +
  aes(x = roo, y = total_refugees) + 
  geom_col(aes(fill = same_region_refugee)) + 
  labs(x = NULL,
       y = "Thousands of refugees",
       title = "Destination of refugees in 2022") + 
  labs(fill='Region of migration') +
  scale_fill_discrete(labels=c('Other region', 'Region of origin')) # legend title

# Save output image
ggsave("tinytuesday_2023-08-22_Refugees/2023-08-22_Refugees.png", width = 1500, height = 700, units = "px")
