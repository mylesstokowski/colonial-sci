# fig1c.R

library(tidyverse)
library(rgbif)

uwiQuery <- occ_data(institutionCode = 'uwizm', 
                     country = 'TT',
                     limit = 100000)

# drop metadata
uwiData <- uwiQuery$data

# add decade and middle of decade columns for plotting
uwiData <- uwiData %>%
  mutate(decade = 10 * floor(year/10),
         decadeMiddle = decade + 5) 

# decide how to make taxonomic groups
table(uwiData$phylum)
table(filter(uwiData, phylum == 'Chordata')$class)
table(filter(uwiData, phylum == 'Arthropoda')$class)

# create taxonomic groups
uwiData <- uwiData %>%
  mutate(taxSimple = case_when(
    phylum == 'Arthropoda' ~ 'Arthropods',
    phylum == 'Mollusca' ~ 'Mollusks',
    class == 'Actinopterygii' ~ 'Fish',
    class == 'Amphibia' ~ 'Amphibians',
    class == 'Reptilia' ~ 'Reptiles',
    class == 'Mammalia' ~ 'Mammals',
    class == 'Aves' ~ 'Birds',
    TRUE ~ 'other'
  ))

# most data is categorized in the new simple taxonomic groups
table(uwiData$taxSimple)
sum(is.na(uwiData$taxSimple))
uwiData %>% filter(taxSimple == 'other') %>% pull(phylum) %>% table()

# plot number of specimens of each taxonomic group published by decade
uwiData %>% 
  filter(taxSimple != 'other') %>%
  ggplot(aes(x = decadeMiddle, fill = taxSimple)) + 
  geom_bar() + 
  scale_fill_viridis_d() + 
  scale_x_continuous(breaks = seq(1890, 2010, by = 10),
                     limits = c(1885, 2015)) + 
  theme_classic() + 
  guides(fill=guide_legend(title="Taxonomic group")) + 
  ylab('Number of specimens') + 
  xlab('Decade')