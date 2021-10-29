# fig2.R
# Generate a histogram of the number of digitzed specimens from the Bahamas 
# published by the University of Florida per decade, grouped by taxonomy.

library(tidyverse)
library(rgbif)
library(ggimage)

# get data from GBIF
bahamasQuery <- occ_data(institutionCode = 'UF;FLAS', 
                         basisOfRecord = 'PRESERVED_SPECIMEN;FOSSIL_SPECIMEN',
                         country = 'BS', 
                         limit = 100000)  

# drop metadata
bahamasData <- bahamasQuery$data

# add decade and middle of decade columns for plotting
bahamasData <- bahamasData %>%
  mutate(decade = 10 * floor(year/10),
         decadeMiddle = decade + 5) 

# decide how to make taxonomic groupings
bahamasData %>% 
  group_by(phylum) %>% 
  summarize(n = n(),
            n_class = n_distinct(class))

# Chordata is the phylum with the most specimens and the most classes, so we
# separate it out into finer taxonomic groups
bahamasData <- bahamasData %>% 
  mutate(taxSimple = case_when(
    kingdom == 'Chromista' ~ 'Foraminifera',
    kingdom == 'Plantae' ~ 'Plants',
    phylum == 'Mollusca' ~ 'Mollusks',
    class == 'Actinopterygii' ~ 'Fish',
    class == 'Amphibia' ~ 'Herpetofauna',
    class == 'Ascidiacea' ~ 'other',
    class == 'Aves' ~ 'Birds',
    class == 'Elasmobranchii' ~ 'Fish',
    class == 'Holocephali' ~ 'other',
    class == 'Leptocardii' ~ 'other',
    class == 'Mammalia' ~ 'Mammals',
    class == 'Myxini' ~ 'other',
    class == 'Reptilia' ~ 'Herpetofauna',
    TRUE ~ 'other'))

# note that almost all specimens are categorized 
table(bahamasData$taxSimple)
sum(is.na(bahamasData$taxSimple))

# also note that almost all data is between decades 1930-2010
table(bahamasData$decade)

# filter out specimens that are not used in the figure
bahamasData <- bahamasData %>%
  filter(!is.na(taxSimple),
         taxSimple != 'other',
         decade >= 1930 & decade <= 2010)

# label taxonomic groups with the number of specimens per group
groupLabels <- 
  bahamasData %>%
  group_by(taxSimple) %>% 
  summarize(n = n()) %>% 
  mutate(label = str_c(taxSimple, ' N=', n)) %>%
  pull(label)

# renaming for nicer facet_wrap labels
bahamasData$basisOfRecord = ifelse(
  bahamasData$basisOfRecord == "PRESERVED_SPECIMEN",
  "Preserved specimen",
  "Fossil specimen")


# plot the number of fossil and preserved specimens published per decade,
# colored by taxonomic group
fig2 <- bahamasData %>% 
  ggplot(aes(x = decadeMiddle, fill = taxSimple)) + 
  geom_bar(width = 9) + 
  scale_fill_viridis_d(labels = groupLabels,
                       name = 'Taxonomic group') + 
  scale_x_continuous(breaks = seq(1930, 2020, by = 10),
                     limits = c(1930, 2020)) + 
  theme_classic() + 
  ylab('Number of specimens') + 
  xlab('Decade') + 
  facet_wrap(vars(basisOfRecord), 
             ncol = 2,
             strip.position = "bottom") + 
  theme(strip.background = element_blank())

ggsave(plot = fig2,
       file = 'fig2.png',
       device = 'png',
       width = 30,
       units = 'cm',
       dpi = 300)


