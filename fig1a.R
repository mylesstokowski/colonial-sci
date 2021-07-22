# fig1a.R

library(tidyverse)
library(rgbif)
library(sf)
library(rnaturalearth)

# split query into two based on kingdom to get around max query size
ttQuery1 <- occ_data(
  country = 'TT',
  basisOfRecord = 'PRESERVED_SPECIMEN;FOSSIL_SPECIMEN',
  kingdomKey = '0;1;2',
  limit = 100000)

ttQuery2 <- occ_data(
  country = 'TT',
  basisOfRecord = 'PRESERVED_SPECIMEN;FOSSIL_SPECIMEN',
  kingdomKey = '3;4;5;6;7',
  limit = 100000)

# combine data from separate queries
ttData <- bind_rows(ttQuery1$data, ttQuery2$data)

# return a tibble row with the publishingOrgKey plus the organization name,
# and the latitude and longitude if available
publishingOrgKeyToName <- function(publishingOrgKey) {
  
  results <- rgbif::organizations(uuid=publishingOrgKey)$data
  
  # what data was available
  names <- names(results)
  
  tibble(publishingOrgKey = publishingOrgKey, 
         title = results$title,
         lat = ifelse('latitude' %in% names, results$latitude, NA),
         long = ifelse('longitude' %in% names, results$longitude, NA))
}

publishingOrgKeys <- unique(ttData$publishingOrgKey)

# get name, lat and long for each publishing organization
orgTibble <- bind_rows(lapply(publishingOrgKeys, publishingOrgKeyToName))

# get number of specimens published by each organization
specimensPerPublishingOrgKey <- ttData %>%
  group_by(publishingOrgKey) %>% 
  summarize(nSpecimens = n())

orgTibble <- inner_join(orgTibble, specimensPerPublishingOrgKey)

# write csv and manually edit it, adding coordinates of organizations
# where GBIF query did not return latitude and longitude
# write_csv(orgTibble, 'orgTibble.csv')

# then read back in the edited file
orgTibbleEdited <- read_csv('orgTibbleEdited.csv')

# convert to sf 
orgTibbleSf <- orgTibbleEdited %>% 
  filter(!is.na(lat), !is.na(long)) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = 4236, agr = 'constant')

# get world basemap
world <- ne_countries(scale = "medium", returnclass = "sf")

# map number of specimens published per institution
ggplot(data = world) +
  geom_sf(lwd = 0) + 
  geom_sf(data = orgTibbleSf, aes(size = nSpecimens), shape = 21, 
          color = 'black', fill = 'black', alpha = 0.3) +
  coord_sf(ylim = c(-59, 70)) +
  theme_void() + 
  scale_size_continuous(name = 'Specimens per \ninstitution',
                        range = c(1, 14))
