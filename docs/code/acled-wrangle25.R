#basic event data wrangling (ACLED)
#PLSC 501
#Spring 2025
#Dave Clark

# the goal here is to do some basic management of a large event data set. 

library(tidyverse)
library(ggplot2)
library(leaflet)
library(leaftime)
library(htmltools)
library(lubridate)
library(countrycode)
library(viridis)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)



rm(list=ls())

# read data, initial cleaning ----
# requires geocoded version of ACLED data
acled <- read.csv("/Users/dave/Documents/teaching/501/2024/exercises/ex5/acled03052024.csv")

# remove incomplete 2024 data
acled <- acled %>% filter(year<2024)

#check for and remove duplicate cases
acled <- acled %>% distinct()

#indicator vars for all values of sub_event_type ----
a2 <- acled %>% mutate(dummy=1) %>%
  spread(key=sub_event_type,value=dummy, fill=0) 

#cow codes by country name ----

a2 <- a2 %>% mutate(ccode=countrycode(country, origin='country.name', destination='cown'))

# only miss is Serbia - look at serbia then verify its ccode isn't assigned elsewhere
#view(a2%>%filter(country=="Serbia"))
#view(a2%>%filter(ccode=="345"))

#add ccode 345 to Serbia

a2$ccode[a2$country=="Serbia"] <- 345

# remove microstates/non-states for analysis data ----

a2 <- a2 %>% filter(is.na(ccode)==FALSE)

# evaluate ----
#count unique ccodes

length(unique(a2$ccode))

# extract year from event_date
         
a2$year <- year(a2$event_date)

# view unique years 

unique(a2$year)

# aggregate data by ccode and year, summing all event types ----

acledcy <- a2 %>%
  group_by(ccode, year) %>%
  summarise(across(`Abduction/forced disappearance`:`Violent demonstration`, sum),
            country = first(country))%>%
            ungroup()
            
# viz year coverage - bar chart of years covered over country ----

acledcy %>%
  group_by(country) %>%
  summarise(n = n_distinct(year)) %>%
  ggplot(aes(x = country, y = n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Years Covered by Country", x = "Country", y = "Years Covered") +
  theme_minimal()


# data viz - maps ----
# am going to model sexual violence. Let's look at the distribution of sexual violence
## event location map by geocodes ----

# acled <- acledG %>% filter(year<2024)
# 
# #indicator vars for all values of sub_event_type
# aG <- acledG %>% mutate(dummy=1) %>%
#   spread(key=sub_event_type,value=dummy, fill=0)%>%
#   mutate(dummy=1) %>%
#   spread(key=event_type, value=dummy, fill=0)

sv <- a2 %>% filter(`Sexual violence`>0)
sv$sv <- "sv$`Sexual violence`"

leaflet(sv) %>% setView(lng = 10.3074, lat = 20.46, zoom = 2) %>%
  addTiles() %>%  addMarkers(lat = ~latitude, lng = ~longitude, popup = ~notes,
                             clusterOptions = markerClusterOptions(singleMarkerMode=FALSE,popup = ~as.character(event_date), label = ~as.character(location))
  )

## Chloropleth map, static ----
# data frame for mapping sexual violence events by country
# one observation per country with total number of sexual violence events

sexual_violence_data <- acledcy %>% dplyr::select(ccode, year, `Sexual violence`)

# sum of sexual violence events by ccode

map_data <- sexual_violence_data %>%
  group_by(ccode) %>%
  summarise(count = sum(`Sexual violence`)) %>%
  ungroup()

# add iso codes for joining to map polygon data
map_data <- map_data %>% 
  mutate(
    iso2=countrycode(ccode, origin='cown', destination='iso2c'),
    iso3 = countrycode(ccode, origin = 'cown', destination = 'iso3c')
  )

# viz sexual violence events by country
plot(density(map_data$count))

# create ln(count) for better visualization
map_data$lncount <- log(map_data$count + 1)
plot(density(map_data$lncount))


# Get world map data from rnaturalearth 
world <- ne_countries(scale = "medium", returnclass = "sf")

# join sexual violence data by iso to the world map data
world_data <- left_join(world, 
                        map_data %>% dplyr::select(iso2, count, lncount), 
                        by = c("iso_a2" = "iso2"))

# Remove Antarctica for better use of space
world_data <- world_data %>% 
  filter(name != "Antarctica")

# Create the map with ggplot2
ggplot(data = world_data) +
  geom_sf(aes(fill = count)) +
  geom_sf_text(aes(label = iso_a3), 
               size = 2,           # Adjust text size
               color = "white",    # Text color
               fontface = "bold",  # Make text bold
               check_overlap = TRUE) +  # Prevents overlapping labels
  scale_fill_viridis_c(
    option = "viridis", 
    name = "Sexual Violence\nEvents",
    na.value = "gray90",
    guide = guide_colorbar(
      barwidth = 1,
      barheight = 15,
      title.position = "top"
    )
  ) +
  coord_sf(expand = FALSE) +  # Fill the plot area completely
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 20)),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),    # Remove axis text
    axis.ticks = element_blank(),   # Remove axis ticks
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(title = "Sexual Violence Events by Country")

##  Chloropleth map, dynamic using leaflet ----

# Convert sf object to dataframe with coordinates for leaflet
world_data_leaflet <- st_transform(world_data, 4326)

# Create color palette based on the count values
pal <- colorNumeric(
  palette = "magma",
  domain = world_data_leaflet$lncount,
  na.color = "transparent"
)

# Create leaflet map
leaflet(world_data_leaflet) %>%
  setView(lng = 0, lat = 30, zoom = 2) %>%  # Center the map
  addPolygons(
    fillColor = ~pal(lncount),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
      label = ~paste0(name, ": ", count, " events"),
      labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addLegend(
    pal = pal,
    values = ~lncount,
    opacity = 0.7,
    title = "Sexual Violence Events",
    position = "bottomright"
  )



