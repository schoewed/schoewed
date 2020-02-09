library(here)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_class = "sf")
library(tidycensus)
library(leaflet)
library(mapview)
library(tidyr)
library(viridis)

census_api_key("b5e98267689c959f7a08d33552ff8e2e0a0e797c", overwrite="TRUE", install = TRUE)
acs17 <- load_variables(year = 2017, dataset = "acs5", cache = TRUE)
View(acs17)



#grab variables
seasonal_housing <- c(seasonal = "B25004_006",
                      tot_own = "B07013_002")
counties <- get_acs(geography = "county", year = 2017, variables = seasonal_housing, shift_geo = , geometry = TRUE, output = "wide")
head(counties)

#create % seasonal variable
counties2 <- counties %>% 
  select(-seasonalM, -tot_ownM) %>% 
  mutate(szn_share = seasonalE/(seasonalE + tot_ownE))

#plot it static
ggplot(counties2) +
  geom_sf(aes(fill=szn_share), color = NA) +
  theme_minimal() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(direction = -1) +
  labs(title="Seasonal Housing", caption="Source: US Census 2017", fill = "Est. % Seasonal")

pal <- colorNumeric(palette = "viridis", 
                    domain = counties2$szn_share)

#define pop-up
pop1 <- paste0("<strong>County: </strong>",
               str_extract(counties2$NAME, "^([^,]*)"),
               "<br><strong>2017 Estimate: </strong>",
               round(counties2$szn_share*100, digits = 2),
               "%")

#interactive map to export as webpage
counties2 %>% 
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = pop1,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(szn_share)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ szn_share,
            title = "Seasonal Housing",
            labFormat = labelFormat(suffix = "%",
                                    transform = function(x) {100*x}),
            opacity = 2)

#doesn't work!?
library(htmlwidgets)
saveWidget(counties2, file="UStest.html")

save(counties2, file = here("US Counties.Rdata"))
load(here("US Counties.Rdata"))
