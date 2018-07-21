
# Reference: http://jschoeley.github.io/2018/06/30/bubble-grid-map.html
library(eurostat)  # eurostat data
library(tidyverse) # tidy data transformation
library(lubridate) # date and time support
library(sf)        # simple features GIS

# download eurostat data of population counts by NUTS-3 region
euro_pop <-
  get_eurostat('demo_r_pjanaggr3', stringsAsFactors = FALSE) %>%
  filter(sex == 'T',
         str_length(geo) == 5, # NUTS-3
         age == 'TOTAL')
euro_pop

# download geospatial data for NUTS-3 regions
eu_nuts3_sf <-
  get_eurostat_geospatial(output_class = 'sf',
                          resolution = '60', nuts_level = 3)

# divide the european continent into a 150 by 150 cell grid
euro_grid <-
  st_make_grid(eu_nuts3_sf %>% filter(LEVL_CODE == 3), n = 150)
eu_nuts3_sf %>% class()
euro_grid %>% class()
euro_grid %>% plot()
eu_nuts3_sf %>% plot()

euro_data_viz <-
  eu_nuts3_sf %>%
  # join population counts from 2017 with geodata
  left_join(y = filter(euro_pop, year(time) == 2017),
            by = c('id' = 'geo')) %>%
  select(values) %>%
  # calculate average population in each grid cell while preserving
  # the observed total (extensive = TRUE)
  st_interpolate_aw(to = euro_grid, extensive = TRUE) %>%
  # return centroid coordinates for each grid cell
  st_centroid() %>%
  cbind(st_coordinates(.)) %>%
  # arrange by value to plot lowest values first so that
  # larger bubbles sit on top of smaller ones
  arrange(values)

viz_euro <-
  euro_data_viz %>%
  ggplot() +
  geom_point(aes(x = X, y = Y, size = values),
             shape = 21, color = 'white', fill = 'black', show.legend = FALSE) +
  coord_map(ylim = c(30, 73), xlim = c(-15, 45),
            # projection = 'albers',
            # lat0 = 50, lat1 = 51
            ) +
  scale_size_area(max_size = 6) +
  theme_void()
viz_euro
