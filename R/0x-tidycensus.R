
library("tidyverse")
#  library("rlang")
library("teplot")
# Need to load {sf} in order to get its {dplyr} methods.
# library("sf")
library("tidycensus")

census_api_key("7380a49e1b5fca4d8782d49d5bb80be02025abd7", install = TRUE)
v1 <- load_variables(2016, "sf3", cache = TRUE)
# v2 <- load_variables(2016, "acs5", cache = TRUE)
View(v2)

acs_tx <-
  get_acs(
    geography = "county",
    variables = "B01003_001",
    state = "TX",
    geometry = TRUE,
    moe_level = 90,
    year = 2016
  )
acs_tx
viz_acs <-
  acs_tx %>%
  # mutate_at(vars(estimate), funs(log10)) %>%
  mutate_at(vars(estimate), funs(estimate / sum(estimate))) %>%
  # st_transform(crs = "+init=epsg:4326") %>%
  ggplot(aes(fill = estimate)) +
  geom_sf() +
  coord_sf(datum = NA) +
  scale_fill_viridis_c() +
  teplot::theme_map()
viz_acs
# library(albersusa)
