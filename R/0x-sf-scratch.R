
library("tidyverse")
#  library("rlang")
library("teplot")
# Need to load {sf} in order to get its {dplyr} methods.
library("sf")

pop_sf <-
  file.path("data-raw", "tl_2013_48_tract") %>%
  sf::read_sf() %>%
  janitor::clean_names()
pop_sf
# pop_sf %>%
#   select(name) %>%
#   plot()
pop_sf %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  count(name) %>%
  arrange(desc(name))
hwys_sf <-
  file.path("data-raw", "TxDOT_Memorial_Highways") %>%
  sf::read_sf() %>%
  janitor::clean_names()
hwys_sf

hwys_n_filt <-
  hwys_sf %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  filter(rte_prfx == "US") %>%
  count(memorial_h, sort = TRUE)
hwys_n_filt
hwys_sf %>%
  inner_join(hwys_n_filt) %>%
  filter(n > 10) %>%
  select(memorial_h) %>%
  plot()
hwys_sf %>%
  semi_join(hwys_n_filt) %>%
  select(rte_prfx) %>%
  plot()

data_tx <- teplot::get_map_data_state(state = "tx")
viz1 <-
  ggplot() +
  geom_polygon(
    data = data_tx,
    ggplot2::aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA
  ) +
  geom_sf(
    data =
      hwys_sf %>%
      inner_join(hwys_n_filt) %>%
      select(rte_prfx, n),
    # aes(size = n)
    size = 1,
    fill = "black"
  ) +
  coord_sf(datum = NA) +
  teplot::theme_te() +
  theme(panel.grid.major = element_blank())
  # ggthemes::theme_map()
viz1
viz1$themes$panel.grid.major
viz1$themes$panel.grid.minor
