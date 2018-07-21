
paths_load <-
  list.files(
    "data",
    pattern = "*RData",
    recursive = FALSE,
    full.names = TRUE
  )
sapply(paths_load, load, .GlobalEnv)

library("tidyverse")
schools_tea_all <-
  urls_tea %>%
  import_tea_data_cleanly()
schools_tea_all

rgx_subtest <- "math|reading|writing|english|science|total"
schools_tea_all %>%
  drop_na(total) %>%
  # filter(group == "All Students") %>%
  mutate_at(vars(matches(rgx_subtest)), funs(if_else(is.na(.), -1L, 1L))) %>%
  group_by_at(vars(matches(paste0("test|", rgx_subtest)))) %>%
  count(sort = TRUE)

schools_tea_all %>%
  # filter(group == "All Students") %>%
  count(is.na(total), sort = TRUE)
schools_tea_all %>% drop_na(total)

scores_byyear <-
  schools_tea_all %>%
  gather(subtest, value, matches(rgx_subtest)) %>%
  filter(!is.na(value)) %>%
  filter(subtest != "total") %>%
  group_by(test, subtest, year) %>%
  summarise_at(vars(value), funs(mean)) %>%
  ungroup()
scores_byyear_sat <-
  scores_byyear %>%
  filter(test == "SAT")

scores_byyear_sat %>%
  ggplot(aes(x = subtest, y = value, size = value, color = subtest)) +
  scale_color_hue(l = 45) +
  geom_point() +
  labs(title = "Year: {frame_time}") +
  gganimate::transition_time(year) +
  gganimate::ease_aes("linear")

scores_bycounty <-
  tx_counties %>%
  left_join(
    schools_tea_all %>%
      gather(subtest, value, matches(rgx_subtest)) %>%
      filter(!is.na(value)) %>%
      filter(subtest != "total") %>%
      group_by(test, subtest, year, county) %>%
      summarise_at(vars(value), funs(mean)) %>%
      ungroup() %>%
      mutate_at(vars(county), funs(tolower)) %>%
      mutate_at(vars(county), funs(if_else(. == "dewitt", "de witt", .))),
    by = c("subregion" = "county")
  )

scores_bycounty_sat <-
  scores_bycounty %>%
  filter(test == "SAT")

# library("gganimate")
library("teplot")
viz <-
  scores_bycounty_sat %>%
  group_by(test, subtest, year) %>%
  summarise_at(vars(long, lat, value), funs(mean)) %>%
  ungroup() %>%
  # filter(subtest == "math") %>%
  ggplot() +
  geom_point(aes(x = long, y = lat, size = value, fill = value), shape = 21) +
  # scale_fill_viridis_c(option = "B", na.value = "#FFFFFF") +
  # gganimate::transition_time(year) +
  facet_wrap( ~ subtest)
viz

viz_schools_sat_byyear <-
  ggplot(scores_bycounty_sat) +
  # ggplot(scores_bycounty_sat, aes(x = long, y = lat, group = group, fill = value)) +
  geom_polygon(
    data = tx_border,
    aes(x = long, y = lat, group = group),
    size = 1.5,
    color = "black",
    fill = NA
  ) +
  geom_polygon() +
  # geom_polygon(
  #   aes(x = long, y = lat, group = group, fill = value),
  # ) +
  # transition_time(year) +
  # transition_states(subtest, transition_length = 0.5, state_length = 1) +
  # ease_aes("linear") +
  facet_wrap(~subtest) +
  scale_fill_viridis_c(option = "B", na.value = "#FFFFFF") +
  teplot::theme_map(legend.position = "bottom")
viz_schools_sat_byyear

library(gapminder)
gapminder
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')
