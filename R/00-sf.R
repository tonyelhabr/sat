
library("tidyverse")
#  library("rlang")
library("teplot")
# Need to load {sf} in order to get its {dplyr} methods.
library("sf")

# https://schoolsdata2-tea-texas.opendata.arcgis.com/dataset
schools_raw_sf <-
  # file.path("data-raw", "unsd")  %>%
  file.path("data-raw", "Current_Schools")  %>%
  sf::read_sf() %>%
  janitor::clean_names()
# schools_raw_sf %>% select(distname, geometry) %>% plot()
# schools_raw_sf %>% count(fename)

schools_sf <-
  schools_raw_sf %>%
  mutate(
    district_num = district %>% as.integer(),
    campus_num = campus %>% as.integer(),
    city = city %>% str_to_lower(),
    county_num = county %>% as.integer(),
    county_name = str_replace_all(cntyname, "\\s+COUNTY", "") %>% str_to_lower(),
    region_num = region %>% as.integer(),
    grade_grp_num = gradegrp %>% as.integer()
  ) %>%
  select(
    district_num,
    district = distname,
    campus_num,
    campus = campname,
    city,
    county_num,
    county = county_name,
    region_num,
    grade_grp = graderange,
    grade_grp_num,
    instr_type,
    magnet,
    geometry
  )

counties_raw_sf <-
  # file.path("data-raw", "unsd")  %>%
  file.path("data-raw", "Counties")  %>%
  sf::read_sf() %>%
  janitor::clean_names()
# counties_raw_sf %>% select(fename, geometry) %>% plot()
# counties_raw_sf %>% count(fename)
# counties_raw_sf %>% count(campname)
# counties_raw_sf %>% count(name)
# counties_raw_sf %>% pull(cntyfips) %>% as.integer() %>% range()
counties_sf <-
  counties_raw_sf %>%
  mutate(county = fename %>% str_to_lower()) %>%
  select(county, geometry)

counties_bbox <-
  counties_sf %>%
  sf::st_bbox()

schools_sf %>%
  st_join(counties_sf)
schools_sf %>%
  anti_join(counties_sf %>% as.data.frame() %>% dplyr::select(-geometry))
counties_sf %>%
  filter(county %>% str_detect("dew"))

counties_raw_sf %>%
  filter(fid == 1) -> z
z
schools_sf %>%
  count(grade_grp, sort = TRUE)
schools_sf %>%
  count(grade_grp_num, sort = TRUE)
schools_sf %>%
  count(grade_grp, grade_grp_num, sort = TRUE) %>%
  filter(grade_grp %>% str_detect("^09"))

schools_sf %>%
  filter(grade_grp_num == 4L) %>%
  count(region_num)

ggplot() +
  geom_sf(data = counties_raw_sf, aes(fill = fid))

viz_regions <-
  ggplot() +
  geom_sf(data = counties_sf, fill = NA) +
  geom_sf(
    data =
      schools_sf %>%
      mutate_at(vars(region_num), funs(factor)) %>%
      filter(grade_grp_num == 4L), # %>%
      # sample_frac(0.1),
    # aes(color = magnet),
    # show.legend = "point"
    aes(color = region_num),
    show.legend = "point"
  ) +
  # scale_color_viridis_c() +
  scale_color_viridis_d() +
  guides(color = guide_legend(override.aes = list(size = 5))) +
  coord_sf(
    xlim = counties_bbox[c("xmin", "xmax")],
    ylim = counties_bbox[c("ymin", "ymax")],
    datum = NA
  ) +
  # theme_minimal()
  teplot::theme_map()
viz_regions
