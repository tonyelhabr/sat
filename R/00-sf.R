

library("tidyverse")
#  library("rlang")
library("teplot")
# Need to load {sf} in order to get its {dplyr} methods.
library("sf")

schools_tea <-
  teproj::import_ext_csv(
    schools_tea,
    dir = "data"
  )

# https://schoolsdata2-tea-texas.opendata.arcgis.com/dataset
schools_raw_sf <-
  # file.path("data-raw", "unsd")  %>%
  file.path("data-raw", "Current_Schools") %>%
    sf::read_sf() %>%
    janitor::clean_names()
# schools_raw_sf %>% select(distname, geometry) %>% plot()
# schools_raw_sf %>% count(fename)

schools_sf <-
  schools_raw_sf %>%
  mutate_at(vars(district, campus, county, region, gradegrp), funs(as.integer)) %>%
  rename(
    district_num = district,
    school_num = campus,
    county_num = county,
    region_num = region,
    grade_grp_num = gradegrp
  ) %>%
  rename(
    school = campname,
    district = distname,
    county = cntyname
  ) %>%
  mutate_at(vars(school, district, county, city), funs(toupper)) %>%
  mutate_at(vars(county), funs(str_remove_all(., "\\s+COUNTY"))) %>%
  mutate_at(vars(school), funs(str_remove_all(., "([H]\\s*[S]$)|(\\s+\\&)") %>% str_trim())) %>%
  select(
    school_num,
    school,

    district,
    city,
    county,
    region_num,
    grade_grp = graderange,
    grade_grp_num,
    instr_type,
    magnet,
    geometry
  )

# Debugging...
.get_n_chr_max <-
  function(data = NULL, col = NULL) {
    data %>%
      summarise(n = !! enquo(col) %>% str_length() %>% max()) %>%
      pull(n)
  }
schools_tea %>% .get_n_chr_max(school)
schools_sf %>% .get_n_chr_max(school)
schools_tea %>% .get_n_chr_max(county)
schools_sf %>% .get_n_chr_max(county)

n_chr_max_school <- schools_tea %>% .get_n_chr_max(school)

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

df_sf <-
  schools_sf %>%
  filter(grade_grp_num == 4L) %>%
  select(school, county, geometry) %>%
  mutate(school = if_else(
    str_length(school) > n_chr_max_school,
    str_trunc(school, n_chr_max, side = "right", ellipsis = ""),
    school
  ))
df_sf %>% .get_n_chr_max(school)
df_tea <-
  schools_tea %>%
  filter(year == 2015) %>%
  filter(test == "SAT")
z1 <-
  df_sf %>%
  anti_join(df_tea) %>%
  arrange(school)
z2 <-
  df_tea %>%
  anti_join(df_sf) %>%
  arrange(school)
z1
z2
z1 %>% pull(school) %>% str_subset("ALLISON")
z2 %>% pull(school) %>% str_subset("ALLISON")

schools_sf_filt <-
  schools_sf %>%
  filter(grade_grp_num == 4L) %>%
  select(school, county, geometry) %>%
  mutate(school = if_else(
    str_length(school) > n_chr_max_school,
    str_trunc(school, n_chr_max, side = "right", ellipsis = ""),
    school
  )) %>%
  inner_join(
    schools_tea %>%
      filter(year == 2015) %>%
      filter(test == "SAT"),
    by = c("school", "county")
  )

# Back to needed stuff....
counties_raw_sf <-
  # file.path("data-raw", "unsd")  %>%
  file.path("data-raw", "Counties") %>%
    sf::read_sf() %>%
    janitor::clean_names()
# counties_raw_sf %>% select(fename, geometry) %>% plot()
# counties_raw_sf %>% count(fename)
# counties_raw_sf %>% count(campname)
# counties_raw_sf %>% count(name)
# counties_raw_sf %>% pull(cntyfips) %>% as.integer() %>% range()
counties_sf <-
  counties_raw_sf %>%
  mutate(county = fename %>% toupper()) %>%
  select(county, geometry)

counties_bbox <-
  counties_sf %>%
  sf::st_bbox()

# Debugging...

ggplot() +
  geom_sf(data = counties_raw_sf, aes(fill = fid))

# Make a bubble grid! ----
# schools_grid <-
#   schools_sf_filt %>%
#   st_make_grid(n = 100)
# sf:::plot.sfc_POLYGON(schools_grid)
counties_grid <-
  counties_sf %>%
  st_make_grid(n = c(20, 30))
counties_grid

# ggplot method ----
counties_bbox_unnamed <- counties_bbox %>% unname()
xlims <- counties_bbox_unnamed[c(1, 3)]
ylims <- counties_bbox_unnamed[c(2, 4)]
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
    xlim = xlims,
    ylim = ylims,
    datum = NA
  ) +
  # theme_minimal()
  teplot::theme_map()
viz_regions

# NOTE: Don't want this to be a sf.
schools_filt <-
  schools_sf_filt %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  as_tibble()

if(FALSE) {
  schools_tea %>%
    filter(year == 2015) %>%
    filter(test == "SAT")
  schools_tea %>%
    gather(subtest, score, matches("math|reading|writing|english|science|total")) %>%
    drop_na()
  schools_grid <-
    counties_sf %>%
    left_join(
      schools_tea %>%
        filter(year == 2015) %>%
        filter(test == "SAT") %>%
        rename(value = math)
    ) %>%
    # left_join(
    #   schools_tea %>%
    #     gather(subtest, score, matches("math|reading|writing|english|science|total")) %>%
    #     drop_na()
    # ) %>%
    select(value) %>%
    st_interpolate_aw(to = counties_grid, extensive = TRUE) %>%
    st_centroid() %>%
    cbind(st_coordinates(.))
}

data_tx <- teplot::get_map_data_state(state = "tx")
viz_schools_grid <-
  schools_grid %>%
  ggplot() +
  geom_polygon(
    data = data_tx,
    ggplot2::aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA
  ) +
  geom_point(
    aes(x = X, y = Y, size = value, color = value),
    # shape = 21,
    # color = "white",
    # fill = "black",
    show.legend = FALSE
  ) +
  scale_size_area(max_size = 5) +
  scale_color_viridis_c() +
  # scale_fill_viridis_c() +
  # coord_map(
  #   projection = "albers",
  #   # datum = NA,
  #   lat0 = ylims[1],
  #   lat1 = ylims[2]
  # ) +
  # coord_sf(datum = NA) +
  teplot::theme_map() +
  # teplot::theme_te() +
  coord_fixed(1.3)
viz_schools_grid
viz_schools_grid + teplot::
teplot::theme_map
