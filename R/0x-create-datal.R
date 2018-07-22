
paths_load <-
  list.files(
    "data",
    pattern = "*RData",
    recursive = FALSE,
    full.names = TRUE
  )
sapply(paths_load, load, .GlobalEnv)

# library("tidyverse")
# schools_tea_all <-
#   urls_tea %>%
#   import_tea_data_cleanly()
# schools_tea_all
#
# teproj::export_ext_csv(
#   schools_tea_all,
#   # export = FALSE,
#   dir = "data"
# )

nms <-
  urls_tea %>%
  create_path_tea() %>%
  tibble(path = .) %>%
  mutate(
    test = stringr::str_extract(path, "([Ss][Aa][Tt])|([Aa][Cc][Tt])") %>% toupper(),
    year = stringr::str_extract(path, "[:digit:]+") %>% as.integer()
  ) %>%
  mutate(contents = purrr::map(path, ~read_csv(.x) %>% names())) %>%
  unnest()
nms %>%
  select(-path) %>%
  filter(test == "SAT") %>%
  mutate_at(vars(contents), funs(tolower)) %>%
  filter(!str_detect(contents, "aicode|county$")) %>%
  mutate_at(vars(contents), funs(if_else(str_detect(contents, "critrd"), "reading", .))) %>%
  unite(test_year, test, year, sep = "_") %>%
  mutate(temp = 1L) %>%
  spread(contents, temp, fill = NA) %>%
  visdat::vis_miss()

import_sat_data_cleanly <-
  function(urls = NULL, ...) {
    ret <-
      # urls_tea %>%
      # str_subset("[Ss][Aa][Tt]") %>%
      # create_path_tea() %>%
      urls %>%
      create_path_tea(...) %>%
      tibble(path = .) %>%
      mutate(year = stringr::str_extract(path, "[:digit:]+") %>% as.integer()) %>%
      mutate(contents = purrr::map(path, ~read_csv(.x) %>% rename_all(funs(tolower)))) %>%
      unnest() %>%
      select(-path) %>%
      # NOTE: This is a "new" step. Although only aicode and county
      # are "new" columns for the 2014 and 2015 years, go ahead and drop other
      # numeric columns which may be confused for strings.
      select(-matches("aicode|campus$|district$|county$|region$")) %>%
      # NOTE: This is a "new" step.
      rename_at(vars(matches("_mskd")), funs(str_remove(., "_mskd"))) %>%
      # NOTE: This is a "new" step (to change the name of this column in 2011).
      mutate_at(vars(reading), funs(coalesce(critrd, reading))) %>%
      select(-matches("critrd")) %>%
      select(-total, everything(), total) %>%
      rename(school = campname,
             district = distname,
             county = cntyname,
             city = regnname) %>%
      mutate_if(is.character, funs(str_replace_all(., "=|\"", ""))) %>%
      mutate_at(vars(school, district, county, city), funs(toupper)) %>%
      mutate_at(vars(county), funs(str_remove_all(., "\\s+COUN.*$"))) %>%
      mutate_at(vars(school), funs(str_remove_all(., "([H]\\s*[S]$)|(\\s+\\&)") %>% str_trim())) %>%
      arrange(year, school)
    ret
  }

scores_sat <-
  urls_tea %>%
  str_subset("[Ss][Aa][Tt]") %>%
  import_sat_data_cleanly()
scores_sat

teproj::export_ext_csv(
  scores_sat,
  # export = FALSE,
  dir = "data"
)
