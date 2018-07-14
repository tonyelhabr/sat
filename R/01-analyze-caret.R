
schools_tea_all <-
  urls_tea %>%
  create_path_dl_tea() %>%
  tibble(path = .) %>%
  mutate(
    test = stringr::str_extract(path, "([Ss][Aa][Tt])|([Aa][Cc][Tt])") %>% str_to_upper(),
    year = stringr::str_extract(path, "[:digit:]+") %>% as.integer()
  ) %>%
  mutate(contents =
           purrr::map2(path, test,
                       # ~import_tea_data(.x, .y)
                       ~ {
                         .x %>%
                           readr::read_csv() %>%
                           rename_all(funs(tolower)) %>%
                           # filter(group == "All Students") %>%
                           select(
                             matches(
                               "name$|math|reading|writing|total|english|science|compos"
                             )
                           )
                       })) %>%
  unnest() %>%
  select(-path) %>%
  mutate_at(vars(total), funs(ifelse(test == "ACT", compos, .))) %>%
  select(-compos) %>%
  select(-total, everything(), total) %>%
  rename(school = campname,
         district = distname,
         county = cntyname,
         city = regnname) %>%
  mutate_if(is.character, funs(str_replace_all(., "=|\"", ""))) %>%
  mutate_at(vars(school, district, county, city), funs(toupper)) %>%
  # NOTE: Some county names are truncated and end with COUN or COUNT.
  # (The max seems to be 18 characters).
  # Fortunately, ther are no county names with COUN in their names, so the following
  # regular expression is sufficient.
  mutate_at(vars(county), funs(str_remove_all(., "\\s+COUN.*$"))) %>%
  # NOTE: Remove all HS/H S at the end of school names, as well as ampersands.
  # This seems to improve join percentages with other data sets.
  mutate_at(vars(school), funs(str_remove_all(., "([H]\\s*[S]$)|(\\s+\\&)") %>% str_trim())) %>%
  # NOTE: This is (try to) to resolve duplicates in raw data.
  # group_by_at(vars(matches("test|year|school|district|county|city"))) %>%
  # summarise_all(funs(max(., na.rm = TRUE))) %>%
  # ungroup() %>%
  arrange(test, year, school)
