
library("tidyverse")
scores_sat <-
  teproj::import_ext_csv(
    scores_sat,
    dir = "data"
  )

scores_sat %>%
  filter(group == "All Students") %>%
  filter(year == 2015) %>%
  count(year, school, district, sort = TRUE) %>%
  filter(n > 1) %>%
  left_join(
    scores_sat %>%
      filter(group == "All Students") %>%
      count(year, school, district, city, sort = TRUE) %>%
      rename(n2 = n)
  )

# NOTE: part_rate = exnees / grads
scores_sat <-
  scores_sat %>%
  unite(school_city, school, city, sep = ", ")

rgx_subtest <- "math|reading|writing|total"
scores_sat_tidy <-
  scores_sat %>%
  mutate_at(vars(grads, exnees, crit), funs(str_remove(., "[^0-9\\.]") %>% as.integer())) %>%
  gather(subtest, score, matches(rgx_subtest)) %>%
  filter(!is.na(score)) %>%
  # filter(subtest != "total") %>%
  mutate_at(vars(group), funs(str_replace_all(., "\\s+", "_") %>%
                                str_replace_all("\\/", "_or_") %>%
                                tolower()))

scores_filt_0 <-
  scores_sat_tidy %>%
  filter(subtest == "math") %>%
  # filter(!is.na(above_crit_rate)) %>%
  select(year, group, school_city, grads, score)

scores_filt
groups <-
  scores_sat_tidy %>%
  tetidy::pull_distinctly(group)
groups
# fmla_bygroup <- formula(score ~ group)
# groups_miss <-
#   groups %>%
#   str_subset("missing")
# groups_miss
# groups_mf <-
#   groups %>%
#   str_subset("ale|Gender")
# groups_mf
# groups_econ <-
#   groups %>%
#   str_subset("[Ee]con")
# groups_econ
groups_dmg <-
  c(
    "african_american",
    "american_indian",
    "asian",
    "hispanic",
    "pacific_islander",
    "white",
    "missing_ethnicity",
    "multiracial"
  )
groups_dmg

scores_filt <-
  scores_filt_0 %>%
  filter(group != "all_students") %>%
  rename(n = grads) %>%
  left_join(
    scores_filt_0 %>%
      filter(group == "all_students") %>%
      select(-group) %>%
      rename(n_all = grads, score_all = score)
  ) %>%
  filter(group %in% groups_dmg) %>%
  mutate(n_frac = n / n_all) %>%
  select(year, group, school_city, n_frac, score_all) %>%
  group_by(year, group, school_city) %>%
  # filter(n_frac == first(n_frac)) %>% # NOTE: This isn't working?
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(year, school_city, score_all) %>%
  nest() %>%
  mutate(missing = purrr::map_dbl(data, ~summarise(.x, temp = 1 - sum(n_frac)) %>% pull(temp))) %>%
  unnest(data) %>%
  spread(group, n_frac, fill = 0)
scores_filt

panels_n4 <-
  scores_filt %>%
  count(school_city, sort = TRUE) %>%
  filter(n == 4)

panels <-
  scores_filt %>%
  semi_join(panels_n4) %>%
  arrange(school_city, year)
panels

fmla_1 <- formula(score_all ~ . - year - school_city)
# fit_1a <-
#   panels %>%
#   filter(year == 2014) %>%
#   lm(fmla_1, data = .)
# fit_1a
#
# fit_1b <-
#   panels %>%
#   filter(year == 2015) %>%
#   lm(fmla_1, data = .)
# fit_1b
#
# stargazer::stargazer(
#   fit_1a,
#   fit_1b,
#   type = "text"
# )
# fmla_plm <- formula(score_all ~ .)
fmla_plm <- formula(score_all ~ african_american + american_indian + asian + hispanic + missing_ethnicity + multiracial + pacific_islander + white + missing)
fit_plm_pool <-
  plm::plm(
    fmla_plm,
    data = panels,
    index = c("school_city", "year"),
    mdoel = "pooling"
  )
fit_plm_pool

fit_plm_fe1 <-
  plm::plm(
    fmla_plm,
    data = panels,
    index = c("school_city", "year"),
    model = "within",
    effect = "individual"
  )
fit_plm_fe1

fit_plm_fe2 <-
  plm::plm(
    fmla_plm,
    data = panels %>% as.data.frame(),
    index = c("school_city", "year"),
    model = "within",
    effect = "twoways"
  )
fit_plm_fe2

stargazer::stargazer(
  fit_plm_pool,
  fit_plm_fe1,
  fit_plm_fe2,
  # se = list(clse(fit_plm_pool), clse(fit_plm_fe1), clse(fit_plm_fe2)),
  # type = "text",
  # title = "Panel Regressions",
  # column.labels = c("Pooled OLS", "School FE", "School-Year FE"),
  df = FALSE,
  digits = 4
)

# data("Star", package  = "Ecdat")
# Star
star <-
  Ecdat::Star %>%
  as_tibble()
star
star %>% rownames()
schooling <-
  Ecdat::Schooling %>%
  as_tibble()
schooling
