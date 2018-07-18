
paths_load <-
  list.files(
    "data",
    pattern = "01.*RData",
    recursive = FALSE,
    full.names = TRUE
  )
sapply(paths_load, load, .GlobalEnv)
library("tidyverse")

schools_tea_all <-
  urls_tea %>%
  import_tea_data_cleanly()
schools_tea_all

rgx_subtest <- "match|reading|writing|english|science|total"
schools_tea_all %>%
  filter(group == "All Students") %>%
  mutate_at(vars(matches(rgx_subtest)), funs(if_else(is.na(.), FALSE, TRUE))) %>%
  group_by_at(vars(matches(paste0("test|", rgx_subtest)))) %>%
  count(sort = TRUE)

schools_tea_all %>%
  filter(group == "All Students") %>%
  count(is.na(total), sort = TRUE)
schools_tea_all %>% drop_na(total)

# Neural network for fuzzy joining?
reticulate::py_discover_config("keras")
reticulate::py_discover_config("tensorflow")

# Reference: http://blogs.rstudio.com/tensorflow/posts/2018-01-09-keras-duplicate-questions-quora/
library("keras")
tokenizer <-
  text_tokenizer(num_words = 10000)

