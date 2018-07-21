

# Neural network for fuzzy joining?
reticulate::py_discover_config("keras")
reticulate::py_discover_config("tensorflow")

# Reference: http://blogs.rstudio.com/tensorflow/posts/2018-01-09-keras-duplicate-questions-quora/
library("keras")

# NOTE: This will get parity blocked.
tokenizer <-
  text_tokenizer(num_words = 10000)
