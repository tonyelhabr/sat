
# Reference: https://github.com/Robinlovelace/Creating-maps-in-R/blob/master/vignettes/geoJSON.Rmd.
library("tidyverse")
url <- "https://opendata.arcgis.com/datasets/059432fd0dcb4a208974c235e837c94f_0.geojson"
path_dest <- file.path("data-raw", "geojson")
downloader::download(url = url, destfile = path_dest)
data_raw <-
  rgdal::readOGR(dsn = path_dest)
data_raw <-
  path_dest %>%
  sf::st_read() %>%
  as_tibble() %>%
  mutate_if(is.factor, funs(as.character)) %>%
  janitor::clean_names()
