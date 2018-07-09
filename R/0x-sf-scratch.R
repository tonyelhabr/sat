
# file.path("data-raw", "elsd") %>%
#   sf::read_sf()
# file.path("data-raw", "scsd") %>%
#   sf::read_sf()
# file.path("data-raw", "scsd_tl") %>%
#   sf::read_sf()
# file.path("data-raw", "unsd") %>%
#   sf::read_sf()
#
# data_raw_rgdal <-
#   file.path("data-raw", "unsd") %>%
#   rgdal::readOGR(
#     layer =
#       list.files(
#         file.path("data-raw", "unsd"),
#         pattern = "shp$",
#         recursive = FALSE,
#         full.names = FALSE
#       ) %>%
#       tools::file_path_sans_ext()
#   )
