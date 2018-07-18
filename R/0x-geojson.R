
json0 <-
  httr::GET("https://opendata.arcgis.com/datasets/059432fd0dcb4a208974c235e837c94f_0.geojson")
json1$features
json1 <-
  jsonlite::fromJSON("https://opendata.arcgis.com/datasets/059432fd0dcb4a208974c235e837c94f_0.geojson")
json1$features
json1$features$properties
json1$features$geometry

df <-
  bind_cols(
    json1$features$properties,
    json1$features$geometry
  )
pnts <-
  json1$features$geometry[,2] %>% sp::SpatialPoints()
pnts
json2 <-
  geojson::read("https://opendata.arcgis.com/datasets/059432fd0dcb4a208974c235e837c94f_0.geojson")
