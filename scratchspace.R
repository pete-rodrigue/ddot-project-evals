
setwd("~/GitHub/ddot-project-evals")


trips <- st_read("data/DC_RideReport_MM_Trips.geojson")
st_crs(trips)

source("load_data.R")
source("make_plots.R")

PLOT_BBOX = c(xmin = -77.020, ymin = 38.94, xmax = -77.010, ymax = 38.944)

rs <- load_subblocks()
st_crs(rs)

st_crs(rs) == st_crs(trips)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = st_geometry(rs) %>% st_crop(PLOT_BBOX), color='red',
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666",
                 fillOpacity = 0.7,
                 bringToFront = TRUE
               )) %>%
  addPolylines(data = st_geometry(trips) %>% st_crop(PLOT_BBOX), color='blue',
               highlight = highlightOptions(
                 weight = 5,
                 color = "#666",
                 fillOpacity = 0.7,
                 bringToFront = TRUE
               ))

library(nngeo)

segment_lookup <- rs  |>
  select(SUBBLOCKKEY) |>
  st_transform(6487)  |>
  st_join(
    trips |> st_transform(6487) |> select(OBJECTID),
    join       = st_nn,
    k          = 1,
    maxdist    = 15,
    returnDist = TRUE,
    progress   = TRUE
  ) |>
  st_drop_geometry() |>
  select(SUBBLOCKKEY, OBJECTID)
