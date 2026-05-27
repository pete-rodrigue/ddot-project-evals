
setwd("~/GitHub/ddot-project-evals")


source("load_data.R")
source("make_plots.R")
library(nngeo)


# set this to true to actually run this script.
RUN = FALSE

PLOT_BBOX = c(xmin = -77.020, ymin = 38.94, xmax = -77.010, ymax = 38.944)

trips <- st_read("data/DC_RideReport_MM_Trips.geojson") 
st_crs(trips)
rs <- load_subblocks()
st_crs(rs)

if (st_crs(rs) != st_crs(trips)) {
  print("STOP! Check your CRSs")
}

# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addPolylines(data = st_geometry(rs) %>% st_crop(PLOT_BBOX), color='red',
#                highlight = highlightOptions(
#                  weight = 5,
#                  color = "#666",
#                  fillOpacity = 0.7,
#                  bringToFront = TRUE
#                )) %>%
#   addPolylines(data = st_geometry(trips) %>% st_crop(PLOT_BBOX), color='blue',
#                highlight = highlightOptions(
#                  weight = 5,
#                  color = "#666",
#                  fillOpacity = 0.7,
#                  bringToFront = TRUE
#                ))


if (RUN) {
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
  
  
  nn_result <- st_nn(
    rs    |> st_transform(6487) |> select(SUBBLOCKKEY),
    trips |> st_transform(6487) |> select(OBJECTID),
    k          = 1,
    maxdist    = 20,
    returnDist = TRUE,
    progress   = TRUE
  )
  
  # Build lookup table manually
  segment_lookup <- tibble(
    SUBBLOCKKEY = rs$SUBBLOCKKEY,
    OBJECTID    = trips$OBJECTID[sapply(nn_result$nn,   \(x) if (length(x) == 0) NA_integer_ else x)],
    dist        =                 sapply(nn_result$dist, \(x) if (length(x) == 0) NA_real_    else x)
  ) |>
    filter(!is.na(OBJECTID))
}


# write.csv(x = segment_lookup, file = "data/trips_subblocks_matched.csv", row.names = F, )

segment_lookup <- readr::read_csv(file = "data/trips_subblocks_matched.csv", show_col_types = F)


  